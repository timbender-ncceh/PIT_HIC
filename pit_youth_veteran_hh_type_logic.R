# veteran_type and 
# youth_type households


# Libraries----
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(data.table)
library(devtools)
library(openxlsx)
library(stringr)
library(igraph)
library(ggplot2)

rm(list=ls())
cat('\f')
gc()

# note:  veterans are not alaways hoh so don't encode logic to account for hoh
# necessarily
calc_age <- function(dob, decimal.month = F, age_on_date = ymd(20230125)){
  # correct as of #1/24/23
  require(lubridate)
  require(dplyr)
  
  # check class of dob - expected to be class 'Date'.  If Character, change to
  # 'Date'
  if(!is.Date(dob)){
    if(is.numeric(dob)){
      dob <- as.character(dob)
    }
    dob <- as_date(dob)
  }
  age <- interval(dob,age_on_date) %>%
    as.period(., unit = "years")
  age.year    <- age@year
  age.decimal <- round(age@year + age@month/12,2)
  
  if(decimal.month) {
    out <- age.decimal
  }else{
    out <- age.year
  }
  return(out)
}
fun_rel2hoh <- function(x){
  if(is.na(x)){
    out <- NA#"Data not collected"
  }else if(x == 2){
    out <- "Head of household’s Child"
  }else if(x == 3){
    out <- "Head of household’s spouse or partner"
  }else if(x == 4){
    out <- "Head of household’s other relation member"
  }else if(x == 5){
    out <- "Other: non-relation member "
  }else if(x == 99){
    out <- NA#"Data not collected"
  }else if(x == 1){
    out <- "Self (head of household)"
  }else{
    out <- NA#"[undetermined]"
  }
  return(out)
}
fun_1.8_def <- function(x){
  if(is.na(x)){
    out <- NA#"Data not collected"
  }else if(x == 1){
    out <- "Yes"
  }else if(x == 8){
    out <- "Client doesn't Know"
  }else if(x == 9){
    out <- "Client refused"
  }else if(x == 99){
    out <- NA #"Data not collected"
  }else if(x == 0){
    out <- "No"
  }else{
    out <- NA#"[undetermined]"
  }
  return(out)
}

# veteran definition----
# source: https://www.hud.gov/sites/dfiles/CPD/documents/2022_HIC_and_PIT_Data_Collection_Notice.pdf
"CoCs must also collect and report data on veterans, including the total number of veteran households,
the total number of veterans, the total number of persons in veteran households, and the gender, race,
and ethnicity of veterans. A “veteran household” includes households with one or more veterans who 
might be presenting with other persons. Please note that data for the gender, race, and ethnicity of
non-veterans in veteran households will only be reported under “All Households” population data in
Appendix C. CoCs should not include veterans in VADOM or VA-funded CWT/TR facilities in
their PIT count."

vet.info_return <- c("veteran_hh", "t_veterans_in_hh", "t_hh_members")

# youth definition----
# source: https://www.hud.gov/sites/dfiles/CPD/documents/2022_HIC_and_PIT_Data_Collection_Notice.pdf
"CoCs must report data on persons in Youth Households, including the gender, race, and ethnicity for
parenting youth and unaccompanied youth, as outlined in Appendix C. However, while gender, race,
and ethnicity are reported for all unaccompanied youth, CoCs will only report the gender, race, and
ethnicity on the parents in the parenting youth households.

Parenting youth are youth who identify as the parent or legal guardian of one or more children who
are present with or sleeping in the same place as that youth parent, where there is no person over age
24 in the household. Parenting youth are either a subset of households with at least one adult and one
child if the parenting youth is between 18 and 24, or households with only children if the parenting
youth is under 18. CoCs should report the numbers of children in parenting youth households
separately for households with parenting youth under 18 and households with parenting youth who
are 18 to 24.

Unaccompanied youth are persons under age 25 who are not presenting or sleeping in the same place
as their parent or legal guardian, any household member over age 24, or their own children.
Unaccompanied youth may be a subset of any household type: they are a subset of households
without children if all household members are 18 to 24. They are a subset of households with at least
one adult and one child if the household includes at least one household member under 18, at least
one member between 18 and 24, and no members over age 24. They are a subset of households with
only children if all household members are under 18. "

youth.info.return <- c("youth_hh", "parenting_youth or unaccompanied_youth", "age_cohort")

# files----
a.client     <- read_csv("C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data/Client.csv")
a.enrollment <- read_csv("C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data/Enrollment.csv")

a.enrollment$rel2hoh <- lapply(X = a.enrollment$RelationshipToHoH, 
                               FUN = fun_rel2hoh) %>% unlist()

a.enrollment <- a.enrollment[,c("EnrollmentID", "PersonalID", "HouseholdID", "ProjectID",
                                "rel2hoh")]
a.client           <- a.client[,c("PersonalID", "DOB", "VeteranStatus")] 
a.client$vetstatus <- unlist(lapply(a.client$VeteranStatus, fun_1.8_def))
a.client$age       <- unlist(lapply(X = a.client$DOB, 
                              FUN = calc_age))

enrcli             <- full_join(a.client, a.enrollment) %>%
                       .[! colnames(.) %in% c("DOB", "VeteranStatus")]

enrcli %>%
  group_by(HouseholdID) %>%
  summarise(n_eid = n_distinct(EnrollmentID), 
            n_pid = n_distinct(PersonalID),
            n_projid = n_distinct(ProjectID),
            n = n()) %>%
  #.[.$n_pid != .$n_eid,] %>%
  ggplot(data = ., 
         aes(x = n_eid, y = n_pid)) + 
  geom_jitter() +
    scale_x_continuous(breaks = seq(0,1000,by=1), 
                       minor_breaks = seq(0,1000,by=1))+
    scale_y_continuous(breaks = seq(0,1000,by=1), 
                       minor_breaks = seq(0,1000,by=1))


# funs----
hh_pid_ages.v <- c(21)
age.hoh <- 21

get_youth.hh.info <- function(hh_pid_ages.v, age.hoh = NULL, 
                              n.veterans,
                              print.inputs = F){
  require(dplyr)
  require(data.table)
  require(glue)
  require(crayon)
  # tidy
  # hh_pid_ages.v <- hh_pid_ages.v %>%
  #   .[order(.)]
  
  # error checking---- 
  
  # it's ok for age_of_hoh to be null if the HH size is one;
  # you can imply it.  otherwise it's required.
  if((is.null(age.hoh) & length(hh_pid_ages.v) > 1)){
    stop("age of Head of Household (age.hoh) must be provided when household size is greater than 1")
  }
  
  # calculate age.hoh
  if(is.null(age.hoh)){
    age.hoh <- hh_pid_ages.v
  }
  
  # possible flags----
  poss_flags <- NULL
  # age of hoh < max.age
  if(age.hoh < max(hh_pid_ages.v)){
    poss_flags <- c(poss_flags, "HoH younger than oldest household member")
  }
  # veterans present when max.age < 22
  if(n.veterans > 0 & max(hh_pid_ages.v) < 22){
    poss_flags <- c(poss_flags, 
                    "Veteran(s) present when max(age) < 22")
  }
  # more veterans than hh memebers
  if(n.veterans > length(hh_pid_ages.v)){
    poss_flags <- c(poss_flags, 
                    "More veterans than hh members")
  }
  
  # need to return
  is_youth_hh      <- c(T,F)
  is_veteran_hh    <- c(T,F)
  youth_hh_type    <- c("Households without children (18+)",  # children =/= youth
                        "Households with at least 1 adult and 1 child (<18 & 18+ in one hh)",
                        "Households with only children (<18)")
  youth_hh_subtype <- c("Unaccompanied youth (18-24)",        # children =/= youth
                        "Parenting youth (18-24)",
                        "Children of parenting youth",
                        "Unaccompanied youth (<18 & 18-24 in one hh)",
                        "Parenting youth (<18)",
                        "Unaccompanied youth (<18)", 
                        "Other people (>=25)", 
                        "Other people (<18)", 
                        "Other people (18-24)")
  is_veteran_hh    <- NA
  is_youth_hh      <- NA
  youth_hh_subtype <- NA
  youth_hh_type    <- NA
  
  # is_youth_hh
  if(all(hh_pid_ages.v <= 24)) { # this number at 24 is correct for identifying the hh type
    is_youth_hh <- T
  }else{
    is_youth_hh <- F
  }
  
  # is_veteran_hh
  if(n.veterans > 0){
    is_veteran_hh <- T
    n_veterans   <- n.veterans
  }else{
    is_veteran_hh <- F
    n_veterans    <- 0
  }
  
  
  # hh_type
  if(length(hh_pid_ages.v) > 1){
    # could be hh with at least 1 adult and 1 child, or [min(age) < 18 & max(age) >= 18 ]
    if(min(hh_pid_ages.v) < 18 & 
       max(hh_pid_ages.v) >= 18){
      youth_hh_type <- "Households with at least 1 adult and 1 child"
    }
    # hh without children, or [all ages >= 18] 
    if(all(hh_pid_ages.v >= 18)) {
      youth_hh_type <- "Households without children"
    }
    # hh with only children [all ages < 18 or]
    if(all(hh_pid_ages.v < 18)){
      youth_hh_type <- "Households with only children"
    }
  }else{
    # cannot be hh with at least 1 adult and 1 child
    if(min(hh_pid_ages.v) < 18 & 
       max(hh_pid_ages.v) >= 18){
      youth_hh_type <- "<ERROR>"
    }
    # either hh without children or [all ages >= 18]
    if(all(hh_pid_ages.v >= 18)) {
      youth_hh_type <- "Households without children"
    }
    # hh with only children [all ages < 18]
    if(all(hh_pid_ages.v < 18)){
      youth_hh_type <- "Households with only children"
    }
  }
  
  # HH Age Group
  
  # step 1: age grouping 
  # (18-24)
  if(all(between(hh_pid_ages.v, 18, 24))){
    ag18.24 <- T
  }else{
    ag18.24 <- F
  }
  # (<18 & 18-24 in one hh)
  if(any(hh_pid_ages.v < 18) & 
     any(between(hh_pid_ages.v, 18, 24)) & 
     all(hh_pid_ages.v <= 24)){
    ag18and18.24 <- T
  }else{
    ag18and18.24 <- F
  }
  # (<18) 
  if(all(hh_pid_ages.v < 18)){
    agU18 <- T
  }else{
    agU18 <- F
  }
  # (>=25)
  if(any(hh_pid_ages.v > 24)){
    ag25O <- T
  }else{
    ag25O <- F
  }
  
  out.ages <- data.frame(variable = c("(18-24)", 
                                      "(<18 & 18-24 in one hh)", 
                                      "(<18)", 
                                      "(>=25)"), 
                         value    = c(ag18.24, 
                                      ag18and18.24, 
                                      agU18, 
                                      ag25O)) %>% 
    as_tibble()
  
  # # HH subtype----
  # c("Unaccompanied youth (18-24)",
  #   "Parenting youth (18-24)",
  #   "Children of parenting youth",
  #   "Unaccompanied youth (<18 & 18-24 in one hh)",
  #   "Parenting youth (<18)",
  #   "Unaccompanied youth (<18)", 
  #   "Other people (>=25)", 
  #   "Other people (<18)", 
  #   "Other people (18-24)")
  
  temp.out.age <- out.ages$variable[out.ages$value]
 
  # error check----
  if(length(temp.out.age) > 1){
    stop("ERROR: var 'temp.out.age' should be length of 1")
  }
  # unaccompanied youth
  # if is_youth household
  if(is_youth_hh){
    if(youth_hh_type == "Households with at least 1 adult and 1 child"){
      youth_hh_subtype <- c("Parenting youth", "Children of parenting youth")
    }else if(youth_hh_type %in% c("Households with only children", 
                                  "Households without children")){
      youth_hh_subtype <- "Unaccompanied youth"
    }else{
      youth_hh_subtype <- NA # this needs to eventually go away.  it's a catch-all for missing logic
    }
    
    
    
  }else{
    # it's not is_youth and thus, is the "other people"
    if(temp.out.age %in%  c("(<18)", 
                            "(18-24)", 
                            "(<18 & 18-24 in one hh)")){
      youth_hh_subtype <- "<ERROR - not possible for house to be not_youth_hh but have all people under 25>"
    }else if(temp.out.age == ("(>=25)")){
      youth_hh_subtype <- "Other people (>=25)"
    }else{
      youth_hh_subtype <- "<ERROR - this isn't possible - search for 's3a9qm' in code"
    }
    
  }
  
  
  # generate output
  out <- list(hh.input.summary = data.frame(hh_size     = length(hh_pid_ages.v), 
                                            hoh_age     = age.hoh, 
                                            min_age     = min(hh_pid_ages.v), 
                                            max_age     = max(hh_pid_ages.v), 
                                            n_veteranss = n_veterans),
              is.youth.hh      = is_youth_hh, 
              youth.hh.type    = youth_hh_type, 
              youth.hh.subtype = youth_hh_subtype, 
              youth.hh.agetype = out.ages$variable[out.ages$value],
              is.veteran.hh    = is_veteran_hh, 
              n.veterans       = n_veterans,
              poss.flags       = poss_flags)
  
  # print inputs to help with debugging
  if(print.inputs){
    cat(inverse(glue("AGES:\t{paste(hh_pid_ages.v,sep=\" \", collapse = \" \")}\nAGEHoH:\t{age.hoh}\n\n")))
  }
  return(out)
}

args(get_youth.hh.info)
get_youth.hh.info(c(1,17,19), 17, 
                  n.veterans = 0)


# testing----

# generate random household of ages and size
for(i in 1:100){
  
  some.ages <- sample(1:45, size = sample(1:3, size = 1), replace = T)
  
  if(sum(some.ages>17) > 1){
    hohage <- sample(some.ages[some.ages > 17], size = 1)
  }else{
    hohage <- max(some.ages)
  }
  
  # if(length(some.ages) > 1){
  #   hohage <- max(some.ages)
  # }else{
  #   hohage <- NULL
  # }
  
  # decide (randomly, but with some logic) whether there is a parent-child
  # relationship in household
  #is.parentchild <- ifelse(length(some.ages) > 1 & any(some.ages > 17) & any(some.ages <= 17), sample(c(T,F), size = 1), F)
  
  # run script
  test.out <- get_youth.hh.info(hh_pid_ages.v = some.ages, 
                                age.hoh = hohage)
  
  if(is.na(test.out$youth.hh.type) | any(is.na(test.out$youth.hh.subtype))){
    print(i);get_youth.hh.info(hh_pid_ages.v = some.ages, 
                               age.hoh =  hohage)
    break
  }
}

get_youth.hh.info(some.ages, hohage)

get_youth.hh.info(c(28,38,38), 28)



# scenario testing ----

age.scenarios <- expand.grid(pid1 = c(1,4, 15:25,NA), 
                             pid2 = c(4, 15:25, 32, 42,NA), 
                             pid3 = c(15:25, 27, 31)) %>%
  as_tibble()

age.scenarios$n_pid <- ifelse(is.na(age.scenarios$pid1), 0, 1) +
  ifelse(is.na(age.scenarios$pid2), 0, 1) +
  ifelse(is.na(age.scenarios$pid3), 0, 1)

age.scenarios$hoh_age <- NA
for(i in 1:nrow(age.scenarios)){
  age.scenarios$hoh_age[i] <- max(c(age.scenarios$pid1[i],age.scenarios$pid2[i],age.scenarios$pid3[i]), na.rm = T)
}

age.scenarios <- mutate(age.scenarios, 
                        is_youth_hh = NA, 
                        is_vet_hh   = NA, 
                        hh_type     = NA, 
                        hh_subtype  = NA,
                        hh_agetype  = NA,
                        poss_flags  = NA)

age.scenarios <- age.scenarios[age.scenarios$n_pid > 0,]

for(i in 1:nrow(age.scenarios)) {
  
  temp.ages <-  c(age.scenarios$pid1[i],age.scenarios$pid2[i],age.scenarios$pid3[i]) %>%
    .[!is.na(.)]
  
  temp.funout <- get_youth.hh.info(hh_pid_ages.v = temp.ages, 
                                   age.hoh       = age.scenarios$hoh_age[i], 
                                   n.veterans    = ifelse(any(temp.ages > 20),sample(c(T,F), size = 1, prob = c(0.3,0.7)),0))
  rm(temp.ages)
  
  if(length(temp.funout$youth.hh.subtype) > 1 & 
     paste(temp.funout$youth.hh.subtype, sep = " ", collapse = " ") != 
     "Parenting youth Children of parenting youth"){
    print(i)
    stop("length(subtype) > 1")
  }
  if(length(temp.funout$poss.flags) > 1){
    print(i)
    stop("length(poss.flags) > 1")
  }
  
  
  age.scenarios$is_youth_hh[i] <- temp.funout$is.youth.hh
  age.scenarios$is_vet_hh[i]   <- temp.funout$is.veteran.hh
  age.scenarios$hh_type[i]     <- temp.funout$youth.hh.type
  age.scenarios$hh_subtype[i]  <- paste(temp.funout$youth.hh.subtype, 
                                        sep = ", ", collapse = ", ")
  age.scenarios$hh_agetype[i]  <- temp.funout$youth.hh.agetype
  age.scenarios$poss_flags[i]  <- paste(temp.funout$poss.flags, 
                                        sep = ", ", collapse = ", ")
  
  rm(temp.funout)
}

library(data.table)
age.scenarios %>%
  as.data.table() %>%
  melt(., 
       measure.vars = c("pid1", "pid2", "pid3"), 
       variable.name = "which_person", 
       value.name = "age") %>%
  as.data.frame() %>%
  as_tibble() %>%
  .[!is.na(.$age),] %>%
  group_by(is_youth_hh, 
           is_vet_hh,
           hh_agetype,
           hh_type, 
           hh_size = ifelse(n_pid > 1, "2+", as.character(n_pid)),
           hh_subtype) %>%
  summarise(min_age = min(age), 
            max_age = max(age), 
            n = n())

age.scenarios2 <- age.scenarios %>%
  as.data.table() %>%
  melt(., 
       measure.vars = c("pid1", "pid2", "pid3"), 
       variable.name = "which_person", 
       value.name = "age") %>%
  as.data.frame() %>%
  as_tibble()%>%
  .[!is.na(.$age),]

ggplot(data = age.scenarios2, 
       aes(x = hh_subtype, y = age, group = hh_subtype, 
           fill = is_youth_hh, color = hh_agetype)) + 
  geom_boxplot() +
  facet_wrap(~hh_type)+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1))+
  scale_y_continuous(breaks = c(0,1,17,18,24,25), minor_breaks = NULL,
                     limits = c(0,NA))

colnames(age.scenarios2)
