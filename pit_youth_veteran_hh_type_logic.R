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
    out <- "Other: non-relation member"
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
ethnicity on the parents in the parenting youth households."

"Parenting youth are youth who identify as the parent or legal guardian of one or more children who
are present with or sleeping in the same place as that youth parent, where there is no person over age
24 in the household. Parenting youth are either a subset of households with at least one adult and one
child if the parenting youth is between 18 and 24, or households with only children if the parenting
youth is under 18. CoCs should report the numbers of children in parenting youth households
separately for households with parenting youth under 18 and households with parenting youth who
are 18 to 24."

def_parenting.youth <- c("is a youth.ALWAYS", 
                         "is a parent or legal guardian of 1 or more kids present in same hh.ALWAYS", 
                         "no person in hh over age of 24.ALWAYS", 
                         "between age of 18 and 24 [households with at least 1 adult and 1 child].1ofA", 
                         "under the age of 18 [households with only children].1ofA")

"Unaccompanied youth are persons under age 25 who are not presenting or sleeping in the same place
as their parent or legal guardian, any household member over age 24, or their own children.
Unaccompanied youth may be a subset of any household type: they are a subset of households
without children if all household members are 18 to 24. They are a subset of households with at least
one adult and one child if the household includes at least one household member under 18, at least
one member between 18 and 24, and no members over age 24. They are a subset of households with
only children if all household members are under 18. "

def_unaccompanied.youth <- c("is under age 25.ALWAYS", 
                             "is not in same household as their parents or legal guardians.ALWAYS", 
                             "if all family members are betwen 18-24 [households without children].1ofA", 
                             "if at least 1 household member <18, at least 1 member 18-24, and no members >24 [households with at least 1 adult and at least 1 child].1ofA", 
                             "if all household members <18 [households with only children].1ofA")

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



get_youth.hh.info <- function(hh_pid.ages.v = c(17,19), 
                               #age.hoh, 
                               relations2hoh.v, 
                               n.veterans){
  out <- NULL
  # Parenting youth - households with at least 1 adult and 1 child----
  if(all(hh_pid.ages.v < 25) & 
     any(hh_pid.ages.v < 18) & 
     any(data.table::between(hh_pid.ages.v, 18, 24)) & 
     length(hh_pid.ages.v) > 1 & 
     any(relations2hoh.v == "Head of household’s Child")){
    out <- c(out, "YOUTH - Parenting youth - households with at least 1 adult and 1 child")
  }
  # Parenting youth - households with only children----
  if(all(hh_pid.ages.v < 18) & 
     length(hh_pid.ages.v) > 1 & 
     any(relations2hoh.v == "Head of household’s Child")){
    out <- c(out, "YOUTH - Parenting youth - households with only children")
  }
  # Unaccompanied youth - households without children----
  if(all(data.table::between(hh_pid.ages.v, 18, 24)) & 
    !any(relations2hoh.v == "Head of household’s Child")){
    out <- c(out, "YOUTH - Unaccompanied youth - households without children")
  }
  # Unaccompanied youth - households with a least 1 adult and 1 child----
  if(all(hh_pid.ages.v < 25) & 
    any(hh_pid.ages.v < 18) & 
    any(data.table::between(hh_pid.ages.v, 18, 24)) & 
    length(hh_pid.ages.v) > 1  & 
    !any(relations2hoh.v == "Head of household’s Child")){
    out <- c(out, "YOUTH - Unaccompanied youth - households with a least 1 adult and 1 child")
  }
  # Unaccompanied youth - households with only children----
  if(all(hh_pid.ages.v < 18) & 
    !any(relations2hoh.v == "Head of household’s Child")){
    out <- c(out, "YOUTH - Unaccompanied youth - households with only children")
  }
  
  # if out is null
  if(is.null(out)){
    out <- "NOT YOUTH"
  }
  
  # veteran
  if(any(vetz,na.rm = T)){
    out <- paste("VETERAN - ", 
                 out, 
                 sep = "", collapse = "")
  }
  
  
  return(out)
}

args(get_youth.hh.info)

# testing----

a.hhid <- sample(enrcli$HouseholdID[!is.na(enrcli$HouseholdID)],size=1) #h_1186802 is a good one to try for multi-person family

ages <- enrcli$age[enrcli$HouseholdID == a.hhid]
rels <- enrcli$rel2hoh[enrcli$HouseholdID == a.hhid]
vetz <- enrcli$vetstatus[enrcli$HouseholdID == a.hhid]

glue("AGES: {paste(ages,sep=\", \",collapse=\", \")}\nRELS: {paste(rels,sep=\", \",collapse=\", \")}\nVETZ: {paste(vetz,sep=\", \",collapse=\", \")}\n\n")

get_youth.hh.info(hh_pid.ages.v    = ages, 
                   relations2hoh.v = rels, 
                   n.veterans      = vets)



