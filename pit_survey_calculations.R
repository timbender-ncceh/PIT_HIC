
c.client %>%
  group_by(RaceNone, AmIndAKNative, 
           Asian, BlackAfAmerican, 
           NativeHIPacific, White) %>%
  summarise(n = n()) %>%
  .[order(.$n, decreasing = T),] %>%
  .[!is.na(.$RaceNone) & 
      .$RaceNone != 99,]

colnames(c.client)

fun_race <- function(racenone=c(8,9,99,NA),
                     amindaknative=c(0,1,99),
                     asian=c(0,1,99),
                     blackafamerican=c(0,1,99),
                     nativehipacific=c(0,1,99),
                     white=c(0,1,99)){
  if(racenone %in% c(8,9)){
    out <- ifelse(racenone == 8, "Client doesn't know", "Client refused")
  }else if((amindaknative + asian + blackafamerican + 
           nativehipacific + white) > 1 ){
    out <- "Multiple Races"
  }else if(amindaknative == 1){
    out <- "American Indian, Alaska Native, or Indigenous"
  }else if(asian == 1){
    out <- "Asian or Asian American"
  }else if(blackafamerican == 1){
    out <- "Black, African American, or African"
  }else if(nativehipacific == 1){
    out <- "Native Hawaiian or Pacific Islander"
  }else if(white == 1){
    out <- "White"
  }else{
    out <- "[unknown]"
  }
}

# fun_ethnicity <- function(ethnicity=c(0,1,8,9,99)){
#   
# }


fun_gender <- function(male = c(0,1),
                       female = c(0,1),
                       nosingle = c(0,1),
                       questioning = c(0,1),
                       trans = c(0,1),
                       gendernone=c(9,99,NA)){
  #out <- NA
  if(trans == 1){
    out <- "Transgender"
  }else if((male == 1 & female == 1)|
           nosingle == 1 ){
    out <- "NoSingleGender"
  }else if(!is.na(gendernone) & gendernone > 1){
    out <- "GenderNone"
  }else if(questioning == 1){
    out <- "Questioning"
  }else if(male == 1){
    out <- "Male"
  }else if(female == 1){
    out <- "Female"
  }else {
    out <- "[unknown]"
  }
  return(out)
}

# all possible permutations found in data: 
# fun_gender(1,0,0,0,0,NA)
# fun_gender(0,1,0,0,0,NA)
# fun_gender(0,0,0,0,0,99)
# fun_gender(0,0,1,0,0,NA)
# fun_gender(0,0,0,0,1,NA)
# fun_gender(0,0,0,1,0,NA)
# fun_gender(0,1,0,0,1,NA)
# fun_gender(0,0,0,0,0,9)
# fun_gender(1,0,0,0,1,NA)
# fun_gender(1,1,0,0,0,NA)


# calculated fields----

# cast down to 1 row per enrolllment with 8 columns (1 per disability)
# sometimes there might be 2+ enrollments on 1 date for 1 person (service + shelter) so we need to look at those and see how to handle them.  

screened_positive_disability <- function(dr0 = c.disabilities$DisabilityResponse,
                                         ii0 = c.disabilities$IndefiniteAndImpairs, 
                                         dt0 = c.disabilities$DisabilityType, 
                                         dis_df = c.disabilities) {
  require(data.table)
  "https://files.hudexchange.info/resources/documents/HMIS-Standard-Reporting-Terminology-Glossary.pdf"
  "Working with Ncceh Data report.docx"
  
    # DisabilityResponse (0,1,2,8,9,99,NA)----
  # 1 = yes
  # 0 = no
  # 2,8,9,99,NA = "unknown or cannot tell"
  
  # dr1 <- ifelse(dr0 %in% c(0,1), dr0, "unknown or cannot tell") %>%
  #   ifelse(. %in% "1", "yes", .) %>%
  #   ifelse(. %in% "0", "no", .)
  
  dr1 <- ifelse(dr0 %in% c(0,1), dr0, NA) %>%
    as.logical()
  
  # IndefiniteAndImpairs (0,1,2,8,9,99,NA)
  # 1 = yes
  # 0 = no
  # 2,8,9,99,NA = "unknown or cannot tell"
  
  ii1 <- ifelse(ii0 %in% c(0,1), ii0, NA) %>%
    as.logical()
  
  # expand.grid(DisabResp = c(T,F,NA), IndefAndImpairs = c(T,F,NA)) %>%
  #   mutate(.,
  #          dr_AND_ii = DisabResp & IndefAndImpairs,
  #          tims_conclusions = c("DISABLED", 
  #                          "[not logically possible]",
  #                          "[not logically possible]", 
  #                          "NOT DISABLED",
  #                          toupper("unknown or cannot tell"), 
  #                          "[not logically possible]", 
  #                          toupper("unknown or cannot tell"),
  #                          toupper("unknown or cannot tell"), 
  #                          toupper("unknown or cannot tell")))
  
  
  # step1: if either DR or II = NA then that record is "unknwon or cannot tell" ---- 
  
  
  expand.grid(DR = c(T,F,NA), 
              II = c(T,F,NA)) %>%
    mutate(., 
           unknown = is.na(DR) | is.na(II), # either is.na() = unknown
           disabled = DR & II,  # both.true = "disabled"
           not_disabled = DR & !II, 
           sum = unknown + disabled + not_disabled) #DR True II False = "not disabled"
  
 
  # (dr1 == T & 
  #   !is.na(ii1)) %>% table(., useNA = "always") # 24,766 true
  
  is.na(dr1) |  is.na(ii1)
  
  # Logic:
  # DisabiltyResponse & IndefiniteAndImpairs 
  
  dis_df$spd <- dr1 & ii1
  
  # DisabilityType----
  
  dis_df <- dis_df %>%
    .[.$spd == T & !is.na(.$spd),] %>%
    left_join(., 
              data.frame(DisabilityType = c(5:10), 
                         DisabilityTypeName = c("physical_disab", 
                                                "developmental_disab", 
                                                "chronic_health_cond", 
                                                "hiv_aids", 
                                                "mental_health_disord", 
                                                "substance_use_disord"))) %>%
    as.data.table() %>%
    dcast(., PersonalID + EnrollmentID ~ DisabilityTypeName, fun.aggregate = length) %>%
    mutate(., 
           t_disabilities = chronic_health_cond + 
             developmental_disab + 
             hiv_aids + 
             mental_health_disord + 
             physical_disab +
             substance_use_disord)
  return(dis_df)
}








calc_age <- function(dob, decimal.month = F, age_on_date = ymd(20230125)){
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

hud_age_category <- function(age_yrs, 
                             breaks_upper = c(17,24,34,44,54,64,126)){
  require(dplyr)
  require(data.table)
  
  bac <- data.frame(lower = 0, upper = breaks_upper[1])
  for(i in 2:length(breaks_upper)){
    bac <- rbind(bac, 
                 data.frame(lower = breaks_upper[i-1]+1, 
                            upper = breaks_upper[i]))
  }
  
  bac <- mutate(bac, 
                name = paste(lower, upper, sep = "-"))
  
  bac$in_cat <- between(x = rep(age_yrs, nrow(bac)), 
                        lower = bac$lower, upper = bac$upper)
  
  out <- bac[bac$in_cat,]$name
  return(out)
}

hud_age_category(41)

coc_region <- function(co_name){
  require(readr)
  cr_crosswalk <- read_tsv("County	Region
Alamance	Region 6
Alexander	Region 3
Anson	Region 7
Beaufort	Region 12
Bertie	Region 12
Bladen	Region 8
Burke	Region 3
Cabarrus	Region 5
Caldwell	Region 3
Camden	Region 11
Carteret	Region 13
Caswell	Region 6
Catawba	Region 3
Chatham	Region 6
Cherokee	Region 1
Chowan	Region 11
Clay	Region 1
Columbus	Region 8
Craven	Region 13
Currituck	Region 11
Dare	Region 11
Davidson	Region 5
Davie	Region 4
Duplin	Region 10
Edgecombe	Region 9
Franklin	Region 9
Gates	Region 11
Graham	Region 1
Granville	Region 9
Greene	Region 10
Halifax	Region 9
Harnett	Region 7
Haywood	Region 1
Henderson	Region 2
Hertford	Region 11
Hoke	Region 7
Hyde	Region 12
Iredell	Region 4
Jackson	Region 1
Johnston	Region 7
Jones	Region 13
Lee	Region 7
Lenoir	Region 10
Macon	Region 1
Madison	Region 1
Martin	Region 12
Mcdowell	Region 3
Montgomery	Region 7
Moore	Region 7
Nash	Region 9
Northampton	Region 9
Onslow	Region 13
Pamlico	Region 13
Pasquotank	Region 11
Perquimans	Region 11
Person	Region 6
Pitt	Region 12
Polk	Region 2
Randolph	Region 7
Richmond	Region 7
Robeson	Region 8
Rockingham	Region 6
Rowan	Region 5
Rutherford	Region 2
Sampson	Region 10
Scotland	Region 8
Stanly	Region 5
Stokes	Region 4
Surry	Region 4
Swain	Region 1
Transylvania	Region 2
Tyrrell	Region 11
Union	Region 5
Vance	Region 9
Warren	Region 9
Washington	Region 12
Wayne	Region 10
Wilson	Region 10
Yadkin	Region 4")
  out <- cr_crosswalk[cr_crosswalk$County == co_name,]$Region
  return(out)
}

coc_region("Yadkin")
