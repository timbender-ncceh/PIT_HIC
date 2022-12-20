

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
  return(out)
}



fun_gender <- function(male = c(0,1),
                       female = c(0,1),
                       nosingle = c(0,1),
                       questioning = c(0,1),
                       trans = c(0,1),
                       gendernone=c(9,99,NA)){
  
  if(trans == 1 & !is.na(trans)){
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



screened_positive_disability <- function(dis_df = c.disabilities, 
                                         enr_df = c.enrollment, 
                                         exit_df = c.exit, 
                                         pit_date = ymd(20220126)){
  
  
  # final score:----
  dis_df$is_disab <- "unknown or cannot tell"
  
  dis_df[dis_df$DisabilityResponse > 1 | 
           is.na(dis_df$DisabilityResponse),]$is_disab <- "unknown or cannot tell"
  dis_df[dis_df$DisabilityResponse == 0 & 
           !is.na(dis_df$DisabilityResponse),]$is_disab <- "not disabled"
  dis_df[dis_df$DisabilityResponse == 1 & 
           !is.na(dis_df$DisabilityResponse) & 
           dis_df$IndefiniteAndImpairs == 1 & 
           !is.na(dis_df$IndefiniteAndImpairs),]$is_disab <- "disabled"
  
  dis_df[dis_df$DisabilityResponse == 1 & 
           !is.na(dis_df$DisabilityResponse) & 
           dis_df$IndefiniteAndImpairs == 0 & 
           !is.na(dis_df$IndefiniteAndImpairs),]$is_disab <- "not disabled"
  
  #dis_df[is.na(dis_df$is_disab),]$is_disab <- "unknown or cannot tell"
  
  
  #To Do----
  
  # add the non-disabled clients back into the output table
  
  # there is a problem in the logic of the output table - there should not be
  # multiple of any one disability type per each enrollment_ID
  
  "https://files.hudexchange.info/resources/documents/HMIS-Standard-Reporting-Terminology-Glossary.pdf"
  "Working with Ncceh Data report.docx"
  
  dis_df %>%
    group_by(is_disab) %>%
    summarise(count_disabilities = n()) 
  
  # identify most recent informationDate for each client-enrollment----
  
  join_dates <- hmis_join(dis_df,  
                          enr_df[colnames(enr_df) %in% c("EnrollmentID", "HouseholdID", "PersonalID", "ProjectID", 
                                                         #"DisablingCondition", "RelationshipToHoH", 
                                                         "EntryDate")], 
                          jtype = "left") %>%
    hmis_join(., 
              exit_df, jtype = "left") 
  
  
  # join_dates$enr_during_surv <- NA
  # join_dates$enr_during_surv[(join_dates$EntryDate > pit_date | 
  #   join_dates$ExitDate < pit_date) | !is.na(join_dates$EntryDate > pit_date | 
  #                                              join_dates$ExitDate < pit_date)] <- F
  # 
  # 
  # table(join_dates$enr_during_surv, useNA = "always")
  # join_dates[is.na(join_dates$enr_during_surv),]
  # 
  # table(join_dates$EntryDate > pit_date, useNA = "always")
  # table(join_dates$EntryDate == pit_date, useNA = "always")
  # table(join_dates$EntryDate < pit_date, useNA = "always")
  
  # filter out all information dates that occur after the PIT survey date and
  # then find the latest InformationDate for each enrollment - that becomes
  # your most recent and thus most applicable date for disability inventory
  
  join_dates <- join_dates %>%
    .[.$InformationDate_disab <= pit_date,] %>%
    group_by(PersonalID, EnrollmentID) %>%
    slice_max(., 
              order_by = InformationDate_disab, 
              n = 1)
  
  
  
  
  #  library(ggplot2)
  #  
  #  plot.this <- mutate(join_dates[sample(1:nrow(join_dates), size = 200, replace = F),], 
  #                      rid = 1:length(EntryDate), 
  #                      color = "normal")
  #  
  #  plot.this[is.na(plot.this$ExitDate),]$color <- "NA - exit"
  #  
  # ggplot() + 
  #   geom_segment(data = plot.this, 
  #                aes(x = EntryDate, xend = ExitDate, 
  #                    y = rid, yend = rid, 
  #                    color = color))+
  #   geom_point(data = plot.this[is.na(plot.this$ExitDate),], 
  #              aes(x = EntryDate, y = rid, color = color), 
  #              size = 0.8) + 
  #   geom_vline(aes(xintercept = Sys.Date()), 
  #              color = "blue")+
  #   geom_vline(aes(xintercept = pit_date), 
  #              linetype = 2232, color = "red")
  
  
  jd <- join_dates %>%
    left_join(., 
              data.frame(DisabilityType = 5:10, 
                         dt_name = c("physical_D", 
                                     "developmental_D", 
                                     "chronic_hlth_C", 
                                     "HIV.AIDS", 
                                     "mental_health_D", 
                                     "substance_use_D"))) %>%
    as.data.table() %>%
    dcast(., 
          PersonalID + EnrollmentID + #is_disab +
            InformationDate_disab ~ dt_name, 
          #fun.aggregate = length
          value.var = "is_disab") 
  
  # There are NA values in this table and it's not clear why----- VVVV
  
  jd[jd$developmental_D == T & jd$chronic_hlth_C == T,]
  
  jd %>%
    group_by(developmental_D, HIV.AIDS, chronic_hlth_C, 
             mental_health_D, physical_D, substance_use_D) %>%
    summarise(n = n()) %>%
    .[order(.$n,decreasing = T),]
  
  return(jd)
  
}


# screened_positive_disability <- function(dr0 = c.disabilities$DisabilityResponse,
#                                          ii0 = c.disabilities$IndefiniteAndImpairs, 
#                                          dt0 = c.disabilities$DisabilityType, 
#                                          dis_df = c.disabilities) {
#   require(data.table)
#   
#   # To Do----
#   
#   # add the non-disabled clients back into the output table
#   
#   # there is a problem in the logic of the output table - there should not be
#   # multiple of any one disability type per each enrollment_ID
#   
#   "https://files.hudexchange.info/resources/documents/HMIS-Standard-Reporting-Terminology-Glossary.pdf"
#   "Working with Ncceh Data report.docx"
#   
#     # DisabilityResponse (0,1,2,3,8,9,99,NA)----
#   # 1 = yes
#   # 0 = no
#   # 2,3,8,9,99,NA = "unknown or cannot tell"
#   
#   dis_df$DisabilityResponse
#   
#   dr1 <- ifelse(dr0 %in% c(0,1), dr0, NA) %>%
#     as.logical()
#   
#   # IndefiniteAndImpairs (0,1,2,8,9,99,NA)
#   # 1 = yes
#   # 0 = no
#   # 2,8,9,99,NA = "unknown or cannot tell"
#   
#   ii1 <- ifelse(ii0 %in% c(0,1), ii0, NA) %>%
#     as.logical()
#   
#    
#   # # step1: if either DR or II = NA then that record is "unknwon or cannot tell" ---- 
#   
#   # Logic:
#   # DisabiltyResponse & IndefiniteAndImpairs 
#   
#   
#   is_disab <- dr1 & ii1
#   is_disab <- ifelse(is_disab, "Disabled", 
#          "Not Disabled") 
#   
#   is_disab <- ifelse(is.na(is_disab), 
#                      "Unknown or Cannot Tell", 
#                      is_disab)
#   
#  
#   
#   dis_df$spd <- dr1 & ii1
#   dis_df$disab_status_calc <- is_disab
#   
#   dis_df$disab_count <- ifelse(dis_df$disab_status_calc == "Disabled", 1, 0)
#   
#   
#   
#   
#   # DisabilityType----
#   
#   # read_csv("Disabilities.csv") %>%
#   #   #c.disabilities %>%
#   #   .[.$PersonalID %in% 713 & 
#   #       .$EnrollmentID %in% 1155687 & 
#   #       .$DisabilityType == 7,] %>%
#   #   group_by(PersonalID,EnrollmentID,DisabilitiesID, 
#   #            InformationDate) %>%
#   #   summarise(n = n())
#   # 
#   # 
#   # read_csv("Disabilities.csv") %>%
#   # #c.disabilities %>%
#   #   .[.$PersonalID %in% 713 & 
#   #       .$EnrollmentID %in% 1155687 & 
#   #       .$DisabilityType == 7,] %>%
#   #   group_by(PersonalID, EnrollmentID, 
#   #            InformationDate,
#   #            DataCollectionStage,
#   #            DisabilityType,
#   #            DisabilityResponse, IndefiniteAndImpairs) %>%
#   #   summarise()
#   
#   library(ggplot2)
#   
#   # x <- read_csv("Disabilities.csv") %>%
#   #   #c.disabilities %>%
#   #   .[.$PersonalID %in% 713 & 
#   #       .$EnrollmentID %in% 1155687,]
#   
#   x <- c.disabilities
#   
#   x <- x[,!grepl(pattern = "^TCell|^Viral|^Anti|^UserID$|^ExportID$|^Date", x = colnames(x))] 
#   x %>%
#     group_by(PersonalID, 
#              EnrollmentID, 
#              #InformationDate, 
#              DataCollectionStage, 
#              DisabilityType, 
#              DisabilityResponse, 
#              IndefiniteAndImpairs) %>%
#     summarise() %>%
#     mutate(., 
#            dr_plus_ii = paste(DisabilityResponse, IndefiniteAndImpairs, sep = "-")) %>%
#     as.data.table() %>%
#     dcast(., 
#           PersonalID + EnrollmentID + DisabilityType ~ DataCollectionStage, value.var = "dr_plus_ii")
#   
#   
#   
#   dis_df <- dis_df %>%
#     #.[.$spd == T & !is.na(.$spd),] %>%
#     left_join(., 
#               data.frame(DisabilityType = c(5:10), 
#                          DisabilityTypeName = c("physical_disab", 
#                                                 "developmental_disab", 
#                                                 "chronic_health_cond", 
#                                                 "hiv_aids", 
#                                                 "mental_health_disord", 
#                                                 "substance_use_disord"))) %>%
#     as.data.table() %>%
#     dcast(., 
#           PersonalID + EnrollmentID + disab_status_calc ~ DisabilityTypeName, 
#           fun.aggregate = sum, 
#           value.var = "disab_count")
#     #dcast(., PersonalID + EnrollmentID ~ DisabilityTypeName, fun.aggregate = length) %>%
#     mutate(., 
#            total_disabilities = chronic_health_cond + 
#              developmental_disab + 
#              hiv_aids + 
#              mental_health_disord + 
#              physical_disab +
#              substance_use_disord) %>% 
#     as.data.frame()
#   return(dis_df)
# }

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
  
  if(is.na(age_yrs) | age_yrs > max(breaks_upper)){
    out <- NA
  }else{
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
  }
  return(out)
}

hud_age_category(18)

get_coc_region <- function(){
  require(readr)
  return(read_tsv("County	Region
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
Yadkin	Region 4"))
}


# coc_region <- function(co_name){
#   require(readr)
#   cr_crosswalk <- read_tsv("County	Region
# Alamance	Region 6
# Alexander	Region 3
# Anson	Region 7
# Beaufort	Region 12
# Bertie	Region 12
# Bladen	Region 8
# Burke	Region 3
# Cabarrus	Region 5
# Caldwell	Region 3
# Camden	Region 11
# Carteret	Region 13
# Caswell	Region 6
# Catawba	Region 3
# Chatham	Region 6
# Cherokee	Region 1
# Chowan	Region 11
# Clay	Region 1
# Columbus	Region 8
# Craven	Region 13
# Currituck	Region 11
# Dare	Region 11
# Davidson	Region 5
# Davie	Region 4
# Duplin	Region 10
# Edgecombe	Region 9
# Franklin	Region 9
# Gates	Region 11
# Graham	Region 1
# Granville	Region 9
# Greene	Region 10
# Halifax	Region 9
# Harnett	Region 7
# Haywood	Region 1
# Henderson	Region 2
# Hertford	Region 11
# Hoke	Region 7
# Hyde	Region 12
# Iredell	Region 4
# Jackson	Region 1
# Johnston	Region 7
# Jones	Region 13
# Lee	Region 7
# Lenoir	Region 10
# Macon	Region 1
# Madison	Region 1
# Martin	Region 12
# Mcdowell	Region 3
# Montgomery	Region 7
# Moore	Region 7
# Nash	Region 9
# Northampton	Region 9
# Onslow	Region 13
# Pamlico	Region 13
# Pasquotank	Region 11
# Perquimans	Region 11
# Person	Region 6
# Pitt	Region 12
# Polk	Region 2
# Randolph	Region 7
# Richmond	Region 7
# Robeson	Region 8
# Rockingham	Region 6
# Rowan	Region 5
# Rutherford	Region 2
# Sampson	Region 10
# Scotland	Region 8
# Stanly	Region 5
# Stokes	Region 4
# Surry	Region 4
# Swain	Region 1
# Transylvania	Region 2
# Tyrrell	Region 11
# Union	Region 5
# Vance	Region 9
# Warren	Region 9
# Washington	Region 12
# Wayne	Region 10
# Wilson	Region 10
# Yadkin	Region 4")
#   out <- cr_crosswalk[cr_crosswalk$County == co_name,]$Region
#   return(out)
# }

#coc_region("Yadkin")
