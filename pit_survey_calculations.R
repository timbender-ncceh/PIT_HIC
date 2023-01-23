
# formulas developed in "nccounty_logic.R"
get.calc_location_county <- function(housingtype, proj.address.county, 
                                     nccounty){
  return(ifelse(test = housingtype %in% c(1,2) & !is.na(housingtype), 
                yes  = proj.address.county, 
                no   = nccounty))
}

get.proj_county <- function(proj_zip = c(27704, 27626, 27829, 45036), 
                            proj_city = c("Durham", "Raleigh", "Fountain", "Lebanon")){
  require(readr)
  zip_cw <- read_tsv(file = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/zip_county_crosswalk.txt")
  out <- NULL
  for(i in 1:length(proj_zip)){
    if(proj_zip[i] %in% zip_cw$ZIP & 
       proj_city[i] %in% zip_cw$City){
      out <- c(out, 
               zip_cw[zip_cw$ZIP %in% proj_zip[i] & 
                        zip_cw$City %in% proj_city[i],]$County)
    }else{
      out <- c(out, NA)
    }
  }
  return(out)
}

get.calc_region <- function(calc_location_county){
  require(dplyr)
  
  # case_correct
  clc <- tolower(calc_location_county)
  clc <- paste(toupper(substr(clc,1,1)), substr(clc,2,nchar(clc)), 
               sep = "", collapse = "")
  
  try(calc_location_county <- clc)
  
  # 
  # region_cw <- read_csv("regionscrosswalk.csv")
  # region_cw$Region %>% unique %>% .[order(.)]
  # 
  #  
  #   
  # cat('\f');paste(region_cw$County[region_cw$Region == "Region 13"], 
  #       sep = " ", collapse = " ") %>%
  #   gsub(" ", "\",\"", .) %>%
  #   paste("c(\"", ., "\")", sep = "") %>% 
  #   gsub(",", ", ", .) %>% 
  #   cat()
  
  # Region 13
  if(calc_location_county %in%
     c("Carteret", "Craven", "Jones", "Onslow", "Pamlico") ){
    out <- "Region 13"
  }
  # Region 12
  if(calc_location_county %in%
     c("Beaufort", "Bertie", "Hyde", "Martin", "Pitt", "Washington") ){
    out <- "Region 12"
  }
  # Region 11
  if(calc_location_county %in%
     c("Camden", "Chowan", "Currituck", "Dare", "Gates", "Hertford", "Pasquotank", "Perquimans", "Tyrrell") ){
    out <- "Region 11"
  }
  # Region 10
  if(calc_location_county %in%
     c("Duplin", "Greene", "Lenoir", "Sampson", "Wayne", "Wilson") ){
    out <- "Region 10"
  }
  # Region 9
  if(calc_location_county %in%
     c("Edgecombe", "Franklin", "Granville", "Halifax", "Nash", "Northampton", "Vance", "Warren") ){
    out <- "Region 9"
  }
  # Region 8
  if(calc_location_county %in%
     c("Bladen", "Columbus", "Robeson", "Scotland") ){
    out <- "Region 8"
  }
  # Region 7
  if(calc_location_county %in%
     c("Anson", "Harnett", "Hoke", "Johnston", "Lee", "Montgomery", "Moore", "Randolph", "Richmond") ){
    out <- "Region 7"
  }
  # Region 6
  if(calc_location_county %in%
     c("Alamance", "Caswell", "Chatham", "Person", "Rockingham") ){
    out <- "Region 6"
  }
  # Region 5
  if(calc_location_county %in%
     c("Cabarrus", "Davidson", "Rowan", "Stanly", "Union") ){
    out <- "Region 5"
  }
  # Region 4
  if(calc_location_county %in%
     c("Davie", "Iredell", "Stokes", "Surry", "Yadkin") ){
    out <- "Region 4"
  }
  # Region 3
  if(calc_location_county %in%
     c("Alexander", "Burke", "Caldwell", "Catawba", "Mcdowell") ){
    out <- "Region 3"
  }
  # Region 2
  if(calc_location_county %in%
     c("Henderson", "Polk", "Rutherford", "Transylvania") ){
    out <- "Region 2"
  }
  # Region 1
  if(calc_location_county %in%
     c("Cherokee", "Clay", "Graham", "Haywood", "Jackson", "Macon", "Madison", "Swain") ){
    out <- "Region 1"
  }
  
  out1 <- NULL
  try(out1 <- out)
  
  if(length(out1) != 1){
    out1 <- NA
  }
  
  return(out1)
}


search_region.names <- function(projname){
  print(projname)
  if(length(projname) == 1){
    out <- strsplit(projname, split = " - | -|- ")
    out <- unlist(out)[grepl("Region", x = unlist(out), ignore.case = F)]
    #out <- strsplit(out, " ")
    #out <- unlist(out[unlist(lapply(X = out, FUN = length)) == 2]) %>% 
    #  paste(., sep = " ", collapse = " ") #%>%
    # #gsub(pattern = " County", "", .)
  }else{
    out <- NA
  }
  
  if(!grepl("Region", x = out)){
    out <- NA
  }
  
  return(out)
}

search_county.names <- function(projname){
  print(projname)
  if(length(projname) == 1){
    out <- strsplit(projname, split = " - | -|- ")
    out <- unlist(out)[grepl("County$", x = unlist(out), ignore.case = F)]
    out <- strsplit(out, " ")
    out <- unlist(out[unlist(lapply(X = out, FUN = length)) == 2]) %>% 
      paste(., sep = " ", collapse = " ") #%>%
    #gsub(pattern = " County", "", .)
    
    
  }else{
    out <- NA
  }
  
  if(!grepl("County", x = out)){
    out <- NA
  }
  
  return(out)
}



#/ formulas developed in "nccounty_logic.R"


fun_dob_dataqual <- function(x){
  out <- data.frame(x_in = c(1,2,8,9,99), 
                    x_txt = c("Full DOB reported", 
                              "Approximate or partial DOB reported", 
                              "Client doesn't know", 
                              "Client refused", 
                              "Data not collected"))
  out1 <- out[out$x_in == x,]$x_txt
  if(any(is.na(out1)) | length(out1) == 0){
    out1 <- "[undetermined]"
  }
  
  return(out1)
}



fun_lengthOfStay <- function(x){
  out <- data.frame(x_in = c(2:5,8:11,99), 
                    x_txt = c("One week or more, but less than one month",
                              "One month or more, but less than 90 days",
                              "90 days or more but less than one year", 
                              "One year or longer", 
                              "Client doesn't know", 
                              "Client refused", 
                              "One night or less", 
                              "Two to six nights", 
                              "Data not collected"))
  out1 <- out[out$x_in == x,]$x_txt
  if(any(is.na(out1)) | length(out1) == 0){
    out1 <- "[undetermined]"
  }
  
  return(out1)
}



fun_projtype <- function(x){
  out <- data.frame(x_in = c(1:4,
                             6:14), 
                    x_txt = c("ES", "TH", 
                              "PH - psh", "Street Outreach", 
                              "Services only", 
                              "Other", "Safe Haven", 
                              "PH - Housing only", 
                              "PH - Housing with services", 
                              "Day Shelter", 
                              "Homelessness Prevention", 
                              "PH - rrh", 
                              "Coordinated Entry"))
  out1 <- out[out$x_in == x,]$x_txt
  if(any(is.na(out1))| length(out1) == 0){
    out1 <- "[undetermined]"
  }
  
  return(out1)
}


fun_livingsituation_def <- function(x){
  out <- data.frame(x_in = c(16,1,18,15,6,
                             7,25,4,5,29,
                             14,2,32,13,36, 
                             12,22,35,23,26,
                             27,28,19,3,31,33,34,10,20,21,
                             11,30,17,24,37,8,9,99), 
                    x_txt = c("Place not meant for human habitation", 
                              "Emergency Shelter", "Safe Haven",
                              "Foster care home or foster care group home", 
                              "Hospital or other residential non-psychiatric medical facility",
                              "Jail, prison or juvenile detention facility", 
                              "Long-term care facility or nursing home", 
                              "Psychiatric hospital or other psychiatric facility", 
                              "Substance abuse treatment facility or detox center", 
                              "Residential project or halfway house iwth no homeless criteria",
                              "Hotel or motel paid without emergency shelter voucher", 
                              "Transitional Housing", 
                              "Host Home (non-crisis)", 
                              "Staying or living with friends, temporary tenure", 
                              "Staying or living in a friend's room, apartment, or house",
                              "Staying or living with family, temporary tenure", 
                              "Staying or living with family, permanent tenure", 
                              "Staying or living in a family member’s room, apartment or house", 
                              "Staying or living with friends, permanent tenure", 
                              "Moved from one HOPWA funded project to HOPWA PH", 
                              "Moved from one HOPWA funded project to HOPWA TH", 
                              "Rental by client, with GPD TIP housing subsidy", 
                              "Rental by client, with VASH housing subsidy", 
                              "Permanent housing (other than RRH) for formerly homeless persons", 
                              "Rental by client (RRH)", 
                              "Rental by client (HCV - tenant or project based)", 
                              "Rental by client in public housing unit", 
                              "Rental by client, no ongoing housing subsidy", 
                              "Rental by client with other ongoing housing subsidy", 
                              "Owned by client, with ongoing housing subsidy", 
                              "Owned by client, no ongoing housing subsidy", 
                              "No exit interview completed", 
                              "Other", "Deceased", "Worker unable to determine", 
                              "Client doesn't know", "Client refused", "Data not collected"))
  out2 <- out[out$x_in == x,]$x_txt
  
  if(any(is.na(out2))){
    out2 <- "Data not collected"
  }
  
  return(out2)
}


fun_hhtype <- function(x){
  if(is.na(x)){
    out <- "[Undetermined]"
  }else if(x == 1){
    out <- "Households without children"
  }else if(x == 3){
    out <- "Households with at least one adult and one child"
  }else if(x == 4){
    out <- "Households with only children"
  }else{
    out <- "[Undetermined]"
  }
  return(out)
}



fun_rel2hoh <- function(x){
  if(is.na(x)){
    out <- "Data not collected"
  }else if(x == 2){
    out <- "Head of household’s Child"
  }else if(x == 3){
    out <- "Head of household’s spouse or partner"
  }else if(x == 4){
    out <- "Head of household’s other relation member"
  }else if(x == 5){
    out <- "Other: non-relation member "
  }else if(x == 99){
    out <- "Data not collected"
  }else if(x == 1){
    out <- "Self (head of household)"
  }else{
    out <- "[undetermined]"
  }
  return(out)
}

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

fun_1.8_def <- function(x){
  if(is.na(x)){
    out <- "Data not collected"
  }else if(x == 1){
    out <- "Yes"
  }else if(x == 8){
    out <- "Client doesn't Know"
  }else if(x == 9){
    out <- "Client refused"
  }else if(x == 99){
    out <- "Data not collected"
  }else if(x == 0){
    out <- "No"
  }else{
    out <- "[undetermined]"
  }
  return(out)
}

fun_ethnicity_def <- function(x){
  if(x == 0){
    out <- "Non-Hispanic/Non-Latin(a)(o)(x)"
  }else if(x == 1){
    out <- "Hispanic/Latin(a)(o)(x)"
  }else if(x == 8){
    out <- "Client doesn’t know"
  }else if(x == 9){
    out <- "Client refused"
  }else if(x == 99){
    out <- "Data not collected"
  }else if(is.na(x)){
    out <- "Data not collected"
  }else {
    out <- "[undetermined]"
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
