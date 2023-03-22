
lead0 <- function(x){
  # adds a leading zero to numbers - useful for setting time from string
  if(!is.character(x)){
    x <- as.character(x)
  }
  if(nchar(x) == 1){
    out <- paste("0", x, sep = "")
  }else{
    out <- x
  }
  return(out)
}

fun_titlecase <- function(string = "a string of words"){
  require(dplyr)
  out <- unlist(strsplit(string, " ")) %>%
    strsplit(., "")
  
  for(i in 1:length(out)){
    out[[i]][1] <- toupper(out[[i]][1])
  }
  
  out <- lapply(X = out, 
                FUN = paste, 
                sep = "", collapse = "") %>%
    unlist() %>%
    paste(., sep = " ", collapse = " ")
  
  return(out)
}

current_folder <- function() {
  path_name <- rstudioapi::getSourceEditorContext()$path
  if (path_name == "") {
    return("File not saved")
  } else {
    utils::browseURL(dirname(path_name))
  }
}

calc_disabling_condition <- function(physical.disab, 
                                     chronic.health.cond, 
                                     mental.health.disord, 
                                     substance.use.disord, 
                                     developmental.disab, 
                                     hiv_aids){
  # The Data Standards allow systems to auto populate the disabling condition
  # field with “yes” when a client answers “Yes” (Dependent Field A = “Yes”) to
  # one of the disability criteria data elements: Physical Disability, Chronic
  # Health Condition, Mental Health Disorder, and/or Substance Use Disorder
  # (4.05, 4.07, 4.09, 4.10), or if Developmental disability (4.06) or HIV/AIDS
  # (4.08) = “Yes” (Field 2 = “Yes”). Reporting should always count these
  # clients as having a Disabling Condition. Though the programming instructions
  # below only directly reference [disabling condition], it is expected that
  # this field is consistent with the auto-population option in the HMIS. So,
  # for projects or systems where auto-population is not used, tests for each of
  # the separate Dependent Field A for 4.05, 4.07, 4.09 and 4.10, as well as
  # Field 2 for 4.06 and 4.08, must occur within the programming logic of each
  # relevant question.
  
  # see:
  # https://files.hudexchange.info/resources/documents/HMIS-Standard-Reporting-Terminology-Glossary.pdf
  
  if(any(physical.disab, 
         chronic.health.cond, 
         mental.health.disord, 
         substance.use.disord, 
         developmental.disab, 
         hiv_aids)){
    out <- T
  }else{
    out <- F
  }
  return(out)
}




disability_type.1.3.def <- function(disability.type.val){
  # error check
  if(!disability.type.val %in% c(5:10)|
     is.na(disability.type.val)){
    stop("disability.type.val must be one of 5,6,7,8,9,10 or NA")
  }
  
  if(disability.type.val == 5){
    out <- "Physical disability"
  }
  if(disability.type.val == 6){
    out <- "Developmental disability"
  }
  if(disability.type.val == 7){
    out <- "Chronic health condition"
  }
  if(disability.type.val == 8){
    out <- "HIV/AIDS"
  }
  if(disability.type.val == 9){
    out <- "Mental health disorder"
  }
  if(disability.type.val == 10){
    out <- "Substance use disorder"
  }
  if(is.na(disability.type.val)){
    out <- NA
  }
  return(out)
}

disability_response.4.10.2.def <- function(disab.response.val, 
                                           disabilityType){
  # logic
  # if DisabilityType is '10' (Substance Abuse Disorder)
  if(disabilityType == 10){
    if(disab.response.val == 0){
      out <- "Not disabled"
    }
    if(disab.response.val == 1){
      out <- "Alcohol use disorder"
    }
    if(disab.response.val == 2){
      out <- "Drug use disorder"
    }
    if(disab.response.val == 3){
      out <- "Both alcohol and drug use disorders"
    }
    if(disab.response.val == 8){
      # 8 = Client doesn't know
      out <- "unknown or cannot tell if disabled"
    }
    if(disab.response.val == 9){
      # 9 = Client refused
      out <- "unknown or cannot tell if disabled"
    }
    if(disab.response.val == 99 |
       # 99 = Data not collected
       is.na(disab.response.val)){
      out <- "unknown or cannot tell if disabled"
    }
  }else{
    # else DisabilityType is NOT '10' (Substance Abuse Disorder)
    if(disab.response.val == 0){
      out <- "Not disabled"
    }
    if(disab.response.val == 1){
      out <- "Disabled"
    }
    if(disab.response.val == 8 |
       # 8 = Client doesn't know
       is.na(disab.response.val)){
      out <- "unknown or cannot tell if disabled"
    }
    if(disab.response.val == 9 |
       # 9 = Client refused
       is.na(disab.response.val)){
      out <- "unknown or cannot tell if disabled"
    }
    if(disab.response.val == 99 |
       # 99 = Data not collected
       is.na(disab.response.val)){
      out <- "unknown or cannot tell if disabled"
    }
  }
  return(out)
}

make_abbr <- function(string = "go ahead and make my day without me"){
  require(dplyr)
  # returns an abbreviation for a multi-word string in by taking the first
  # letter of each word, making them upper-case and pasting together to form a
  # single word string output
  
  # error checking
  if(length(string) != 1){
    stop("Arg \"string\" must be exactly length() 1")
  }
  
  out <- strsplit(string, " ") %>%
    unlist()
  # special case: 'and' >>> '&'
  out[out == "and"] <- "&"
  out <- out %>%
    strsplit(., "") %>%
    lapply(., first) %>%
    unlist()
  out <- out %>% toupper()
  
  # special cases----
  # make 'of' lowercase
  # if(grepl("\\bof\\b", string, ignore.case = T)){
  #   of.which <- strsplit(x = string, split = " ") %>%
  #     unlist() %>% tolower()
  #   of.which <- which(of.which == "of")
  #   out[of.which] <- "o"
  # }
  
  # without to w/o (lowercase)
  if(grepl("\\bwithout\\b", string, ignore.case = T)){
    without.which <- strsplit(x = string, split = " ") %>%
      unlist() %>% tolower()
    without.which <- which(without.which == "without")
    out[without.which] <- "w/o"
  }
  out <- out %>% paste(., sep = "", collapse = "")
  return(out)
}

make_abbr()

pit_xls_info <- function(filenames){
  require(lubridate)
  require(dplyr)
  filenames <- filenames[which(unlist(lapply(strsplit(filenames, "2023__"), first)) %in% 
                                 c("andrea_output", "DQ_Flag"))]
  
  
  
  df.out <- data.frame(which_file =  unlist(lapply(strsplit(x = filenames, 
                                                            split = "2023__"), 
                                                   first)), 
                       file_name = filenames, 
                       file_date = ymd(gsub("_HR.*$", "", 
                                            gsub("\\.xlsx$", "", 
                                                 unlist(lapply(strsplit(x = filenames, 
                                                                        split = "2023__"), 
                                                               last)))))) %>%
    as_tibble()
  
  df.out$file_datetime <- as_datetime(df.out$file_date) %m+% 
    hours(as.numeric(unlist(lapply(strsplit(gsub("\\.xlsx$", "", 
                                                 unlist(lapply(strsplit(x = filenames, 
                                                                        split = "2023__"), 
                                                               last))), 
                                            split = "_HR"), 
                                   last)))) %>%
    force_tz(., tzone = Sys.timezone())
  
  df.out <- mutate(df.out, 
                   year = year(file_date),
                   month = lubridate::month(file_date, abbr = T, label = F), 
                   mday = mday(file_date),
                   dow   = lubridate::wday(file_date, label = T, abbr = T), 
                   hr.day = as.numeric(strftime(file_datetime, format = "%H")))
  return(df.out)
}

fun_flag_nmfhh_and_1day_before.after_pitnight <- function(hh_cls1, hh_cls_infodate1, 
                                                          pit.night1){
  ((hh_cls1 == "16" | hh_cls1 == 16) & !is.na(hh_cls1)) & 
    hh_cls_infodate1 != pit.night1
}

get_xl.date.numeric <- function(unix_date = ymd(20200501)){
  require(lubridate)
  return(as.numeric(unix_date) + 25569)
}

get_unix.date.date <- function(excel_numeric = 43952){
  require(lubridate)
  return(as_date(excel_numeric-25569))
}

# excel_nDate <- function(excelN = 44587){
#   require(lubridate)
#   require(dplyr)
#   # 44587 = 1-22-22
#   return(ymd(18991230) %m+% days(excelN))
# }

# most recent cls function: 
calc_CLS_final <- function(a.enr, a.cls){
  require(dplyr)
  
  a.enr <- a.enr[,c("HouseholdID", 
                    "RelationshipToHoH",
                    "EnrollmentID",
                    "PersonalID",
                    "ProjectID")]
  
  a.cls <- a.cls[,c("CurrentLivingSitID",
                    "EnrollmentID",
                    "PersonalID",
                    "InformationDate",
                    "CurrentLivingSituation")]
  
  # join datas
  a.enrcls <- left_join(a.enr, a.cls, 
                        by = c("EnrollmentID", "PersonalID")) %>%
    # add new binary field for HoH 
    mutate(., HoH = ifelse(RelationshipToHoH == 1, "HoH", "not HoH")) %>%
    group_by(HouseholdID, RelationshipToHoH, 
             EnrollmentID, PersonalID, ProjectID, InformationDate, 
             CurrentLivingSituation) %>% 
    summarise() %>%
    .[order(.$HouseholdID,.$PersonalID),]
  
  # Prepare for loop
  a.enrcls$hh_cls <- NA          # will be the household currentLivingSituation
  a.enrcls$hh_cls_infodate <- NA # will be the household CLS InformationDate
  a.enrcls$HoH_PersonalID <- NA  # will be the household HoH PersonalID
  
  # For each row in the joined data.frame
  for(i in 1:nrow(a.enrcls)){
    temp.hhid <- a.enrcls$HouseholdID[i]
    temp.projid <- a.enrcls$ProjectID[i]
    temp.infodate <- a.enrcls$InformationDate[i]
    
    # is "i" HoH?  
    if(a.enrcls$RelationshipToHoH[i] == 1){
      a.enrcls$hh_cls[i] <- a.enrcls$CurrentLivingSituation[i]
      a.enrcls$hh_cls_infodate[i] <- a.enrcls$InformationDate[i]
      a.enrcls$HoH_PersonalID[i] <- a.enrcls$PersonalID[i]
    }else{
      
      temp.hoh.pid <- a.enrcls$PersonalID[a.enrcls$HouseholdID == temp.hhid & 
                                            a.enrcls$ProjectID == temp.projid & 
                                            a.enrcls$RelationshipToHoH == 1] %>% unique
      if(length(temp.hoh.pid) == 0){
        # there is no hoh
        a.enrcls$hh_cls[i] <- "no HoH found"#NA
        a.enrcls$hh_cls_infodate[i] <- "no HoH found"#NA
        a.enrcls$HoH_PersonalID[i] <- "no HoH found"#NA
      }else{
        # carry on 
        temp.hh_cls <- a.enrcls[a.enrcls$PersonalID %in% temp.hoh.pid & 
                                  a.enrcls$HouseholdID == temp.hhid & 
                                  a.enrcls$ProjectID == temp.projid,]
        
        if(nrow(temp.hh_cls) > 1 & 
           all(!is.na(temp.hh_cls$InformationDate)) & 
           !temp.infodate %in% temp.hh_cls$InformationDate){
          # row [i] has no InformationDate to join with, so you're stuck
          a.enrcls$hh_cls[i] <- "NA"
          a.enrcls$hh_cls_infodate[i] <- "NA"
          a.enrcls$HoH_PersonalID[i] <- "NA"
        } 
        if(nrow(temp.hh_cls) > 1 & 
           all(!is.na(temp.hh_cls$InformationDate)) & 
           temp.infodate %in% temp.hh_cls$InformationDate){
          # row [i] has no InformationDate to join with, so you're stuck
          temp.hh_cls <- temp.hh_cls[temp.hh_cls$InformationDate == temp.infodate,]
          a.enrcls$hh_cls[i] <- temp.hh_cls$CurrentLivingSituation
          a.enrcls$hh_cls_infodate[i] <- temp.hh_cls$InformationDate
          a.enrcls$HoH_PersonalID[i] <- temp.hh_cls$HoH_PersonalID
        } 
        if(nrow(temp.hh_cls) == 1){
          a.enrcls$hh_cls[i] <- temp.hh_cls$CurrentLivingSituation  
          a.enrcls$hh_cls_infodate[i] <- temp.hh_cls$InformationDate
          a.enrcls$HoH_PersonalID[i] <- temp.hh_cls$PersonalID
        }
        
        # placed this logic here because was getting warning that there were
        # multiple values trying to be assigned to a single var
        if(is.na(a.enrcls$hh_cls[i])){a.enrcls$hh_cls[i] <- "error - multiple rows"}
        if(is.na(a.enrcls$hh_cls_infodate[i]))  {a.enrcls$hh_cls_infodate[i] <- "error - multiple rows"}
        if(is.na(a.enrcls$HoH_PersonalID[i])){ a.enrcls$HoH_PersonalID[i] <- "error - multiple rows"}
        
      }
    }
  }
  
  # peform a few fixes related to date formatting
  a.enrcls$hh_cls_infodate.char <- a.enrcls$hh_cls_infodate
  a.enrcls$hh_cls_infodate <- a.enrcls$hh_cls_infodate %>% 
    as.numeric() %>%
    lubridate::as_date()
  
  return(a.enrcls)
}

# formulas developed in "nccounty_logic.R"
flag_colnames <- function(x){
  grep("^flag", colnames(x), ignore.case = T, value = T)
}
is_household_CLS_nmfhh <- function(pid, eid, 
                                   df_enr = a.enrollment, 
                                   df_cls = a.currentlivingsituation){
  
  hhid <- df_enr$HouseholdID[df_enr$PersonalID == pid & 
                               df_enr$EnrollmentID == eid] %>%
    unique()
  # is pid the hoh?
  pid_is_hoh <- unique(df_enr$reltionshiptohoh_def[df_enr$PersonalID == pid & 
                                                     df_enr$EnrollmentID == eid]) == 
    "Self (head of household)"
  
  # if pid IS the HoH
  if(pid_is_hoh){
    # do this
    pid_cls <- left_join(df_enr[,c("PersonalID", "EnrollmentID", "HouseholdID")], 
                         df_cls[,c("PersonalID", "EnrollmentID", 
                                   "CurrentLivingSituation")]) %>%
      .[.$PersonalID == pid & 
          .$EnrollmentID == eid,] %>%
      .$CurrentLivingSituation %>% unique()
    
    hh_cls <- pid_cls
  }else{
    # do that
    hoh_pid_cls <- left_join(df_enr[,c("PersonalID", "EnrollmentID", "HouseholdID")], 
                             df_cls[,c("PersonalID", "EnrollmentID", 
                                       "CurrentLivingSituation")]) %>%
      .[.$HouseholdID == hhid & 
          .$EnrollmentID == eid,] %>%
      .$CurrentLivingSituation %>% unique()
    hh_cls <- hoh_pid_cls
  }
  
  out <- NA
  if(length(hh_cls) != 1){
    if(length(hh_cls) == 0){ out <- "no cls records found"}
    if(length(hh_cls) > 1){ out <- "multiple cls records found"}
  }else{
    if(length(hh_cls) == 1 & hh_cls == 16 & !is.na(hh_cls)){ out <- "Place Not Meant for Human Habitation"}
    if(length(hh_cls) == 1 & hh_cls != 16 & !is.na(hh_cls)){ out <- "other than NMFHH"}
  }
  return(out)
}


get.calc_location_county <- function(housingtype, proj.address.county, 
                                     nccounty){
  return(ifelse(test = housingtype %in% c(1,2) & !is.na(housingtype), 
                yes  = proj.address.county, 
                no   = nccounty))
}

get.calc_location_county(housingtype = 1, 
                         proj.address.county = NA, 
                         nccounty = "Rowan")

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

get.calc_region <- function(calc_location_county = NA){
  require(dplyr)
  out <- NA
  # case_correct
  clc <- tolower(calc_location_county)
  
  if(!is.na(clc)){
    clc <- paste(toupper(substr(clc,1,1)), substr(clc,2,nchar(clc)), 
                 sep = "", collapse = "")
  }
  
  
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



search_region.names <- function(projname = "Greenville Community Shelters - (Region 1) Pitt County - Emergency Shelter - ES - State ESG CV"){
  #print(projname)
  out <- gsub(pattern = "Region {1,}", "Region_", projname)
  out <- strsplit(out, " ") %>% unlist()
  out <- out[grepl(pattern = "Region_", x = out)] %>% unique()
  
  if(length(out) == 1){
    out <- strsplit(out, split = " ")
    out <- unlist(out)[grepl("Region", x = unlist(out), ignore.case = F)]
    out <- gsub("_", " ", out)
    out <- gsub("\\(|\\)", "", out)
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
  #print(projname)
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
  # updated 1/24/23
  if(is.na(x)){
    out1 <- NA
  }else{
    out <- data.frame(x_in = c(1,2,8,9,99), 
                      x_txt = c("Full DOB reported", 
                                "Approximate or partial DOB reported", 
                                "Client doesn't know", 
                                "Client refused", 
                                NA#"Data not collected"
                      ))
    out1 <- out[out$x_in == x & !is.na(out$x_in),]$x_txt
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
    out <- NA
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
    out <- NA#"[unknown]"
  }
  
  if((gendernone == c(99) | is.na(gendernone)) & out == "GenderNone"){
    out <- NA
  }
  
  if(out == "[unknown]" & !is.na(out)){
    out <- NA
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


fun_ethnicity_def <- function(x){
  if(x  == 0 & !is.na(x)){
    out <- "Non-Hispanic/Non-Latin(a)(o)(x)"
  }else if(x == 1 & !is.na(x) ){
    out <- "Hispanic/Latin(a)(o)(x)"
  }else if(x  == 8 & !is.na(x) ){
    out <- "Client doesn’t know"
  }else if(x  == 9 & !is.na(x) ){
    out <- "Client refused"
  }else if(x == 99 & !is.na(x) ){
    out <- NA#"Data not collected"
  }else if(is.na(x)){
    out <- NA#"Data not collected"
  }else {
    out <- NA
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

# cast down to 1 row per enrollment with 8 columns (1 per disability). Sometimes
# there might be 2+ enrollments on 1 date for 1 person (service + shelter) so we
# need to look at those and see how to handle them.

hud_disabling_condition <- function(){
  # see: https://files.hudexchange.info/resources/documents/FY-2022-HMIS-Data-Dictionary.pdf
  # see: 3.08 disabling condition
  # see: Enrollments.csv$DisablingCondition
  # see: 1.8 (0=no,1=yes,8.9.99 = "unknown or cannot tell")
  
  # NOTE: this value may be set to "YES" independent of any other data element. 
  
  # NOTE: this value may be inferred by the responses to "ability to live
  # independently (4.05, 4.07, 4.09, or 4.10, 0r 4.06, or 4.08)
  
  # NOTE: this value may be automatically-populated 
  # NOTE: regardless of anything else mentioned above, if a client has a Physical Disability 
}

parse_DisabilitiesID <- function(dID, return_disability.type = T){
  require(dplyr)
  #print(dID)
  split.temp <- strsplit(x = dID, 
                         split = "_") %>%
    unlist()
  
  # eid
  temp.eid <- split.temp[1]
  
  # suffix_num
  # suffix_char
  if(length(split.temp) == 3){
    temp.suffix_num  <- split.temp[2]
    temp.suffix_char <- split.temp[3]
  }else if(length(split.temp) == 2){
    temp.suffix_num  <- ifelse(!is.na(as.numeric(split.temp[2])), 
                               yes = split.temp[2], # the number
                               no  = NA)            # the the character
    temp.suffix_char <- ifelse(is.na(as.numeric(split.temp[2])), 
                               yes = split.temp[2], # the character
                               no  = NA)            # the number
  }else{
    temp.suffix_num  <- NA
    temp.suffix_char <- NA
  }
  
  out <- c("EnrollmentID" = temp.eid, 
           "suffix_number" = temp.suffix_num, 
           "suffix_character" = temp.suffix_char)
  
  if(return_disability.type){
    if(out["suffix_character"] == "C"){
      out["suffix_character"] <- "CH.condition"
    }else if(out["suffix_character"] == "D"){
      out["suffix_character"] <- "D.disability"
    }else if(out["suffix_character"] == "H"){
      out["suffix_character"] <- "HIV.AIDS"
    }else if(out["suffix_character"] == "M"){
      out["suffix_character"] <- "MH.disorder"
    }else if(out["suffix_character"] == "P"){
      out["suffix_character"] <- "P.disability"
    }else if(out["suffix_character"] == "S"){
      out["suffix_character"] <- "SU.disorder"
    }else{
      out["suffix_character"] <- NA
    }
  }
  
  return(out)
}

# dis_df <- a.disabilities

screened_positive_disability2 <- function(dis_df){
  # desired outputs (multiple select from following list): 
  # (M)ental Health Disorder               (DisabilityType 9), 
  # (S)ubstance Use Disorder               (Disability Type 10)
  # Alcohol Use Disorder                   (DisabilityType 10; DisabilityResponse [??]), 
  # Drug Use Disorder                      (DisabilityType 10; DisabilityResponse [??]), 
  # Both Alcohol and Drug Use Disorders    (DisabilityType 10; DisabilityResponse [??]),
  # (C)hronic Health Condition             (DisabilityType 7),
  # (H)IV/AIDS                             (DisabilityType 8),
  # (D)evelopment Disability               (DisabilityType 6), 
  # (P)hysical Disability                  (DisabilityType 5)

  # NOTE: Everyone will have a response for all of the above Types.  The
  # response will be listed in DisabilityResponse,   you will need to filter
  # down after that to make sense of who has what.
  
  
  # work down to the columns we need
  dis_df <- dis_df[colnames(dis_df) %in% 
                     grep(pattern = "disab|Disab|^Disab.*ID$|^Enr.*ID$|^Pers.*ID$|Date_disab$|Stage$|^Indef", 
                          x = colnames(dis_df), ignore.case = F, 
                          value = T)] 
  
  # get dID suffix_char
  dis_df$dID_suffix.type <- lapply(X = dis_df$DisabilitiesID, FUN = parse_DisabilitiesID) %>% lapply(., nth, 3) %>% unlist()
  dis_df$dID_2           <- paste(unlist(lapply(lapply(X = dis_df$DisabilitiesID, FUN = parse_DisabilitiesID), nth, 1)), 
                                  unlist(lapply(lapply(X = dis_df$DisabilitiesID, FUN = parse_DisabilitiesID), nth, 2)), 
                                  sep = "_")#, 
  
  dis_df
  
  # bring in text values for dr and dt
  dis_df$DisabilityType_text <-  unlist(lapply(X = dis_df$DisabilityType, 
                                               FUN = disability_type.1.3.def))
  dis_df$DisabilityResponse_text <- unlist(mapply(FUN = disability_response.4.10.2.def, 
                                                  dis_df$DisabilityResponse, 
                                                  dis_df$DisabilityType))
  
  # DEFINITION: "indefinite and impairing":  If Yes for “Physical Disability”,
  # "Chronic Health Condition", "Mental Health Disorder", OR if Alcohol use
  # disorder, Drug use disorder, or Both alcohol and drug use disorders for
  # “Substance Use Disorder”, THEN... Expected to be of long–continued and
  # indefinite duration and substantially impairs ability to live independently
  # (https://files.hudexchange.info/resources/documents/FY-2022-HMIS-Data-Dictionary.pdf)
  
  dis_df$IndefiniteAndImpairs_txt <- unlist(lapply(X = dis_df$IndefiniteAndImpairs, 
                                                   FUN = fun_1.8_def)) %>%
    ifelse(. %in% 
             c("Client doesn't Know", 
               "Client refused") | 
             is.na(.), 
           yes = "unknown or cannot tell", 
           no = .)
  
 
  dis_df$DisabilityResponse_text.categories_calc <- NA
  dis_df$DisabilityResponse_text.categories_calc <- ifelse(dis_df$DisabilityResponse_text %in%
                                                             c("Disabled", 
                                                               "Drug use disorder", 
                                                               "Alcohol use disorder", 
                                                               "Both alcohol and drug use disorders"), 
                                                           yes = "DisabResp_YES", 
                                                           no = dis_df$DisabilityResponse_text.categories_calc)
  dis_df$DisabilityResponse_text.categories_calc <- ifelse(dis_df$DisabilityResponse_text %in%
                                                             c("Not disabled"), 
                                                           yes = "DisabResp_NOorUNKNOWN", 
                                                           no = dis_df$DisabilityResponse_text.categories_calc)
  dis_df$DisabilityResponse_text.categories_calc <- ifelse(dis_df$DisabilityResponse_text %in%
                                                             c("unknown or cannot tell if disabled"), 
                                                           yes = "DisabResp_NOorUNKNOWN", 
                                                           no = dis_df$DisabilityResponse_text.categories_calc)
  
  dis_df$IndefiniteAndImpairs_txt.categories_calc <- NA
  dis_df$IndefiniteAndImpairs_txt.categories_calc <- ifelse(dis_df$IndefiniteAndImpairs_txt %in% 
                                                              c("Yes"), 
                                                            yes = "IndefImpairs_YES", 
                                                            no = dis_df$IndefiniteAndImpairs_txt.categories_calc)
  dis_df$IndefiniteAndImpairs_txt.categories_calc <- ifelse(dis_df$IndefiniteAndImpairs_txt %in% 
                                                              c("No", "unknown or cannot tell"), 
                                                            yes = "IndefImpairs_NOorUNKNOWN", 
                                                            no = dis_df$IndefiniteAndImpairs_txt.categories_calc)
  
  
  disdf_indimp <- dis_df %>%
    mutate(., 
           IndefiniteAndImpairs_txt = paste("II", 
                                           gsub(" ", "", 
                                                unlist(lapply(IndefiniteAndImpairs_txt, 
                                                              FUN = fun_titlecase))), 
                                           sep = ".")) %>%
    group_by(PersonalID, EnrollmentID, dID_2, InformationDate_disab, dID_suffix.type, 
             DisabilityResponse_text, IndefiniteAndImpairs_txt
             #DisabilityResponse_text.categories_calc, IndefiniteAndImpairs_txt.categories_calc
    ) %>%
    summarise() %>%
    as.data.table() %>%
    dcast.data.table(., 
                     PersonalID + 
                       EnrollmentID + 
                       dID_2 + 
                       InformationDate_disab ~ 
                       dID_suffix.type,
                     value.var = "IndefiniteAndImpairs_txt",
                     #value.var = "IndefiniteAndImpairs_txt.categories_calc",
                     sep = "//") %>%
    as.data.frame() %>% as_tibble()
  
  disdf_disabresp <- dis_df %>%
    mutate(., 
           DisabilityResponse_text = paste("DR", 
                                           gsub(" ", "", 
                                                unlist(lapply(DisabilityResponse_text, 
                                                              FUN = fun_titlecase))), 
                                           sep = ".")) %>%
    group_by(PersonalID, EnrollmentID, dID_2, InformationDate_disab, dID_suffix.type, 
             DisabilityResponse_text, IndefiniteAndImpairs_txt
             #DisabilityResponse_text.categories_calc, IndefiniteAndImpairs_txt.categories_calc
    ) %>%
    summarise() %>%
    as.data.table() %>%
    dcast.data.table(., 
                     PersonalID + 
                       EnrollmentID + 
                       dID_2 + 
                       InformationDate_disab ~ 
                       dID_suffix.type,
                     value.var = "DisabilityResponse_text",
                     sep = "//") %>%
    as.data.frame() 
  
  disdf_ddrii <- full_join(disdf_disabresp, 
                           disdf_indimp, 
                           by = c("PersonalID", "EnrollmentID", "dID_2", 
                                  "InformationDate_disab"), 
                           suffix = c("__dr", "__ii")) %>% as_tibble()
  
  disdf_ddrii %>%
    colnames() %>%
    grep("__", ., value = T) %>%
    gsub("__.*$", "", .) %>%
    unique()
  
  drop.cols <- disdf_ddrii %>%
    colnames() %>%
    grep("__", ., value = T)
  
  disdf_ddrii <- mutate(disdf_ddrii, 
         CH.condition = paste(CH.condition__dr, CH.condition__ii, sep = ", "), 
         D.disability = paste(D.disability__dr, D.disability__ii, sep = ", "), 
         HIV.AIDS     = paste(HIV.AIDS__dr,     HIV.AIDS__ii,     sep = ", "), 
         MH.disorder  = paste(MH.disorder__dr,  MH.disorder__ii,  sep = ", "), 
         P.disability = paste(P.disability__dr, P.disability__ii, sep = ", "), 
         SU.disorder  = paste(SU.disorder__dr,  SU.disorder__ii,  sep = ", ")) %>%
    .[!colnames(.) %in% drop.cols]
  
  # filter by newest informationdate
  
  disdf_ddrii <- disdf_ddrii %>%
    ungroup() %>%
    group_by(PersonalID) %>%
    slice_max(., 
              order_by = InformationDate_disab, 
              n = 1)
 
  return(disdf_ddrii)
}

x <- screened_positive_disability2(dis_df = a.disabilities)

screened_positive_disability <- function(dis_df = c.disabilities, 
                                         enr_df = c.enrollment, 
                                         exit_df = c.exit, 
                                         pit_date = ymd(20230125)){
  
  
  # final score:----
  dis_df$is_disab <- "unknown or cannot tell"
  
  dis_df[dis_df$DisabilityResponse > 1 | 
           dis_df$DisabilityResponse == 99 | # added 3/17/23 but probably redundant
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
  dis_df[dis_df$DisabilityResponse == 1 & 
           (dis_df$IndefiniteAndImpairs %in% c(8,9,99) | 
              is.na(dis_df$IndefiniteAndImpairs)),]$is_disab <- "unknown or cannot tell"
  
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
  
  # filter out all information dates that occur after the PIT survey date and
  # then find the latest InformationDate for each enrollment - that becomes
  # your most recent and thus most applicable date for disability inventory
  
  join_dates <- join_dates %>%
    .[.$InformationDate_disab <= pit_date,] %>%
    group_by(PersonalID, EnrollmentID) %>%
    slice_max(., 
              order_by = InformationDate_disab, 
              n = 1)
  
  join_dates <- join_dates %>%
    left_join(., 
              data.frame(DisabilityType = 5:10, 
                         dt_name = c("physical_D", 
                                     "developmental_D", 
                                     "chronic_hlth_C", 
                                     "HIV.AIDS", 
                                     "mental_health_D", 
                                     "substance_use_D"))) %>% as_tibble() %>% 
    .[colnames(.) %in% c("PersonalID", "EnrollmentID", "InformationDate_disab", "dt_name", "is_disab", 
                         "destination_def", 
                         "DisabilityType", "DisabilityResponse", "IndefiniteAndImpairs", "DataCollectionStage",
                         "DisabilityResponse_text", "DisabilitiesID")]
  
  # #troublesome.records 
  # 
  # trouble.ids <- join_dates %>%
  #   group_by(PersonalID, EnrollmentID, InformationDate_disab, 
  #            dt_name, is_disab) %>%
  #   summarise(n = n(), 
  #             n_DR_txt = n_distinct(DisabilityResponse_text),
  #             n_DisabID = n_distinct(DisabilitiesID)) %>%
  #   .[.$n != .$n_DisabID,] %>%
  #   mutate(., 
  #          trouble_id = paste(PersonalID, 
  #                             EnrollmentID, 
  #                             as.character(InformationDate_disab), 
  #                             sep = "-", collapse = "-")) %>%
  #   .$trouble_id %>%
  #   unique()
  
  
  
  jd <- join_dates %>%
      as.data.table() %>%
    dcast(., 
          PersonalID + EnrollmentID + #is_disab +
            InformationDate_disab ~ dt_name, 
          #fun.aggregate = length
          value.var = "is_disab") 
  
  # There are NA values in this table and it's not clear why----- VVVV
  
  jd[jd$developmental_D == T & jd$chronic_hlth_C == T,]  # this code is wrong - testing for logicals on numeric class
  
  jd %>%
    group_by(developmental_D, HIV.AIDS, chronic_hlth_C, 
             mental_health_D, physical_D, substance_use_D) %>%
    summarise(n = n()) %>%
    .[order(.$n,decreasing = T),]
  
  return(jd)
  
}


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
