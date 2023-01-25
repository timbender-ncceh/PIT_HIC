
# Libraries----
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(data.table)
library(devtools)
library(openxlsx)

rm(list=ls())
cat('\f')
gc()

# NOTE----
print("For hud pit survey for the night of Jan 26th, Entered on January 26th, Exited on Jaunary 27th")

pit.night <- ymd(20220126)

# /NOTE----


# resources----
out.template <- "https://ncceh.sharepoint.com/:x:/s/DataCenter/EdQERAgSu5pGsBcN5VNGD20B3qlfQ7iOCFz9BPJi2xoADQ?e=zOvaac"

# Setup----
#csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/data/coc_by_ffy/bos_2022"
csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/test_data"

# Functions----
#devtools::source_url(url = "https://github.com/timbender-ncceh/R-scripts/blob/main/format_phone_email.R?raw=TRUE")
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/pit_survey_calculations.R?raw=TRUE")


const_nchar <- function(x = 1:130){
  #formats numbers for use in column fields so that they can be sorted as
  #strings and remain in order
  x <- as.character(x)
  max.char <- max(nchar(x))
  out <- list()
  for(i in 1:length(x)){
    out[[i]] <- paste(c(rep("0", max.char - nchar(x[i])), x[i]), sep = "", collapse = "")
  }
  out <- unlist(out)
  return(out)
}

hmis_join <- function(x.file,
                      y.file, 
                      jtype,# = c("full", "left", "right", "inner"),
                      dropcols = c("DateCreated","DateUpdated", 
                                   "DateDeleted", "UserID", 
                                   "ExportID", "CoCCode")){
  require(glue)
  # remove cols
  x.file <- x.file[!colnames(x.file) %in% dropcols]
  y.file <- y.file[!colnames(y.file) %in% dropcols]
  
  # join
  if(jtype == "full"){
    out <- full_join(x.file, y.file)
  }else if(jtype == "left"){
    out <- left_join(x.file, y.file)
  }else if(jtype == "right"){
    out <- right_join(x.file,y.file)
  }else if(jtype == "inner"){
    out <- inner_join(x.file,y.file)
  }else{
    stop("invalid 'jtype' var")
  }
  return(out)
}

is_hashed <- function(x){
  return(nchar(x) == 64 & class(x) == "character")
}

bind_file <- function(filename, 
                      dir1 = "bos_2022", dir2 = "dur_2022", dir3 = "ora_2022", 
                      dir.top = "C:/Users/TimBender/Documents/R/ncceh/data/coc_by_ffy"){
  require(glue)
  require(readr)
  f1 <- glue("{dir.top}/{dir1}/{filename}")
  f2 <- glue("{dir.top}/{dir2}/{filename}")
  f3 <- glue("{dir.top}/{dir3}/{filename}")
  
  return(rbind(read_csv(f1), read_csv(f2), read_csv(f3)))
}

find_cols <- function(patrn = "ID$", ic = T,
                      l = file.colnames){
  out <- list()
  for(i in 1:length(l)){
    out[[names(l[i])]] <- grep(pattern = patrn,
                               x = l[[i]],
                               ignore.case = ic,
                               value = T)
  }
  # remove files with no results returned
  out <- out[lapply(out, length) > 0]
  return(out)
}



# Working Directory Setup----
setwd(csv.file.dir)
getwd()

# Identify the columns that I need to join----
# List all files
csv.files <- list.files(pattern = "\\.csv$") 
csv.files <- csv.files[!csv.files %in% c("Client2.csv", "report_phoneemail.csv")]

# # # build list of colnames
# file.colnames <- list()
# for(i in csv.files){
#   file.colnames[[i]] <- colnames(read_csv(i))
# }

# Client Checks----
a.client <- read_csv("Client.csv")

# pii hash check
a.rando.key <- paste(sample(c(letters,LETTERS,0:9), size = 100,replace = T),sep="",collapse="")

if(sum(is_hashed(a.client$SSN),na.rm = T)== 0){
  a.client$SSN <- openssl::sha256(x = as.character(a.client$SSN), key = a.rando.key)
}

if(sum(is_hashed(a.client$FirstName),na.rm = T)== 0){
  a.client$FirstName <- openssl::sha256(x = as.character(a.client$FirstName), key = a.rando.key)
}

if(sum(is_hashed(a.client$LastName),na.rm = T)== 0){
  a.client$LastName <- openssl::sha256(x = as.character(a.client$LastName), key = a.rando.key)
}

rm(a.rando.key)
# /pii hash check

# add fields
a.client$DOBDataQuality_def <- unlist(lapply(a.client$DOBDataQuality, 
                                             fun_dob_dataqual))

a.client$age_calc     <- calc_age(dob = a.client$DOB)

# FLAG - age----
a.client$flag.age_too_old <- a.client$age_calc >= 80
a.client$flag.DOB_na <- is.na(a.client$DOB) | is.na(a.client$DOBDataQuality)

# a.client[a.client$PersonalID %in% c(1028147,1021469),c("flag.age_too_old", "flag.DOB_na", "PersonalID", 
#                                                        "DOBDataQuality", "DOBDataQuality_def", "DOB")]



a.client$hud_age_calc <- NA
for(i in 1:nrow(a.client)){
  #print(i)
  a.client$hud_age_calc[i] <- hud_age_category(age_yrs = a.client$age_calc[i])
}

a.client$gender_calc  <- NA
for(i in 1:nrow(a.client)){
  a.client$gender_calc[i] <- fun_gender(male        = a.client$Male[i], 
                                        female      = a.client$Female[i], 
                                        nosingle    = a.client$NoSingleGender[i], 
                                        questioning = a.client$Questioning[i], 
                                        trans       = a.client$Transgender[i], 
                                        gendernone  = a.client$GenderNone[i])
}

# FLAG - gender----
a.client$flag.gender <- a.client$gender_calc == "[unknown]" | is.na(a.client$gender_calc)

a.client$race_calc    <- NA
for(i in 1:nrow(a.client)){
  a.client$race_calc[i] <- fun_race(racenone        = a.client$RaceNone[i], 
                                    amindaknative   = a.client$AmIndAKNative[i], 
                                    asian           = a.client$Asian[i], 
                                    blackafamerican = a.client$BlackAfAmerican[i], 
                                    nativehipacific = a.client$NativeHIPacific[i], 
                                    white           = a.client$White[i])
  
}

# FLAG - race----
a.client$flag.race <-  a.client$race_calc == "[unknown]" | is.na(a.client$race_calc)
#table(a.client$flag.race, useNA = "always")


a.client$ethnicity_def <- unlist(lapply(a.client$Ethnicity, fun_ethnicity_def))
# FLAG - ethnicity----
a.client$flag.ethnicity <- a.client$ethnicity_def == "[undetermined]" | is.na(a.client$ethnicity_def)

a.client$vetStatus_def <- unlist(lapply(a.client$VeteranStatus, fun_1.8_def))
# FLAG - veteran status----
a.client$flag.vetstatus <- a.client$vetStatus_def == "[undetermined]" | is.na(a.client$vetStatus_def)

# Enrollment Checks----
a.enrollment <- read_csv("Enrollment.csv")

# FLAG - nccounty----
a.enrollment$flag.nccounty_na <- is.na(a.enrollment$NCCounty)

# FLAG - relationshiphoh----
#a.enrollment$flag.reltohoh_na <- is.na(a.enrollment$RelationshipToHoH) | a.enrollment$RelationshipToHoH == 99
a.enrollment$reltionshiptohoh_def <- unlist(lapply(a.enrollment$RelationshipToHoH, fun_rel2hoh))
a.enrollment$flag.reltohoh_na <- is.na(a.enrollment$reltionshiptohoh_def)


# FLAG - child HOH----
flag.child.hoh <- left_join(a.enrollment[,c("PersonalID", "EnrollmentID", "reltionshiptohoh_def")], 
          a.client[,c("PersonalID", "age_calc")]) %>%
  mutate(., flag.child_hoh = age_calc <= 15 & reltionshiptohoh_def == 
           "Self (head of household)") 
a.enrollment <- left_join(a.enrollment, 
          flag.child.hoh[,c("PersonalID", "EnrollmentID", "flag.child_hoh")])
rm(flag.child.hoh)

# 2023-01-25 removed vvv [CLS]----
# a.enrollment$HoH_PersonalID <- NA
# # a.enrollment$HoH_CLS        <- NA
# # a.enrollment$HoH_CLS_date   <- NA
# for(i in unique(a.enrollment$HouseholdID)){
#   
#   # HoH_PersonalID
#   enr.hoh_pid <- NA
#   try(enr.hoh_pid <- a.enrollment[a.enrollment$HouseholdID == i & 
#      a.enrollment$reltionshiptohoh_def == "Self (head of household)",]$PersonalID)
#   
#   if(length(enr.hoh_pid) != 1){
#     enr.hoh_pid <- NA
#   }
#   
#   try(a.enrollment$HoH_PersonalID[a.enrollment$HouseholdID == i] <- enr.hoh_pid)
#   
#   
#   
# }
# 
# 
# a.enrollment$HoH_PersonalID %>% is.na() %>% table(., useNA = "always")
# END 2023-01-25 remove [CLS]

# NC County of Service & Region
zip_co.cw <- read_tsv(file = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/zip_county_crosswalk.txt")

a.projectcoc <- read_csv(file = "ProjectCoC.csv") %>%
  left_join(., zip_co.cw[,c("ZIP", "County", "City")]) %>%
  left_join(., get_coc_region())

a.projectcoc2 <- read_csv(file = "ProjectCoC.csv") %>%
  .[,c("ProjectID", "City", "State", "ZIP")] %>% 
  left_join(., 
          zip_co.cw[,c("ZIP", "County", "City")]) %>%
  left_join(., 
            get_coc_region())



a.enrollment2 <- left_join(a.enrollment[,c("EnrollmentID", "PersonalID", "ProjectID", 
                "NCCounty")], 
          get_coc_region(), by = c("NCCounty" = "County"))


full_join(a.projectcoc2,a.enrollment2, by = "ProjectID") %>%
  group_by(#County,NCCounty,
           Region.x, Region.x == Region.y) %>%
  summarise(n = n())


# left_join(a.projectcoc2, a.enrollment, 
#             by = c("ProjectID")) 
# 
#   left_join(., get_coc_region(), 
#             by = c("NCCounty" = "County"))

rm(zip_co.cw)


# living situation

# 2023-01-25 removed vvv [CLS]----
#a.enrollment$livingSituation_def <- unlist(lapply(a.enrollment$LivingSituation, fun_livingsituation_def))
# END 2023-01-25 [cls]


# Exit Checks----
a.exit <- read_csv("Exit.csv")

# date filter

all.eids <- full_join(a.enrollment[,c("EnrollmentID", "EntryDate")],
                      a.exit[,c("EnrollmentID", "ExitDate")])

eids.that.span.pit.night <- all.eids$EntryDate <= pit.night & 
  all.eids$ExitDate > pit.night
eids.that.span.pit.night[is.na(eids.that.span.pit.night)] <- F
all.eids$stay_spans_pit_night <- eids.that.span.pit.night


all.eids$lengthOfStay_calc <- all.eids$ExitDate - 
  all.eids$EntryDate

all.eids$days_bw_entry_and_pit_night <- pit.night - all.eids$EntryDate


pit.eids <- all.eids[all.eids$stay_spans_pit_night,]
rm(all.eids)

pit.eids <- pit.eids[!colnames(pit.eids) %in% c("EntryDate", "ExitDate")]

# / date filter

a.exit$destination_def <- unlist(lapply(a.exit$Destination, fun_livingsituation_def))
table(a.exit$destination_def, useNA = "always")

# CurrentLivingSituation Checks (this is the last thing working on tuesday night)----
a.currentlivingsituation <- read_csv("CurrentLivingSituation.csv")

out.cls <- calc_CLS_final(a.enr = a.enrollment, 
               a.cls = a.currentlivingsituation)

a.enrollment <- left_join(a.enrollment, 
          out.cls)


# 2023-01-25 NOTE: works up to this point

# 2023-01-25 removed vvv [CLS]----
# a.currentlivingsituation$currentLivingSituation_def <- unlist(lapply(a.currentlivingsituation$CurrentLivingSituation, 
#                                                                      fun_livingsituation_def))
# a.currentlivingsituation$currentLivingSituation.Date_calc <- a.currentlivingsituation$InformationDate
# 
# 
# # latest.cls <- a.currentlivingsituation %>%
# #   group_by(EnrollmentID, HoH_CLS_date = InformationDate, PersonalID,
# #            currentLivingSituation_def) %>%
# #   summarise() %>%
# #   group_by(EnrollmentID, HoH_PersonalID = PersonalID,
# #            HoH_currentLivingSituation_def = currentLivingSituation_def) %>%
# #   slice_max(., order_by = HoH_CLS_date, n = 1)
# # 
# # a.enrollment <- left_join(a.enrollment, 
# #           latest.cls, by = c("EnrollmentID", "PersonalID" = "HoH_PersonalID" ))
# # 
# # colnames(a.enrollment)
# 
# # # # test
# # # test.hhid <- "s_1146500"
# # # test.pid  <- 128731
# # # test.eid <- a.enrollment$EnrollmentID[a.enrollment$HouseholdID == test.hhid]
# # # 
# # # 
# # # 
# # # is_household_CLS_nmfhh(pid = test.pid, 
# # #                        eid = test.eid, 
# # #                        df_enr = a.enrollment, 
# # #                        df_cls = a.currentlivingsituation)
# # # # /test
# # 
# # 
# # 
# # # 20 minute logic below vvv----
# # a.enrollment$calc_household_currentlivingsituation <- NA
# # for(i in 1:nrow(a.enrollment)){
# #   #if(i > 1000){break}
# #   a.enrollment$calc_household_currentlivingsituation[i] <- 
# #     is_household_CLS_nmfhh(pid = a.enrollment$PersonalID[i], 
# #                                    eid = a.enrollment$EnrollmentID[i],
# #                                    df_enr = a.enrollment, 
# #                                    df_cls = a.currentlivingsituation) 
# # }
# 
# temp.hh.cls.info <- left_join(a.enrollment[,c("PersonalID", 
#                                               "EnrollmentID", 
#                                               "HouseholdID", 
#                                               "RelationshipToHoH")],
#                               a.currentlivingsituation[,c("EnrollmentID", 
#                                                           "PersonalID", 
#                                                           "InformationDate", 
#                                                           "CurrentLivingSituation")]) %>%
#   .[.$RelationshipToHoH == 1,
#     c("HouseholdID","EnrollmentID", "PersonalID",
#       "InformationDate", "CurrentLivingSituation")]
# 
# colnames(temp.hh.cls.info)[3] <- "HoH_PersonalID_cls"
# 
# temp.hh.cls.info <- left_join(a.enrollment, 
#                               temp.hh.cls.info, by = "EnrollmentID", "HouseholdID")
# 
# temp.hh.cls.info$calc_household_currentlivingsituation <- NA
# 
# for(i in 1:nrow(temp.hh.cls.info)){
#   if(!is.na(temp.hh.cls.info$CurrentLivingSituation[i])){
#     temp.hh.cls.info$calc_household_currentlivingsituation[i] <- is_household_CLS_nmfhh(pid = temp.hh.cls.info$PersonalID[i], 
#                                                                                         eid = temp.hh.cls.info$EnrollmentID[i],
#                                                                                         df_enr = a.enrollment, 
#                                                                                         df_cls = a.currentlivingsituation) 
#   }
# }
# 
# # rename and drop columns
# # temp.hh.cls.info <- temp.hh.cls.info[!colnames(temp.hh.cls.info) %in% 
# #                                        c("PersonalID")]
# 
# # colnames(temp.hh.cls.info)[colnames(temp.hh.cls.info) %in%
# #                              c("PersonalID")] <- "HoH_PersonalID"
# colnames(temp.hh.cls.info)[colnames(temp.hh.cls.info) %in%
#                              c("InformationDate")] <- "calc_CLS_date"
# colnames(temp.hh.cls.info)[colnames(temp.hh.cls.info) %in%
#                              c("CurrentLivingSituation")] <- "hh_CurrentLivingSituation"
# 
# 
# # join to a.enrollment
# 
# # a.enrollment <- left_join(a.enrollment, 
# #           temp.hh.cls.info)
# 
# a.enrollment <- temp.hh.cls.info
# 
# #a.enrollment$hh_CurrentLivingSituation %>% table(., useNA = "always")
# 
# # a.enrollment[a.enrollment$HouseholdID == "h_1153894" & 
# #                !is.na(a.enrollment$HouseholdID),
# #              c("HouseholdID",
# #                "PersonalID",
# #                "RelationshipToHoH",
# #                "hh_CurrentLivingSituation", 
# #                #"HoH_CLS_date", 
# #                "calc_CLS_date", 
# #                "calc_household_currentlivingsituation")]
# 
# 
# possible.hhids <- a.enrollment %>%
#   group_by(HouseholdID) %>%
#   summarise(n_ppl = n_distinct(PersonalID), 
#             n_hoh = sum(RelationshipToHoH == 1)) %>%
#   .[.$n_ppl > 1 & 
#       .$n_hoh == 1,] %>%
#   .$HouseholdID
# 
# 
# # a.enrollment[a.enrollment$HouseholdID %in% possible.hhids & 
# #                !is.na(a.enrollment$HouseholdID),
# #              c("HouseholdID",
# #                "PersonalID",
# #                "RelationshipToHoH",
# #                "hh_CurrentLivingSituation", 
# #                #"HoH_CLS_date", 
# #                "calc_CLS_date", 
# #                "calc_household_currentlivingsituation")] %>%
# #   group_by(NA_cls = is.na(hh_CurrentLivingSituation), 
# #            NA_clsdate = is.na(calc_CLS_date), 
# #            NA_hhcls = is.na(calc_household_currentlivingsituation)) %>%
# #   summarise(n=n())
# 
# # remove temp file
# rm(temp.hh.cls.info)
# END 2023-01-25 [CLS]

# Project Checks----
a.project <- read_csv("Project.csv")
a.project$provider_calc <- a.project$ProjectName
a.project$projectType_def <- unlist(lapply(a.project$ProjectType, fun_projtype))

# assign project_county
a.projectcoc$proj_county <- get.proj_county(proj_zip = a.projectcoc$ZIP, 
                                            proj_city = a.projectcoc$City)

# get_hh_calc_location_co_by_hoh----
# and implement get.calc_location_county()----

a.enrollment$calc_location_county <- NA
a.enrollment$calc_region <- NA

# # test
#  test.hhid <- "h_1138910"
#  test.pid  <- 1025651
#  test.eid <- a.enrollment$EnrollmentID[a.enrollment$HouseholdID == test.hhid]
# # /test
#  
# i <- which(a.enrollment$PersonalID == test.pid & 
#         a.enrollment$EnrollmentID == test.eid)[1]




#i <- sample(1:nrow(a.enrollment), size = 1)

# a.enrollment$calc_region %>% table(., useNA = "always")
# 
# left_join(a.enrollment[is.na(a.enrollment$calc_location_county),c("calc_location_county", "calc_region", "PersonalID", "ProjectID", "EnrollmentID")], 
#           a.project[,c("ProjectID", "ProjectName")]) %>%
#   .[!is.na(.$calc_region),]
# 
# a.enrollment[i,c("PersonalID", "EnrollmentID")]
 
for(i in 1:nrow(a.enrollment)){
  
  if(length(unique(a.project$HousingType[a.project$ProjectID == a.enrollment$ProjectID[i]])) > 1){
    stop(glue::glue("row {i} has multiple project.csv records associated with it"))
  }
  
  if(length(unique(a.projectcoc$proj_county[a.projectcoc$ProjectID == a.enrollment$ProjectID[i]])) > 1){
    stop(glue::glue("row {i} has multiple projectcoc.csv records associated with it"))
  }
  
  a.enrollment$calc_location_county[i] <- get.calc_location_county(housingtype = a.project$HousingType[a.project$ProjectID == 
                                                                                                         a.enrollment$ProjectID[i]], 
                                                                   proj.address.county = unique(a.projectcoc$proj_county[a.projectcoc$ProjectID == 
                                                                                                                           a.enrollment$ProjectID[i]]), 
                                                                   nccounty = a.enrollment$NCCounty[i])
  # if you don't get a county back (i.e. NA returned above)
  if(is.na(a.enrollment$calc_location_county[i])){
    # see if you can extract county from project name
    temp <- search_county.names(a.project$ProjectName[a.project$ProjectID %in% a.enrollment$ProjectID[i]]) %>%
      unique()
    
    if(length(temp) == 1 & 
       !is.na(temp)){
      a.enrollment$calc_location_county[i] <- temp
    }
    rm(temp)
  }
  
  # Regions
  a.enrollment$calc_region[i] <- get.calc_region(a.enrollment$calc_location_county[i])
  # if you don't get a region back (i.e. NA returned above)
  if(is.na(a.enrollment$calc_region[i])){
    # see if you can extract county from project name
    temp <- search_region.names(a.project$ProjectName[a.project$ProjectID %in% a.enrollment$ProjectID[i]]) %>%
      unique()
    
    if(length(temp) == 1 & 
       !is.na(temp)){
      a.enrollment$calc_region[i] <- temp
    }
    rm(temp)
  }
  
  
}



# a.enrollment %>%
#   group_by(NA_clc = is.na(calc_location_county), 
#            na_reg = is.na(calc_region)) %>%
#   #group_by(calc_location_county, calc_region) %>%
#   summarise(n = n())

# a.enrollment <- left_join(a.enrollment, 
#           read_csv("https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/regionscrosswalk.csv"),
#           by = c("calc_location_county" = "County")) 
# colnames(a.enrollment)[colnames(a.enrollment) == "Region"] <- "calc_region_join"
# 
# a.enrollment %>%
#   group_by(calc_location_county, calc_region, calc_region_join) %>%
#   summarise(n = n()) %>%
#   .[!complete.cases(.),]


a.enrollment$calc_location_county_flag <- NA
for(i in unique(a.enrollment$HouseholdID)){
  
  n_hh_pids <- length(unique(a.enrollment[a.enrollment$HouseholdID == i,]$PersonalID))
  n_hh_hoh  <- sum(a.enrollment[a.enrollment$HouseholdID == i,]$RelationshipToHoH == 1)
  hoh_loc_co <- a.enrollment[a.enrollment$HouseholdID == i & 
                               a.enrollment$RelationshipToHoH == 1,]$calc_location_county
  
  # notes for flag
  flag1  <- ifelse(n_hh_pids == 1, "Only 1 person in HH.", "")
  flag2a <- ifelse(n_hh_hoh < 1, "No HoHs in HH.", "")
  flag2b <- ifelse(n_hh_hoh > 1, "Multiple HoHs in HH.", "")
  flag3  <- ifelse(any(is.na(hoh_loc_co)), "Non-valid calc_location_county.", "")
  
  a.enrollment$calc_location_county_flag[a.enrollment$HouseholdID == i] <- paste(#flag1, 
    flag2a, flag2b, flag3, 
    sep = " ", collapse = " ") %>%
    trimws() %>% 
    gsub(" {1,}", " ", .)
  
  if(all(c(#flag1,
    flag2a,flag2b,flag3) == "")){
    a.enrollment$calc_location_county_flag[a.enrollment$HouseholdID == i] <- "no flags"
  }
  rm(flag1,flag2a,flag2b,flag3,n_hh_pids,n_hh_hoh,hoh_loc_co)
  
}

# dropping in some code developed from "nccounty_logic.R"




# Disabilities Check----
a.disabilities <- read_csv("Disabilities.csv")

a.disabilities$InformationDate_disab <- a.disabilities$InformationDate

screened.pos.disab_df <- screened_positive_disability(dis_df = a.disabilities, enr_df = a.enrollment, exit_df = a.exit)


# Healthanddv check----
a.healthanddv <- read_csv("HealthAndDV.csv")

a.healthanddv$domesticViolenceVictim_def <- unlist(lapply(a.healthanddv$DomesticViolenceVictim,
                                                          fun_1.8_def))
a.healthanddv$currentlyFleeingDV_def <- unlist(lapply(a.healthanddv$CurrentlyFleeing, fun_1.8_def))

# FLAG - DV----
a.healthanddv$flag_dv <- is.na(a.healthanddv$domesticViolenceVictim_def) | 
  is.na(a.healthanddv$currentlyFleeingDV_def)

# a.healthanddv[a.healthanddv$PersonalID %in% c(1019681), 
#               c("domesticViolenceVictim_def", 
#                 "currentlyFleeingDV_def", 
#                 "flag_dv", "InformationDate")]

# Inventory check----
a.inventory <- read_csv("Inventory.csv")
a.inventory$householdType_def <- unlist(lapply(a.inventory$HouseholdType, fun_hhtype))

# Output files, pre-join----

flag_colnames(a.client)
b.client <- a.client[,c("PersonalID", "age_calc", "DOBDataQuality_def",
                        "hud_age_calc", "gender_calc", "race_calc", 
                        "ethnicity_def", "vetStatus_def", 
                        flag_colnames(a.client))]

colnames(a.enrollment)


b.enrollment <- a.enrollment[colnames(a.enrollment) %in% c(grep("_def$|_calc$", colnames(a.enrollment), 
                                                ignore.case = F, value = T), 
                                           "EnrollmentID", "PersonalID", "ProjectID",
                                           "NCCounty",
                                           "HouseholdID", "HoH_PersonalID", 
                                           "HoH_currentLivingSituation_def",
                                           "HoH_CLS_date", "livingSituation_def", 
                                           "relationshiptohoh_def", 
                                           "EntryDate", 
                                           "calc_household_currentlivingsituation",
                                           "calc_location_county", 
                                           "calc_region",
                                           "calc_location_county_flag",
                                           flag_colnames(a.enrollment))]

colnames(a.currentlivingsituation)

b.currentlivingsituation <-  a.currentlivingsituation[colnames(a.currentlivingsituation) %in% 
                                                        c(grep("_def$|_calc$", colnames(a.currentlivingsituation), 
                                                ignore.case = F, value = T), 
                                           "EnrollmentID", "PersonalID", "ProjectID", 
                                           "HouseholdID", "HoH_PersonalID", 
                                           "InformationDate", "currentLivingSituation_def", 
                                           "currentLivingSituation.Date_calc", 
                                           flag_colnames(a.currentlivingsituation))]

colnames(a.exit)
b.exit <- a.exit[colnames(a.exit) %in% c(grep("_def$|_calc$", colnames(a.exit), 
                                                ignore.case = F, value = T), 
                                           "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID", 
                               "ExitID", "ExitDate", 
                               flag_colnames(a.exit))]

colnames(a.project)
b.project <- a.project[colnames(a.project) %in% c(grep("_def$|_calc$", colnames(a.project), 
                                                ignore.case = F, value = T), 
                                           "ProjectID", "OrganizationID", 
                                           "projectType_def", 
                                           "ProjectName", 
                                           "provider_calc",
                                           flag_colnames(a.project))]

colnames(a.projectcoc)
b.projectcoc <- a.projectcoc[colnames(a.projectcoc) %in% c(grep("_def$|_calc$", colnames(a.projectcoc), 
                                                ignore.case = F, value = T), 
                                           "ProjectCoCID", "ProjectID", 
                                           #"City", "ZIP", "CoCCode", 
                                           "County", "Region", 
                                           "NCCounty", "HoH_PersonalID", "ProjectName", 
                                           "proj_county",
                                           flag_colnames(a.projectcoc))]

screened.pos.disab_df


colnames(a.healthanddv)
b.healthanddv <- a.healthanddv[colnames(a.healthanddv) %in% c(grep("_def$|_calc$", colnames(a.healthanddv), 
                                    ignore.case = F, value = T), 
                               "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID", 
                               "ExitID", "ExitDate", 
                               flag_colnames(a.healthanddv))]

colnames(a.inventory)
b.inventory <- a.inventory[colnames(a.inventory) %in% c(grep("_def$|_calc$", colnames(a.inventory), 
                                                  ignore.case = F, value = T), 
                                             "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID", 
                                             "ExitID", "ExitDate", "CoCCode", 
                                             flag_colnames(a.inventory))]


# keep only these enrollment_ids----
#what is this???
c.enrollment             <- inner_join(b.enrollment, pit.eids)
c.exit                   <- inner_join(b.exit, pit.eids)
c.currentlivingsituation <- inner_join(b.currentlivingsituation, pit.eids)
c.client                 <- b.client[b.client$PersonalID %in% c.enrollment$PersonalID,]
c.screened.pos.disab_df  <- screened.pos.disab_df[screened.pos.disab_df$EnrollmentID %in% 
                                                    pit.eids$EnrollmentID,]
c.healthanddv            <- b.healthanddv[b.healthanddv$EnrollmentID %in%
                                            pit.eids$EnrollmentID,]

colnames(c.currentlivingsituation)
# get last Information Date prior to or on pit night

d.currentlivingsituation <- c.currentlivingsituation %>%
  .[.$InformationDate <= pit.night & 
      .$currentLivingSituation.Date_calc <= pit.night,] %>%
  group_by(EnrollmentID, InformationDate) %>%
  slice_max(., order_by= currentLivingSituation.Date_calc, 
            n = 1) %>%
  ungroup() %>%
  group_by(EnrollmentID) %>%
  slice_max(., 
            order_by = InformationDate, n = 1)


b.projectcoc

c.enrollment %>% colnames
c.exit
c.currentlivingsituation
b.project


comp_county <- inner_join(c.enrollment[,c("EnrollmentID", "ProjectID", "NCCounty",
                                          flag_colnames(c.enrollment))],
           b.projectcoc[,c("ProjectID", "County", flag_colnames(b.projectcoc))]) #%>%
  #mutate(., county_matches = ifelse(NCCounty == County, T, F)) %>%
  #mutate(., county_matches = ifelse(is.na(county_matches), F, county_matches))

#rm(comp_county)

ls(pattern = "^a\\.") %>%
  .[! . %in% c("a.client", "a.enrollment", "a.exit", 
               "a.project", "a.projectcoc", "a.disabilities", 
               "a.healthanddv")]


output <- left_join(c.enrollment, c.client) %>%
  #left_join(., c.exit) %>%
  left_join(., comp_county) %>%
  left_join(., b.project) %>%
  left_join(., screened.pos.disab_df) %>%
  left_join(., c.healthanddv) %>%
  #left_join(., d.currentlivingsituation) %>%
  left_join(., b.inventory[,c("ProjectID", 
                              "householdType_def")] ) %>%
  left_join(., b.projectcoc)
# output %>%
#   group_by(EnrollmentID) %>%
#   summarise(n = n()) %>%
#   .$n %>% table()

## new changes ----
# date_in_pitweek <- function(a.date){
#   # date stuff
#   
#   #  within a date range: PIT Night plus PIT week. Nicole has requested (to me) that we expand this to
#   #  dates of 1/22/23 - 2/4/23. For this filter, 
#   if(Sys.Date() < ymd(20230122)){
#     pit.date <- ymd(20220126)
#     pit.week <- ymd(20220122):ymd(20220204)
#     pit.week <- as_date(pit.week)
#   }else{
#     pit.date <- ymd(20230126)
#     pit.week <- ymd(20220123):ymd(20230204)
#     pit.week <- as_date(pit.week)
#   }
#   
#   # informationdate of cls is in pit.week
#   return(a.date %in% pit.week)
# }
# 
# hh_nmfh <- function(hhid = NULL, pid = NULL, 
#                     df_enr = a.enrollment, 
#                     df_cls = a.currentlivingsituation){
#   
# #  1. There must be a filter that narrows Andrea's output to households with
# #  Current Living Situation = Place not mean for habitation 
# 
# 
#   
# #  if a HOH has CLS meeting these
# #  parameters, apply it to the rest of the household.
#   
#   require(glue)
#   require(dplyr)
#   require(lubridate)
#   
#   # error check
#   if(is.null(hhid) & is.null(pid)){
#     # fails error check (1)
#     stop("[must provide one of 'hhid' or 'pid']")
#   }else if(!is.null(hhid) | !is.null(pid)){
#     # passes error check - main body here
#     # get hhid if null
#     if(is.null(hhid)){
#       hhid <- df_enr$HouseholdID[df_enr$PersonalID == pid]
#     }
#     # get hoh_personalID
#     hoh_personalID <- df_enr$PersonalID[df_enr$HouseholdID == hhid & 
#                                           df_enr$RelationshipToHoH == 1]
#         # get hh_enrollmentID
#     hh_enrollmentID <- df_enr$EnrollmentID[df_enr$HouseholdID == hhid]
#     
#     if(length(hoh_personalID) != 1){
#       # if there is
#       out <- "Flag - incorrect number of HeadOfHouseholds"
#     }else{
#       
#       # handle possible multiple-record duplicates due to data changes that are not 
#       # related to these values. 
#       
#       df_cls <- df_cls %>%
#         .[.$PersonalID %in% hoh_personalID & 
#             .$EnrollmentID %in% hh_enrollmentID,] %>%
#         .[!duplicated(.),]
#       
#      # df_cls <- df_cls %>%
#      #    .[.$PersonalID %in% hoh_personalID & 
#      #        .$EnrollmentID %in% hh_enrollmentID,] %>%
#      #    group_by(PersonalID, 
#      #             EnrollmentID, 
#      #             InformationDate, 
#      #             CurrentLivingSituation) %>%
#      #    summarise(n = n())
#       
#       # get currentlivingsituation of hoh_personalID
#       out <- (df_cls$CurrentLivingSituation[df_cls$PersonalID == hoh_personalID] == 16) & 
#         !is.na(df_cls$CurrentLivingSituation[df_cls$PersonalID == hoh_personalID])
#     }
#     
#     
#   }else {
#     # fails error check (2)
#     stop("[logic did not account for input paramter combination]")
#   }
#   
#   if(length(out) == 0 ){
#     out <- "no solution found (NA)"
#   }
#   
#   if(length(out) > 1){
#     out <- "multiple solutions found (NA)"
#   }
#   
#   # return output
#   return(out)
# }
# 
# # a.pid <- sample(a.enrollment$PersonalID, size = 1)
# # a.hhid <- sample(a.enrollment$HouseholdID, size = 1)
# # 
# # 
# # # error check for hhids that return multiples in function
# # some.hhids <- replicate(n = 200,
# #                         sample(a.enrollment$HouseholdID,
# #                                size = 1));x <- lapply(X = some.hhids, 
# #                                                       FUN = hh_nmfh) %>%
# #   lapply(X = ., FUN = length) %>% 
# #   unlist(); any(x>1)
# # 
# # some.hhids[which(x>1)]
# # 
# # df_cls[df_cls$PersonalID == hoh_personalID,] %>%
# #   group_by(PersonalID, EnrollmentID, InformationDate) %>%
# #   summarise(n_rows = n())
# 
# check.hhid <- c("s_1147220", "h_1145388", "s_1153561", "h_1141007", "s_1147473", "s_1111647")
# 
# 
# 
# a.enrollment$tempA <- NA
# a.enrollment$tempB <- NA
# a.enrollment$test_cls_tim <- NA
# 
# #n <- 0
# 
# for(i in 1:nrow(a.enrollment)){
#   # n <- n + 1
#   # if ( n > 100){
#   #   break
#   # }
#   
#   tempa <- hh_nmfh(hhid = a.enrollment$HouseholdID[i], pid = a.enrollment$PersonalID[i], 
#                    df_enr = a.enrollment,
#                    df_cls = a.currentlivingsituation)
#   
#   tempb <- date_in_pitweek(a.date = a.currentlivingsituation$InformationDate[which(a.currentlivingsituation$PersonalID %in% 
#                                                                                      a.enrollment$PersonalID[i])])
#   tempa <- ifelse(length(tempa) == 0, NA, tempa)
#   tempa <- ifelse(grepl("\\(NA\\)$|^Flag", tempa), F, tempa)
#   tempb <- ifelse(length(tempb) == 0, NA, tempb)
#   
#   a.enrollment$tempA[i] <- tempa
#   a.enrollment$tempB[i] <- tempb
#   
#   a.enrollment$test_cls_tim[i] <- (tempa & tempb) | (tempa & is.na(tempb))
#   
#   
#   # try(a.enrollment$test_cls_tim[a.enrollment$HouseholdID == i] <- 
#   #   as.logical(hh_nmfh(hhid = i, pid = NULL, 
#   #           df_enr = a.enrollment,
#   #            df_cls = a.currentlivingsituation)) & 
#   #   date_in_pitweek(a.date = a.currentlivingsituation$InformationDate[which(a.currentlivingsituation$PersonalID %in% 
#   #                                                                       a.enrollment$PersonalID[a.enrollment$HouseholdID %in% i])]))
# }
# 
# 
# 
# a.enrollment %>%
#   group_by(tempA, tempB) %>%
#   summarise(n = n())
# 
# 
# full_join(a.enrollment, 
#           a.currentlivingsituation, 
#           by = c("EnrollmentID", "PersonalID")) %>% 
#   .[,c("EnrollmentID", "PersonalID", "HouseholdID", "CurrentLivingSituation", "InformationDate")] %>%
#   group_by(NA_eid = is.na(EnrollmentID),
#            NA_pid = is.na(PersonalID), 
#            NA_hhid = is.na(HouseholdID),
#            NA_infoDate = is.na(InformationDate),
#            NA_cls = is.na(CurrentLivingSituation)) %>%
#   summarise(total = n()) %>%
#   ungroup() %>%
#   mutate(., 
#          pct_tot = scales::percent(total / sum(total)))
# 
# 
# # / error






# reorder to meet andrea's specs----
"https://ncceh.sharepoint.com/:x:/s/DataCenter/EdQERAgSu5pGsBcN5VNGD20B3qlfQ7iOCFz9BPJi2xoADQ?e=zOvaac"

out.cn <- colnames(output)

grep("cls", out.cn, ignore.case = T, value = T)

output$gender_category_calc <- NA
output$race2_calc <- NA
output$race_cat_calc <- NA
output$CH <- NA
output$youth_type_hh <- NA
output$veteran_type_hh <- NA

grep("county|calc", colnames(output), value = T, ignore.case = T)

output2 <- output[,c("PersonalID", 
                     "reltionshiptohoh_def", 
                     #"HouseholdID", 
                     "vetStatus_def", 
                     "calc_location_county", 
                     "calc_location_county_flag",
                     "calc_region",
                     "proj_county",
                     "age_calc", 
                     "DOBDataQuality_def",
                     "hud_age_calc", 
                     "gender_calc", "gender_category_calc", 
                     "race_calc", "race2_calc", "race_cat_calc", 
                     "ethnicity_def", 
                     "HIV.AIDS", 
                     "chronic_hlth_C", 
                     "developmental_D", 
                     "mental_health_D", 
                     "physical_D", 
                     "substance_use_D", 
                     "provider_calc", 
                     "Region", 
                     #"County", 
                     "NCCounty", 
                     #"county_matches", 
                     "CH", 
                     "domesticViolenceVictim_def", 
                     "currentlyFleeingDV_def",
                     "householdType_def", 
                     "youth_type_hh", "veteran_type_hh", 
                     "livingSituation_def",
                     #'HoH_CLS_date',
                     "HoH_PersonalID", 
                     #"HoH_currentLivingSituation_def", 
                     "EnrollmentID", 
                     "ProjectName", 
                     "EntryDate", 
                     flag_colnames(output))]

output2 <- output2[!colnames(output2) %in% c("Region")]


grep("calc", colnames(output2), value = T, ignore.case = T)




out.name.andrea <- glue("andrea_output{Sys.Date()}_HR{hour(Sys.time())}.xlsx")
write.xlsx(x = output2, 
           file = out.name.andrea)

# identify data issues----
colnames(output2) %>%
  .[!. %in% c("age_calc", "DOBDataQuality_def", 
              #"Region", 
              "calc_region",
              "hud_age_calc",
              "NCCounty", "County", "county_matches", 
              "domesticViolenceVictim_def", 
              "currentlyFleeingDV_def")]

grep("type", 
     colnames(output2), value = T, ignore.case = T)


# issue - HOH living situation vs Person livins situation
output2


# issue - fleeing but not domestic violence victim

output2$domesticViolenceVictim_def %>% unique()
output2$currentlyFleeingDV_def %>% unique()

output2$issue_dv.victim_vs_dv.fleeing <- (output2$domesticViolenceVictim_def %in%
                                            c("Client refused", "No") & 
  output2$currentlyFleeingDV_def %in% c("Yes", "Client doesn't Know"))

# issue - region
#output2$issue_region <- is.na(output2$Region)
#output2$issue_nccounty <- is.na(output2$county_matches) | !output2$county_matches 


# DOB vs DOB Data Quality
output2$issue.DOB_vs_DOBDataQuality <- F
output2[output2$DOBDataQuality_def %in% c("Client refused", "Data not collected") & 
  !is.na(output2$age_calc),]$issue.DOB_vs_DOBDataQuality <- T


output2[is.na(output2$age_calc) & 
  output2$DOBDataQuality_def %in% 
  c("Full DOB reported", "Approximate or partial DOB reported"),]$issue.DOB_vs_DOBDataQuality <- T
  

output2$issue_race <- output2$race_calc == "[unknown]"
output2$householdType_def %>% unique()

output2$livingSituation_def %>% unique()
output2$reltionshiptohoh_def

output2$issue_no_HeadOfHousehold <- is.na(output2$PersonalID == output2$HoH_PersonalID & 
  output2$reltionshiptohoh_def != "Self (head of household)")



# for(i in 1:ncol(output2)){
#   if(any(grepl("^\\[|unknown|undetermined", unname(unlist(output2[,i])), ignore.case = T))|
#      any(is.na(unname(unlist(output2[,2]))))){
#     print(i)
#   }
# }
# output2[,10]




colnames(output) %>%
  grep("date", ., ignore.case = T, value = T)

output3 <- output2 %>%
  group_by(client_id = PersonalID, 
           EnrollmentID, # THIS NEEDS TO BE REMOVED
           ProjectName,
           EntryDate,
           #calc_location_county_flag,
           calc_location_county,
           calc_region,
           #proj_county,
           issue_dv.victim_vs_dv.fleeing,
           # issue_region, 
           # issue_nccounty, 
           issue.DOB_vs_DOBDataQuality, 
           issue_race, 
           issue_no_HeadOfHousehold, 
           #Region,
           #County, 
           #NCCounty,
           flag.nccounty_na, 
           flag.reltohoh_na, 
           flag.child_hoh, 
           flag.age_too_old, 
           flag.DOB_na, 
           flag.gender, 
           flag.race, 
           flag.ethnicity, 
           flag.vetstatus, 
           flag_dv) %>%
  summarise() %>%
  # left_join(., 
  #           read_csv("regionscrosswalk.csv"), 
  #           by = c("County" )) %>%
  as.data.table() %>%
  melt(., 
        id.vars = c("EnrollmentID", "client_id", "ProjectName", "EntryDate", "calc_location_county", "calc_region") , 
       variable.name = "DQ_flag_type") %>%
  #.$calc_location_county %>% is.na() %>% table()
  as.data.frame() %>%
  as_tibble() %>%
  .[.$value == T,] %>%
  .[!colnames(.) %in% c("value")]

# left_join(output3[!colnames(output3) %in% c("calc_location_county", "calc_region")],
# output2[,c("EnrollmentID", "PersonalID", "calc_location_county", "calc_region")],
# by = c("client_id" = "PersonalID", "EnrollmentID")) %>%
#   group_by(na_co = is.na(calc_location_county), 
#            na_reg = is.na(calc_region)) %>%
#   summarise(n = n())


output3 <- output3[grepl("^flag", output3$DQ_flag_type, ignore.case = T),]

# narrow donw to just 1 field with a county in it
grep("county", colnames(output3), value = T, ignore.case = T)

output3 <- output3[!colnames(output3) %in% c("proj_county")]
#output[output$PersonalID == 2664,]$Region


# change names----
output3$DQ_flag_type <- output3$DQ_flag_type  %>% as.character()

unique(output3$DQ_flag_type)

output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.nccounty_na", 
       "verify NCCounty", output3$DQ_flag_type)
output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.child_hoh", 
       "Head Of Household aged 16 or under", output3$DQ_flag_type)
output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.age_too_old", 
       "older than 80 years old", output3$DQ_flag_type)
output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.DOB_na", 
       "missing Date of Birth or DOB_quality", output3$DQ_flag_type)
output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.race", 
       "missing race", output3$DQ_flag_type)

colnames(output3)

# output3 <- left_join(output3, a.enrollment[,c("EnrollmentID", "NCCounty")]) %>%
#   left_join(., 
#             a.project[,c("ProjectName", "ProjectID")]) %>%
#   left_join(., 
#             a.projectcoc[,c("ProjectID", "County")]) %>%
#   left_join(., 
#             read_csv("regionscrosswalk.csv")) %>%
#   .[!colnames(.) %in% c("ProjectID")] %>% colnames()
# 
# 
# colnames(output3)
# 
# output3 <- output3[!colnames(output3) %in% c("County", "NCCounty", "Region")]

#colnames(output3)[colnames(output3) == "County"] <- "project_county"

# spot check nicole to andrea
spot.check3 <- output3[sample(1:nrow(output3), size = 10),]
spot.check2 <- output2 %>%
  .[.$EnrollmentID %in% spot.check3$EnrollmentID & 
      .$PersonalID %in% spot.check3$client_id,] %>% 
  group_by(EnrollmentID, 
           PersonalID, calc_location_county, 
           calc_region) %>%
  summarise(n = n())

# nicole changes----
# remove enrolllmentID
output3 <- output3[!colnames(output3) %in% c("EnrollmentID")]






# write to file----
getwd()
library(glue)

grep("calc", colnames(output3), value = T, ignore.case = T)


#stop("notes:  1) remove enrollment_ID\n2) one county column,\n3)??? see chat")


out.name <- glue("nicole_output{Sys.Date()}_HR{hour(Sys.time())}.xlsx")
write.xlsx(x = output3, 
           file = out.name)


library(readxl)
read_xlsx(list.files(pattern = "^andrea_output2023-01-19")) %>%
  colnames() %>%
  grepl(pattern = "calc", x = ., 
       #value = T, 
       ignore.case = T) %>%
  which()
