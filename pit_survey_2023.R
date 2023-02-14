
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

# re-run 2023 pit data every thursday morning until 3/31/2023

thurs.hmis.pulls.complete <- ymd(c(20230209)) # update this after you pull and export new data each thursday

if(as.character(lubridate::wday(Sys.Date(),label=T,abbr=F))=="Thursday" & 
   !Sys.Date() %in% thurs.hmis.pulls.complete){
  # build hmis search: 
# BoS FY 2022 unsheltered whole CoC reporting group (2504)
# Unsheltered PIT Custom CSV 1/22/23 - 2/4/23 (For Tim!)
# 1/22/2023 - 2/04/2023
  print("https://app.smartsheet.com/sheets/9gH67xJw5FXM2FvWr5j9MmJWqX53qp5qXPcQ7V51?view=grid")
  stop("Upload new export Today") # stops the code unless you've uploaded weeklies
}

# NOTE----
print("For hud pit survey for the night of Jan 26th, Entered on January 26th, Exited on Jaunary 27th")

pit.night     <- ymd(20230125) #20230125 or 20220126
pit.week_start <- pit.night %m+% days(1) #ymd(20220127) #20230126
pit.week_end   <- pit.night %m+% days(7) #ymd(20220222) #20230221

# /NOTE----


# resources----
out.template <- "https://ncceh.sharepoint.com/:x:/s/DataCenter/EdQERAgSu5pGsBcN5VNGD20B3qlfQ7iOCFz9BPJi2xoADQ?e=zOvaac"

# Setup----
csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/test_data"
csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data"

# Functions----
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/pit_survey_calculations.R?raw=TRUE")


# const_nchar <- function(x = 1:130){
#   #formats numbers for use in column fields so that they can be sorted as
#   #strings and remain in order
#   x <- as.character(x)
#   max.char <- max(nchar(x))
#   out <- list()
#   for(i in 1:length(x)){
#     out[[i]] <- paste(c(rep("0", max.char - nchar(x[i])), x[i]), sep = "", collapse = "")
#   }
#   out <- unlist(out)
#   return(out)
# }

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

# bind_file <- function(filename, 
#                       dir1 = "bos_2022", dir2 = "dur_2022", dir3 = "ora_2022", 
#                       dir.top = "C:/Users/TimBender/Documents/R/ncceh/data/coc_by_ffy"){
#   require(glue)
#   require(readr)
#   f1 <- glue("{dir.top}/{dir1}/{filename}")
#   f2 <- glue("{dir.top}/{dir2}/{filename}")
#   f3 <- glue("{dir.top}/{dir3}/{filename}")
#   
#   return(rbind(read_csv(f1), read_csv(f2), read_csv(f3)))
# }

# find_cols <- function(patrn = "ID$", ic = T,
#                       l = file.colnames){
#   out <- list()
#   for(i in 1:length(l)){
#     out[[names(l[i])]] <- grep(pattern = patrn,
#                                x = l[[i]],
#                                ignore.case = ic,
#                                value = T)
#   }
#   # remove files with no results returned
#   out <- out[lapply(out, length) > 0]
#   return(out)
# }



# Working Directory Setup----
setwd(csv.file.dir)
getwd()

# # Identify the columns that I need to join----
# # List all files
# csv.files <- list.files(pattern = "\\.csv$") 
# csv.files <- csv.files[!csv.files %in% c("Client2.csv", "report_phoneemail.csv")]
# 
# # # # build list of colnames
# # file.colnames <- list()
# # for(i in csv.files){
# #   file.colnames[[i]] <- colnames(read_csv(i))
# # }

# Client Checks----
a.client <- read_csv("Client.csv")

# pii hash check
a.rando.key <- paste(sample(c(letters,LETTERS,0:9), size = 100,replace = T),sep="",collapse="")

if(sum(is_hashed(a.client$SSN),na.rm = T)== 0){
  a.client$SSN <- openssl::sha256(x = as.character(a.client$SSN), key = a.rando.key)
  write_csv(x = a.client, 
            file = "Client.csv")
}

if(sum(is_hashed(a.client$FirstName),na.rm = T)== 0){
  a.client$FirstName <- openssl::sha256(x = as.character(a.client$FirstName), key = a.rando.key)
  write_csv(x = a.client, 
            file = "Client.csv")
}

if(sum(is_hashed(a.client$LastName),na.rm = T)== 0){
  a.client$LastName <- openssl::sha256(x = as.character(a.client$LastName), key = a.rando.key)
  write_csv(x = a.client, 
            file = "Client.csv")
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


# full_join(a.projectcoc2,a.enrollment2, by = "ProjectID") %>%
#   group_by(#County,NCCounty,
#            Region.x, Region.x == Region.y) %>%
#   summarise(n = n())



rm(zip_co.cw)

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

# TO-DO----

# TO DO:  (AS OF 2-10-2023): THERE IS AN ISSUE WITH THE OUTPUT2A (ANDREA'S
# OUTPUT) GENEREATING A REALLY SMALL # OF UNSHELETERED PEOPLE ON PIT NIGHT (LIKE
# 25) WHEN A LARGER NUMBER IS EXPECTED (I.E. 867).  THE RAW DATA ENROLLMENT.CSV
# AT THE INDIVIDUAL LEVEL SHOWS CLOSER TO 867 WHEN CALCULATED FROM SCRATCH
# OUTSIDE OF THIS CODE, SO WE NEED TO TRACK DOWN WHERE THE PROBLEM IS HAPPENING.
# SIDE-NOTE: LOOK INTO DATE FILTERS BECAUSE THE HMIS EXPORT IS DOING THAT
# AUTOMATICALLY, AND ANY DATE FILTERS IN HERE MAY BE GUMMING UP THE WORKS.
# Related to hh_cls


a.currentlivingsituation <- read_csv("CurrentLivingSituation.csv")

out.cls <- calc_CLS_final(a.enr = a.enrollment, 
                          a.cls = a.currentlivingsituation)

# the number below should be roughly equal to the smartsheet total figure: 
out.cls %>%
  group_by(HouseholdID, hh_cls, 
           hh_cls_infodate) %>%
  summarise(n_pid = n_distinct(PersonalID)) %>%
  .[.$hh_cls == 16 & 
      !is.na(.$hh_cls),] %>%
  .[!is.na(.$hh_cls_infodate) & 
      .$hh_cls_infodate == pit.night,] %>%
  .$n_pid %>% sum
  

# TO-DO: (2-10-2020)----
# somewhere individual CurrentLivingSituation is being over-written bc/ of andrea pivot table in chat on 2/10/23 at 12:00 noon
out.cls$currentLivingSituation_def <- unlist(lapply(out.cls$CurrentLivingSituation,
                                                    fun_livingsituation_def))
out.cls$InformationDate_cls <- out.cls$InformationDate
out.cls <- out.cls[!colnames(out.cls) %in% 
                     c("CurrentLivingSituation", "InformationDate")]

a.enrollment <- left_join(a.enrollment, 
                          out.cls)
# the number below should be roughly the same as the total number in the smartsheet tracker:
a.enrollment %>%
  group_by(HouseholdID, hh_cls, 
           hh_cls_infodate) %>%
  summarise(n_pid = n_distinct(PersonalID)) %>%
  .[.$hh_cls == 16 & 
      !is.na(.$hh_cls),] %>%
  .[!is.na(.$hh_cls_infodate) & 
      .$hh_cls_infodate == pit.night,] %>%
  .$n_pid %>% sum

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

# the number below should be roughly the same as the total number in the smartsheet tracker:
a.enrollment %>%
  group_by(HouseholdID, hh_cls, 
           hh_cls_infodate) %>%
  summarise(n_pid = n_distinct(PersonalID)) %>%
  .[.$hh_cls == 16 & 
      !is.na(.$hh_cls),] %>%
  .[!is.na(.$hh_cls_infodate) & 
      .$hh_cls_infodate == pit.night,] %>%
  .$n_pid %>% sum

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

# the number below should be roughly the same as the total number in the smartsheet tracker:
a.enrollment %>%
  group_by(HouseholdID, hh_cls, 
           hh_cls_infodate) %>%
  summarise(n_pid = n_distinct(PersonalID)) %>%
  .[.$hh_cls == 16 & 
      !is.na(.$hh_cls),] %>%
  .[!is.na(.$hh_cls_infodate) & 
      .$hh_cls_infodate == pit.night,] %>%
  .$n_pid %>% sum


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


# Inventory check----
a.inventory <- read_csv("Inventory.csv")
a.inventory$householdType_def <- unlist(lapply(a.inventory$HouseholdType, fun_hhtype))


# Output files, pre-join----

flag_colnames(a.client)

b.client <- a.client[,c("PersonalID", "age_calc", "DOBDataQuality_def",
                        "hud_age_calc", "gender_calc", "race_calc", 
                        "ethnicity_def", "vetStatus_def", 
                        flag_colnames(a.client))]

colnames(a.enrollment) %>%
  .[order(.)] %>%
  grep(pattern = "^calc_|^hh_cls|^HoH_", 
       ., value = T, ignore.case = F)

b.enrollment <- a.enrollment[colnames(a.enrollment) %in% 
                               c(grep("_def$|_calc$", colnames(a.enrollment), 
                                      ignore.case = F, value = T), 
                                 "EnrollmentID", "PersonalID", "ProjectID",
                                 "NCCounty",
                                 "HouseholdID", "HoH_PersonalID", 
                                 "HoH_currentLivingSituation_def",
                                 "HoH_CLS_date", #"livingSituation_def", 
                                 "relationshiptohoh_def", 
                                 "EntryDate", 
                                 "calc_household_currentlivingsituation",
                                 "calc_location_county", 
                                 "calc_region",
                                 "calc_location_county_flag",
                                 "hh_cls", "hh_cls_infodate", 
                                 "HoH_PersonalID",
                                 "InformationDate_cls", #"InformationDate",
                                 "currentLivingSituation_def", #"CurrentLivingSituation",
                                 flag_colnames(a.enrollment))]

# the number below should be roughly the same as the total number in the smartsheet tracker:
b.enrollment %>%
  group_by(HouseholdID, hh_cls, 
           hh_cls_infodate) %>%
  summarise(n_pid = n_distinct(PersonalID)) %>%
  .[.$hh_cls == 16 & 
      !is.na(.$hh_cls),] %>%
  .[!is.na(.$hh_cls_infodate) & 
      .$hh_cls_infodate == pit.night,] %>%
  .$n_pid %>% sum

colnames(a.currentlivingsituation)
grep("_def|CurrentLivingSituation", colnames(a.enrollment), value = T, ignore.case = T)


# this is fine VV.  it doesn't get joined to anything else
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
b.healthanddv <- a.healthanddv[colnames(a.healthanddv) %in% 
                                 c(grep("_def$|_calc$", colnames(a.healthanddv), 
                                        ignore.case = F, value = T), 
                                   "EnrollmentID", "PersonalID", "ProjectID",
                                   "HouseholdID", "HoH_PersonalID", 
                                   "ExitID", "ExitDate", 
                                   flag_colnames(a.healthanddv))]

colnames(a.inventory)
b.inventory <- a.inventory[colnames(a.inventory) %in% 
                             c(grep("_def$|_calc$", colnames(a.inventory), 
                                    ignore.case = F, value = T), 
                               "EnrollmentID", "PersonalID", "ProjectID", 
                               "HouseholdID", "HoH_PersonalID", 
                               "ExitID", "ExitDate", "CoCCode", 
                               flag_colnames(a.inventory))]

# keep only these enrollment_ids----

# 2023-01-25: NOTE: This vvv is where we deploy the pit night filter via eids. should this be by hhid instead?---- 

#pit.eids----

# this is where the ERROR is happening (20230214)----

# no need to join to pit.eids because that is a table that attempts to filter
# down to pit-night but the dataset already does that implicitly.

#c.enrollment             <- inner_join(b.enrollment, pit.eids) 
c.enrollment              <- b.enrollment

# the number below should be roughly the same as the total number in the smartsheet tracker:
c.enrollment %>%
  group_by(HouseholdID, hh_cls, 
           hh_cls_infodate) %>%
  summarise(n_pid = n_distinct(PersonalID)) %>%
  .[.$hh_cls == 16 & 
      !is.na(.$hh_cls),] %>%
  .[!is.na(.$hh_cls_infodate) & 
      .$hh_cls_infodate == pit.night,] %>%
  .$n_pid %>% sum
# it's not

# what is pit.eids? 



#c.exit                   <- inner_join(b.exit, pit.eids)
c.exit                    <- b.exit
#c.currentlivingsituation <- inner_join(b.currentlivingsituation, pit.eids)
c.currentlivingsituation <- b.currentlivingsituation
#c.client                 <- b.client[b.client$PersonalID %in% c.enrollment$PersonalID,]
c.client                  <- b.client
# c.screened.pos.disab_df  <- screened.pos.disab_df[screened.pos.disab_df$EnrollmentID %in% 
#                                                     pit.eids$EnrollmentID,]
c.screened.pos.disab_df  <- screened.pos.disab_df

# c.healthanddv            <- b.healthanddv[b.healthanddv$EnrollmentID %in%
#                                             pit.eids$EnrollmentID,]
c.healthanddv            <- b.healthanddv


# 2023-01-25 NOTE: works up to this point----

colnames(c.currentlivingsituation) %>% grep("^hh|^hoh|date", ., value = T, ignore.case = T)
# get last Information Date prior to or on pit night

b.projectcoc

c.enrollment %>% colnames
c.exit
c.currentlivingsituation
b.project


comp_county <- inner_join(c.enrollment[,c("EnrollmentID", "ProjectID", "NCCounty",
                                          flag_colnames(c.enrollment))],
           b.projectcoc[,c("ProjectID", "County", flag_colnames(b.projectcoc))]) 

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
                     #'HoH_CLS_date',
                     "HoH_PersonalID", 
                     "hh_cls"   ,
                     "hh_cls_infodate" ,
                     #"HoH_currentLivingSituation_def", 
                     "EnrollmentID", 
                     "ProjectName", 
                     "EntryDate", 
                     "currentLivingSituation_def",
                     "InformationDate_cls",#"InformationDate",
                     flag_colnames(output))]



output2 <- output2[!colnames(output2) %in% c("Region")]

output2$flag_nmfhh_and_1day_before.after_pitnight <- fun_flag_nmfhh_and_1day_before.after_pitnight(hh_cls1 = output2$hh_cls, 
                                                                                                   hh_cls_infodate1 = output2$hh_cls_infodate, 
                                                                                                   pit.night1 = pit.night)

grep("calc", colnames(output2), value = T, ignore.case = T)


# # 2022-01-25 [cls]: trying to do some summary stuff for andrea----
# args(data.table::between)
# 
# data.frame(nrow = 1:nrow(output2), 
#            ind_infodate = output2$InformationDate_cls,
#            NA.ind_infodate = is.na(output2$InformationDate_cls),
#            between_ind_infodate = data.table::between(x = output2$InformationDate_cls, 
#                                                       lower = pit.week_start, 
#                                                       upper = pit.week_end),
#            hh_infodate = output2$hh_cls_infodate, 
#            NA.hh_infodate = is.na(output2$hh_cls_infodate),
#            between_hh_infodate = data.table::between(x = output2$hh_cls_infodate, 
#                                          lower = pit.week_start, 
#                                          upper = pit.week_end)) %>%
#   as_tibble() %>%
#   group_by(NA.ind_infodate,
#            between_ind_infodate#,
#            #NA.hh_infodate, 
#            #between_hh_infodate
#            ) %>%
#   summarise(n=n())

# # /check something


output2 <- output2[!duplicated(output2),]

if(year(pit.night) == 2023){
  out.name.andrea <- glue("andrea_output2023__{Sys.Date()}_HR{hour(Sys.time())}.xlsx")
}else{
  out.name.andrea <- glue("andrea_output2022__{Sys.Date()}_HR{hour(Sys.time())}.xlsx")
}


# 2023-02-07: change col orders for andrea: 
andrea_cols_changes <- read_tsv("COLUMN_NAME	Original_Order	New_Order_Requested	REMOVE_COLUMN	NEED_TO_FINISH	RENAME_to_this_from_column_A
PersonalID	1	1			
reltionshiptohoh_def	2	2			
vetStatus_def	3	3			
calc_location_county	4	31			
calc_location_county_flag	5	32			
calc_region	6	33			
proj_county	7	34			
age_calc	8	4			
DOBDataQuality_def	9	5			
hud_age_calc	10	6			
gender_calc	11	7			
gender_category_calc	12	8	TRUE		
race_calc	13	9			
race2_calc	14	10	TRUE		
race_cat_calc	15	11	TRUE		
ethnicity_def	16	12			
HIV.AIDS	17	13			
chronic_hlth_C	18	14			
developmental_D	19	15			
mental_health_D	20	16			
physical_D	21	17			
substance_use_D	22	18			
provider_calc	23	19			
NCCounty	24	29			
CH	25	20		true - document what i've found so far	
domesticViolenceVictim_def	26	21			
currentlyFleeingDV_def	27	22			
householdType_def	28	23			
youth_type_hh	29	24		true - document what i've found so far	
veteran_type_hh	30	25		true - document what i've found so far	
HoH_PersonalID	31	35			
hh_cls	32	36			
hh_cls_infodate	33	37			
EnrollmentID	34	26			
ProjectName	35	27			
EntryDate	36	28			
flag.nccounty_na	37	30			
flag.reltohoh_na	38	40			
flag.child_hoh	39	41			
flag.age_too_old	40	42			
flag.DOB_na	41	43			
flag.gender	42	44			
flag.race	43	45			
flag.ethnicity	44	46			
flag.vetstatus	45	47			
flag_dv	46	48			
InformationDate_cls	newER_column	39			pid_cls_infodate
currentLivingSituation_def	newER_column	38			pid_cls
flag_nmfhh_and_1day_before.after_pitnight	49	49			")

output2A <- output2

andrea_cols_changes
o2a_cols <- data.frame(name.o2a = colnames(output2A), 
                       order.o2a = 1:ncol(output2A)) %>%
  as_tibble()

andrea_join <- full_join(andrea_cols_changes, 
          o2a_cols, 
          by = c("COLUMN_NAME" = "name.o2a"))

# # rename
# andrea_join$COLUMN_NAME[!is.na(andrea_join$RENAME_to_this_from_column_A)] <- 
#   andrea_join$RENAME_to_this_from_column_A[!is.na(andrea_join$RENAME_to_this_from_column_A)]

# remove fields
andrea_join <- andrea_join[is.na(andrea_join$REMOVE_COLUMN) | 
              andrea_join$REMOVE_COLUMN == F,]

# reorder fields
andrea_join <- andrea_join[order(andrea_join$order.o2a),]

andrea_join$Original_Order2 <- NA
for(i in 1:nrow(andrea_join)){
  # get colnumber of corresponding column in output2a
  andrea_join$Original_Order2[i] <- which(colnames(output2A) == andrea_join[i,]$COLUMN_NAME)
}
andrea_join <- andrea_join[order(andrea_join$Original_Order2),]
andrea_join$order.o2a

output2A <- output2A[,andrea_join$Original_Order2[order(andrea_join$New_Order_Requested)]]

# rename
colnames(output2A)[colnames(output2A) %in%
                     andrea_join$COLUMN_NAME[!is.na(andrea_join$RENAME_to_this_from_column_A)]] <- 
  andrea_join$RENAME_to_this_from_column_A[!is.na(andrea_join$RENAME_to_this_from_column_A)]


colnames(output2A)

andrea_join[,c("COLUMN_NAME", "Original_Order2", "New_Order_Requested", 
               "REMOVE_COLUMN", "NEED_TO_FINISH", "RENAME_to_this_from_column_A")]

# / 2023-02-07: change col orders for andrea: 

output2A %>%
  .[!duplicated(.),] %>%
  .[.$hh_cls == 16 & 
      !is.na(.$hh_cls),] %>%
  .[!is.na(.$hh_cls_infodate) & 
      .$hh_cls_infodate == pit.night,] %>%
  group_by(PersonalID) %>%
  summarise(n = n())
  



write.xlsx(x = output2A[!duplicated(output2A),], 
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

# DOB vs DOB Data Quality
output2$issue.DOB_vs_DOBDataQuality <- F
output2[output2$DOBDataQuality_def %in% c("Client refused", "Data not collected") & 
  !is.na(output2$age_calc),]$issue.DOB_vs_DOBDataQuality <- T

output2[is.na(output2$age_calc) & 
  output2$DOBDataQuality_def %in% 
  c("Full DOB reported", "Approximate or partial DOB reported"),]$issue.DOB_vs_DOBDataQuality <- T

table(output2$issue.DOB_vs_DOBDataQuality, useNA = "always")  

output2$issue_race <- output2$race_calc == "[unknown]"
output2$householdType_def %>% unique()

#output2$livingSituation_def %>% unique()
output2$reltionshiptohoh_def

output2$issue_no_HeadOfHousehold <- is.na(output2$PersonalID == output2$HoH_PersonalID & 
  output2$reltionshiptohoh_def != "Self (head of household)")


output3 <- output2 %>%
  .[!duplicated(.),] %>%
  # filter out dates not during pit week [added 2023-01-25]
  .[data.table::between(x = .$hh_cls_infodate, 
                        #lower = pit.week_start, (commented out 2023-01-31)
                        lower = pit.night,       #(added 2023-01-31)
                        upper =  pit.week_end) & 
      !is.na(.$hh_cls_infodate),] %>% 
  # / 
  
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
           hh_cls, 
           hh_cls_infodate,
           flag.nccounty_na, 
           flag.reltohoh_na, 
           flag.child_hoh, 
           flag.age_too_old, 
           flag.DOB_na, 
           flag.gender, 
           flag.race, 
           flag.ethnicity, 
           flag.vetstatus, 
           flag_dv,
           flag_nmfhh_and_1day_before.after_pitnight) %>%
  summarise() %>%
  # left_join(., 
  #           read_csv("regionscrosswalk.csv"), 
  #           by = c("County" )) %>%
  as.data.table() %>%
  melt(., 
        id.vars = c("EnrollmentID", "client_id", "ProjectName", "EntryDate", "calc_location_county", "calc_region", 
                    "hh_cls", "hh_cls_infodate") , 
       variable.name = "DQ_flag_type") %>%
  #.$calc_location_county %>% is.na() %>% table()
  as.data.frame() %>%
  as_tibble() %>%
  .[.$value == T,] %>%
  .[!is.na(.$EnrollmentID),] %>%
  .[!colnames(.) %in% c("value","EnrollmentID")]
  
output3 <- output3[grepl("^flag", output3$DQ_flag_type, ignore.case = T),]

# narrow donw to just 1 field with a county in it
grep("county", colnames(output3), value = T, ignore.case = T)

output3 <- output3[!colnames(output3) %in% c("proj_county")]
#output[output$PersonalID == 2664,]$Region


# change names----
output3$DQ_flag_type <- output3$DQ_flag_type  %>% as.character()

unique(output3$DQ_flag_type) %>% grep("^flag", ., value = T)

output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.reltohoh_na", 
                               "missing RelationshipToHoh", output3$DQ_flag_type)

output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag_nmfhh_and_1day_before.after_pitnight", 
                               "verify Current Living Situation Date", output3$DQ_flag_type)


output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.ethnicity", 
                               "verify ethnicity", output3$DQ_flag_type)
output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.gender", 
                               "verify gender", output3$DQ_flag_type)
output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag.vetstatus", 
                               "verify veteran status", output3$DQ_flag_type)
output3$DQ_flag_type <- ifelse((output3$DQ_flag_type) == "flag_dv", 
                               "verify DV-fleeing and DV-victim", output3$DQ_flag_type)
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

# spot check nicole to andrea
spot.check3 <- output3[sample(1:nrow(output3), size = 10),]
spot.check2 <- output2 %>%
  .[#.$EnrollmentID %in% spot.check3$EnrollmentID & 
      .$PersonalID %in% spot.check3$client_id,] %>% 
  group_by(#EnrollmentID, 
           PersonalID, calc_location_county, 
           calc_region) %>%
  summarise(n = n())

# nicole changes----
# remove enrolllmentID
output3 <- output3[!colnames(output3) %in% c("EnrollmentID")]

# remove dv flag
output3 <- output3[output3$DQ_flag_type != "verify DV-fleeing and DV-victim",]


# write to file----
getwd()
library(glue)

grep("calc", colnames(output3), value = T, ignore.case = T)

# Nicole final filter-----
#output3 #<-  do t he following filter: 

# filter 1: 

#output3[!nicole_filter1,]$DQ_flag_type

nicole_filter1 <- output3$hh_cls == 16 & 
  output3$hh_cls_infodate == pit.night & 
  output3$DQ_flag_type %in% c("verify NCCounty", 
                              "missing RelationshipToHoh",
                              "Head Of Household aged 16 or under", 
                              "older than 80 years old", 
                              "missing Date of Birth or DOB_quality", 
                              "verify gender", 
                              "missing race", 
                              "verify ethnicity", 
                              "verify veteran status")

nicole_filter2 <- output3$hh_cls == 16 & 
  output3$hh_cls_infodate != pit.night & 
  output3$DQ_flag_type %in% c("verify Current Living Situation Date")

# table(nicole_filter1, 
#       useNA = "always")
# table(nicole_filter2, 
#       useNA = "always")
# table(nicole_filter1 | nicole_filter2, 
#       useNA = "always")

output3 <- output3[nicole_filter1 | nicole_filter2,]

output3$hh_cls <- lapply(X = output3$hh_cls, 
       FUN = fun_livingsituation_def) %>% unlist()

# output3 %>%
#   group_by(DQ_flag_type) %>%
#   summarise(current_n = n())
# 
# output3$hh_cls


# / nicole final filter----



if(year(pit.night) == 2023){
  out.name <- glue("DQ_Flag2023__{Sys.Date()}_HR{hour(Sys.time())}.xlsx")
}else{
  out.name <- glue("nicole_output2022__{Sys.Date()}_HR{hour(Sys.time())}.xlsx")
}


write.xlsx(x = output3, 
           file = out.name)

