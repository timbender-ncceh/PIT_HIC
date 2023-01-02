
# Libraries----
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(data.table)
library(devtools)

rm(list=ls())
cat('\f')
gc()

# NOTE----
print("For hud pit survey for the night of Jan 26th, Entered on January 26th, Exited on Jaunary 27th")

pit.night <- ""

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

# # build list of colnames
file.colnames <- list()
for(i in csv.files){
  file.colnames[[i]] <- colnames(read_csv(i))
}

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

a.client$age_calc     <- calc_age(dob = a.client$DOB)
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

a.client$race_calc    <- NA
for(i in 1:nrow(a.client)){
  a.client$race_calc[i] <- fun_race(racenone        = a.client$RaceNone[i], 
                                    amindaknative   = a.client$AmIndAKNative[i], 
                                    asian           = a.client$Asian[i], 
                                    blackafamerican = a.client$BlackAfAmerican[i], 
                                    nativehipacific = a.client$NativeHIPacific[i], 
                                    white           = a.client$White[i])
  
}

a.client$ethncity_def <- unlist(lapply(a.client$Ethnicity, fun_ethnicity_def))
a.client$vetStatus_def <- unlist(lapply(a.client$VeteranStatus, fun_1.8_def))

# Enrollment Checks----
a.enrollment <- read_csv("Enrollment.csv")

# date filter

# / date filter


a.enrollment$reltionshiptohoh_def <- unlist(lapply(a.enrollment$RelationshipToHoH, fun_rel2hoh))

a.enrollment$HoH_PersonalID <- NA
for(i in unique(a.enrollment$HouseholdID)){
  enr.hoh_pid <- NA
  try(enr.hoh_pid <- a.enrollment[a.enrollment$HouseholdID == i & 
     a.enrollment$reltionshiptohoh_def == "Self (head of household)",]$PersonalID)
  
  if(length(enr.hoh_pid) != 1){
    enr.hoh_pid <- NA
  }
  
  try(a.enrollment$HoH_PersonalID[a.enrollment$HouseholdID == i] <- enr.hoh_pid)
}




# NC County of Service & Region
zip_co.cw <- read_tsv(file = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/zip_county_crosswalk.txt")

a.projectcoc <- read_csv(file = "ProjectCoC.csv") %>%
  left_join(., zip_co.cw[,c("ZIP", "County", "City")]) %>%
  left_join(., get_coc_region())

rm(zip_co.cw)





# living situation
a.enrollment$livingSituation_def <- unlist(lapply(a.enrollment$LivingSituation, fun_livingsituation_def))

# Exit Checks----
a.exit <- read_csv("Exit.csv")

a.exit$destination_def <- unlist(lapply(a.exit$Destination, fun_livingsituation_def))
table(a.exit$destination_def, useNA = "always")

# CurrentLivingSituation Checks----
a.currentlivingsituation <- read_csv("CurrentLivingSituation.csv")
a.currentlivingsituation$currentLivingSituation_def <- unlist(lapply(a.currentlivingsituation$CurrentLivingSituation, 
                                                                     fun_livingsituation_def))
a.currentlivingsituation$currentLivingSituation.Date_calc <- a.currentlivingsituation$InformationDate

table(a.currentlivingsituation$currentLivingSituation_def, 
      useNA = "always")


# Project Checks----
a.project <- read_csv("Project.csv")
a.project$provider_calc <- a.project$ProjectName
a.project$projectType_def <- unlist(lapply(a.project$ProjectType, fun_projtype))



# Disabilities Check----
a.disabilities <- read_csv("Disabilities.csv")

a.disabilities$InformationDate_disab <- a.disabilities$InformationDate

screened.pos.disab_df <- screened_positive_disability(dis_df = a.disabilities, enr_df = a.enrollment, exit_df = a.exit)


# Healthanddv check----
a.healthanddv <- read_csv("HealthAndDV.csv")

a.healthanddv$domesticViolenceVictim_def <- unlist(lapply(a.healthanddv$DomesticViolenceVictim,
                                                          fun_1.8_def))
a.healthanddv$currentlyFleeingDV_def <- unlist(lapply(a.healthanddv$CurrentlyFleeing, fun_1.8_def))


# Inventory check----
a.inventory <- read_csv("Inventory.csv")
a.inventory$householdType_def <- unlist(lapply(a.inventory$HouseholdType, fun_hhtype))



# Output files, pre-join----
b.client <- a.client[,c("PersonalID", "age_calc", "hud_age_calc", "gender_calc", "race_calc", "ethncity_def", "vetStatus_def")]

colnames(a.enrollment)
a.enrollment[colnames(a.enrollment) %in% c(grep("_def$|_calc$", colnames(a.enrollment), 
                                                ignore.case = F, value = T), 
                                           "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID")]

colnames(a.currentlivingsituation)
a.currentlivingsituation[colnames(a.currentlivingsituation) %in% c(grep("_def$|_calc$", colnames(a.currentlivingsituation), 
                                                ignore.case = F, value = T), 
                                           "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID")]

colnames(a.exit)
a.exit[colnames(a.exit) %in% c(grep("_def$|_calc$", colnames(a.exit), 
                                                ignore.case = F, value = T), 
                                           "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID", 
                               "ExitID", "ExitDate")]

colnames(a.project)
a.project[colnames(a.project) %in% c(grep("_def$|_calc$", colnames(a.project), 
                                                ignore.case = F, value = T), 
                                           "ProjectID", "OrganizationID")]

colnames(a.projectcoc)
a.projectcoc[colnames(a.projectcoc) %in% c(grep("_def$|_calc$", colnames(a.projectcoc), 
                                                ignore.case = F, value = T), 
                                           "ProjectCoCID", "ProjectID", "City", "ZIP", "County", "Region", 
                                           "CoCCode", "NCCounty", "HoH_PersonalID", "ProjectName")]

screened.pos.disab_df


colnames(a.healthanddv)
a.healthanddv[colnames(a.healthanddv) %in% c(grep("_def$|_calc$", colnames(a.healthanddv), 
                                    ignore.case = F, value = T), 
                               "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID", 
                               "ExitID", "ExitDate")]

colnames(a.inventory)
a.inventory[colnames(a.inventory) %in% c(grep("_def$|_calc$", colnames(a.inventory), 
                                                  ignore.case = F, value = T), 
                                             "EnrollmentID", "PersonalID", "ProjectID", "HouseholdID", "HoH_PersonalID", 
                                             "ExitID", "ExitDate", "CoCCode")]
