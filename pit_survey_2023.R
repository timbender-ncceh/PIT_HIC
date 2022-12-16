
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

library(tictoc)


tic()
# resources----
out.template <- "https://ncceh.sharepoint.com/:x:/s/DataCenter/EdQERAgSu5pGsBcN5VNGD20B3qlfQ7iOCFz9BPJi2xoADQ?e=zOvaac"

# Setup----
csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/data/coc_by_ffy/bos_2022"

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


# Working Directory Setup----
setwd(csv.file.dir)
getwd()

# Identify the columns that I need to join----
# List all files
csv.files <- list.files(pattern = "\\.csv$") 
csv.files <- csv.files[!csv.files %in% c("Client2.csv", "report_phoneemail.csv")]


# build list of colnames
file.colnames <- list()
for(i in csv.files){
  file.colnames[[i]] <- colnames(read_csv(i))
}

# desired cols 
des.cols <- read_tsv("var1	Description	Calculated	Type	Option 1	Option 2	Option 3	Option 4	Option 5	Option 6	Option 7	var2	var3	var4	var5	var6	var7
Client ID	Client ID	FALSE	numeric													
HH Rel	Relationship to Head of Household	FALSE	picklist	Self	Head of Household's child	Head of Household's spouse		Head of Household's other relation	Other: non-relation	Data Not Collected						
HLD ID	Household ID	FALSE	numeric													
Vet	Veteran Status	FALSE	picklist	Yes	No	Client Doesn't Know	Client Refused	Data Not Collected								
Age	Age on 1/25/23	TRUE	numeric													
Age Category	Category as defined by HUD	TRUE	picklist	Under 18	18-24	25-34	35-44	45-54	55-64	65 and older						
Gender	Gender Identity	FALSE	picklist (multiple select)	Male	Female	A gender other than singlularly female or male	Questioning	Transgender	Client Doesn't Know	Client Refused	Data Not Collected					
Gender Category	Gender Category	TRUE	picklist	Male	Female	No Single Gender	Questioning	Transgender	Client Doesn't Know	Client Refused	Data Not Collected					
race1	Primary Race 	FALSE	picklist	White	Black, African American, or African	Asian or Asian American	American Indian, Alaska Native, or Indigenous	Native Hawaiian or Pacific Islander	Client Doesn't Know	Client Refused	Data Not Collected					
race2	secondary Race	FALSE	picklist	White	Black, African American, or African	Asian or Asian American	American Indian, Alaska Native, or Indigenous	Native Hawaiian or Pacific Islander	Client Doesn't Know	Client Refused	Data Not Collected					
Race Cat	Multi-racial or single racial identity	TRUE	picklist	White	Black, African American, or African	Asian or Asian American	American Indian, Alaska Native, or Indigenous	Native Hawaiian or Pacific Islander	Multiple Races	Client Doesn't Know	Client Refused	Data Not Collected				
ethnicity	Ethnicity	FALSE	picklist	Non-Hispanic/Non-Latin(a)(o)(x)	Hispanic/Latin(a)(o)(x)	Client Doesn't Know	Client Refused	Data Not Collected								
Disability (Gateway)	Disabling Condition	FALSE	picklist	Yes	No	Client Doesn't Know	Client Refused	Data Not Collected								
Disability Type	positive types of Disabling Conditions	FALSE	picklist (multiple select)	Mental Health Disorder	Alcohol Use Disorder	Drug Use Disorder	Both Alcohol and Drug Use Disorders	Chronic Health Condition	HIV/AIDS	Development Disability	Physical Disability					
Provider	Project Name	FALSE	CHAR (50)													
Region	NC-BoS CoC Region	TRUE	picklist	R01	R02	R03	R04	R05	R06	R07	R08	R09	R10	R11	R12	R13
County	NC County of Service	FALSE	picklist													
CH	Chronic Homelessness	not sure														
DV	Domestic Violence History	FALSE	picklist	Yes	No	Client Doesn't Know	Client Refused	Data Not Collected								
DV fleeing	Fleeing Domestic Violence response	FALSE	picklist	Yes	No	Client Doesn't Know	Client Refused	Data Not Collected								
HH Type	Household Type	TRUE	picklist	AO (Adults Only)	AC (Adults with Children)	CO (Children Only)										
Youth Type	Youth Household Type	TRUE	picklist	UA (Unaccompanied Adult)	PY (Parenting Youth)	PYC (Children of Parenting Youth)										
Vet Type	Veteran Household Type	TRUE	picklist	Vet AO (Adults Only)	Vet AC (Adults with Children)											
Disability Types	formatting break only															
Alcohol Use disorder	positive for Alcohol Use Disorder	TRUE	picklist	Yes	No											
Drug Use Disorder	positive for Substance Use Disorder	TRUE	picklist	Yes	No											
Mental health	positive for Mental Health Disorder	TRUE	picklist	Yes	No											
HIV	positive for HIV/AIDS	TRUE	picklist	Yes	No											
DV	positive for Fleeing Domestic Violence	TRUE	picklist	Yes	No											
CLS Date	Client's Current Living Situation Date	FALSE	Date													
CLS	Client's Current Living Situation	FALSE	picklist	3.12.1 Living Situation Option List												
HoH CLS Date	Head of Household's Current Living Situation Date	TRUE	Date													
HoH CLS	Head of Household's Current Living Situation	TRUE	picklist	3.12.1 Living Situation Option List												
Notes	Manual notes for context															")



colnames(des.cols) <- c("abbr", "desc", "calculated", 
                        "type", paste("opt", const_nchar(1:13), sep = ""))

des.cols[,c(1,2)]



# create function for finding colnames in hmis.csv dataset

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

# find_cols("RelationshipToHoH")
# find_cols("PersonalID")
# find_cols("HouseholdID")
# find_cols("VeteranStatus")
# #find_cols("age[!stage][!manage]")
# find_cols("gender")
# find_cols("sexualorientation")
# find_cols("category")
# find_cols("racenone")
# find_cols("white|black|african|asian|paci|chin")
# find_cols("primary|secondary")
# find_cols("multi")
# find_cols("Ethnicity")
# find_cols("DisablingCondition")
# find_cols("DisabilityType")
# find_cols("projectname")
# find_cols("provider")
# find_cols("CoC")
# find_cols("County")
# find_cols("CH", ic = F)
# find_cols("DV")
# find_cols("Violence")
# find_cols("Fleeing")
# find_cols("history")
# find_cols("HouseholdType")
# find_cols("HoH")
# find_cols("Alcohol|Drug")
# find_cols("Disorder")
# find_cols("Mental|MentalHealth")
# find_cols("CurrentLivingSituation")
# find_cols("InformationDate")
# find_cols("LivingSituation")
# find_cols("DOB")
# find_cols("ID$")
# find_cols("male")
# find_cols("entrydate")



is_hashed(read_csv("Client.csv")) %>% as.data.frame()



# Needed Data----

c.curlivingsit <- read_csv("CurrentLivingSituation.csv") %>%
  .[,c("CurrentLivingSitID", "EnrollmentID", "PersonalID","CurrentLivingSituation", 
       "InformationDate")]
colnames(c.curlivingsit) <- c("CurrentLivingSitID", "EnrollmentID", "PersonalID","CurrentLivingSituation", 
                              "InformationDate_cls")


c.healthanddv <- read_csv("HealthAndDV.csv") %>%
  .[,c("HealthAndDVID", "EnrollmentID",  "PersonalID", "CurrentlyFleeing", 
       "InformationDate")]
colnames(c.healthanddv) <- c("HealthAndDVID", "EnrollmentID",  "PersonalID", "CurrentlyFleeing", 
                         "InformationDate_hdv")

c.client <- read_csv("Client.csv") %>%
  .[,c("PersonalID", "DOB", "NoSingleGender", "Transgender", "GenderNone", 
       "Female", "Male", "Ethnicity", "RaceNone", "AmIndAKNative", "Asian", 
       "BlackAfAmerican", "NativeHIPacific", "White", "Questioning", 
       "VeteranStatus")]

c.enrollment <- read_csv("Enrollment.csv") %>%
  .[,c("EnrollmentID", "HouseholdID", "PersonalID", "ProjectID", 
       "DisablingCondition", "NCCounty", "RelationshipToHoH", 
       "LiteralHomelessHistory", "LivingSituation",
       "MentalHealthDisorderFam", "AlcoholDrugUseDisorderFam", 
       "EntryDate")]

c.exit <- read_csv("Exit.csv") %>%
  .[,c("ExitID"  ,     "EnrollmentID" ,"PersonalID", "ExitDate")]
  

c.enrollmentcoc <- read_csv("EnrollmentCoC.csv") %>%
  .[,c("EnrollmentCoCID", "EnrollmentID", "HouseholdID", "ProjectID", "PersonalID", 
       "CoCCode", 
       "InformationDate")]
colnames(c.enrollmentcoc) <- c("EnrollmentCoCID", "EnrollmentID", "HouseholdID", "ProjectID", "PersonalID", 
                               "CoCCode", 
                               "InformationDate_enr")
  
c.disabilities <- read_csv("Disabilities.csv") %>%
  .[,c("DisabilitiesID", "EnrollmentID", "PersonalID", "DisabilityType", 
       "DisabilityResponse", "IndefiniteAndImpairs", 
       "InformationDate")]
colnames(c.disabilities) <- c("DisabilitiesID", "EnrollmentID", "PersonalID", "DisabilityType", 
                              "DisabilityResponse", "IndefiniteAndImpairs", 
                              "InformationDate_disab")

c.project <- read_csv("Project.csv") %>% 
  .[,c("ProjectID", "OrganizationID", "ProjectName")]

c.projectcoc <- read_csv("ProjectCoC.csv") %>%
  .[,c("ProjectCoCID", "ProjectID", "CoCCode")]

c.inventory <- read_csv("Inventory.csv") %>%
  .[,c("InventoryID", "ProjectID","HouseholdType")]

some.date <- ymd(20220126)


temp <- hmis_join(c.enrollment, c.exit, jtype = "left") %>%
  hmis_join(., c.enrollmentcoc, 
            jtype = "left") 

temp$enrollment_open <- is.na(temp$ExitDate)

temp$somedate_between_enrollment <- NA
for(i in 1:nrow(temp)){
  
  if(!temp$enrollment_open[i]){
    temp$somedate_between_enrollment[i] <- between(x = some.date, 
            lower = temp$EntryDate[i], upper = temp$ExitDate[i])
  }
  
}

temp <- temp[temp$somedate_between_enrollment | 
  temp$enrollment_open,]


temp <- temp %>%
  #.[.$EntryDate <= some.date & .$ExitDate >= some.date ,] %>%
  
  hmis_join(., c.client, jtype = "left") %>%
  hmis_join(., c.project, jtype = "left") %>%
  hmis_join(., c.projectcoc, jtype = "left") %>%
  hmis_join(., c.inventory, jtype = "left") %>%
  hmis_join(., c.curlivingsit, jtype = "left") %>%
  #hmis_join(., c.disabilities, jtype = "left") %>%
  hmis_join(., c.healthanddv, jtype = "left")

temp <- temp[!(is.na(temp$EnrollmentID) & is.na(temp$PersonalID)),]

gc()


#disabilities

temp$disab_calc <- NA

args(screened_positive_disability)

disab_out <- screened_positive_disability(dr0 = c.disabilities$DisabilityResponse, 
                                          ii0 = c.disabilities$IndefiniteAndImpairs, 
                                          dt0 = c.disabilities$DisabilityType, 
                                          dis_df = c.disabilities) %>% as_tibble()


temp$age_calc <- calc_age(temp$DOB, age_on_date = ymd(20220105))
temp$is_HoH_calc <- temp$RelationshipToHoH == 1

#temp$hud_agegrp_cal <- unlist(lapply(temp$age_calc, hud_age_category))

temp$hud_agegrp_cal <- NA
gc()
for(i in 1:nrow(temp)){
  temp$hud_agegrp_cal[i] <- hud_age_category(temp$age_calc[i])
}

cat(crayon::bgRed(crayon::bold("DONE")))


temp$gender <- NA

for(i in 1:nrow(temp)){
   if(!is.na(temp$EnrollmentID[i])){
    if(i %% 1000 == 0){
      print(i) 
    }
     
    temp$gender[i] <- fun_gender(male = temp$Male[i], 
                                 female = temp$Female[i], 
                                 nosingle = temp$NoSingleGender[i], 
                                 questioning = temp$Questioning[i], 
                                 trans = temp$Transgender[i], 
                                 gendernone = temp$GenderNone[i])
  }
}
  



temp$race <- NA

for(i in 1:nrow(temp)){
  temp$race[i] <- fun_race(racenone = temp$RaceNone[i], 
           amindaknative = temp$AmIndAKNative[i], 
           asian = temp$Asian[i], 
           blackafamerican = temp$BlackAfAmerican[i], 
           nativehipacific = temp$NativeHIPacific[i], 
           white = temp$White[i])
}


temp$region <- NA
for(i in 1:nrow(temp)){
  try(temp$region[i] <- coc_region(temp$NCCounty[i]))
}





colnames(temp)
des.cols[,c(1,2)]$desc

des.cols$abbr

# missing cols

missing.cols <- c(#"race", 
                  "disability stuff", 
                  #"region", 
                  #"place not meant for human habitation", 
                  "chronic homelessness", 
                  #"fleeing domestic violence", 
                  "youth HH type", "vet HH type", 
                  #"current living situation date", 
                  "hoh current living situation date", 
                 # "client current living situation", 
                  "hoh current living situation", 
                  "Notes/manual notes for context")




#remove cols

temp$curr

keep.cols <- c("PersonalID", "HouseholdID", "EnrollmentID", 
               "RelationshipToHoH", "VeteranStatus", 
               "age_calc", "gender", "Ethnicity", "LivingSituation",
               "NCCounty", "HouseholdType", "CurrentLivingSituation",
               "CurLivingSitDate", "CurrentlyFleeing", "race")


# Output----

# check if hashed
library(openssl)



hash.key <- paste(sample(c(letters,LETTERS,0:9), size = sample(c(3:100), 1), replace = T), 
                  sep = "", collapse = "")

if(!all(is_hashed(temp$DOB))){
  temp$DOB <- openssl::sha256(x = as.character(temp$DOB), key = hash.key)
}

library(readr)
write_csv(x = temp, 
          file = "master_PIT_draft1.csv")

openxlsx::write.xlsx(x = temp, 
                     file = "master_PIT_draft1.xlsx")


toc(log = T)
