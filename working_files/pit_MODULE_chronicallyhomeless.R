# CHRONIC HOMELESSNESS MODULE

library(openxlsx)
library(datetimeutils)

# DOWNLOAD INSTRUCTIONS from HMIS----

# 1. log into HMIS
# 2. Mode: Shadow
# 3. Search Shadow Mode User: bralexander (5483)

# 4. REPORTS >>> BUSINESSOBJECTS >>> FOLDERS >>> PUBLIC FOLDERS >>>
# hmisncceh_live_folder >>> HMIS@NCCEH Gallery >>>
# A003_Chronic_Homelessness_Check

# 5. Provier(s): (Optional)

# 6. Reporting Group(s): 
#   [1]  "Region 10 Coordinated Entry SSO Reporting Group(1688)"
#   [2]  "Region 11 Coordinated Entry SSO Reporting Group(1689)"
#   [3]  "Region 12 Coordinated Entry SSO Reporting Group(1690)"
#   [4]  "Region 13 Coordinated Entry SSO Reporting Group(1997)"
#   [5]  "Region  1 Coordinated Entry SSO Reporting Group(2046)" 
#   [6]  "Region  2 Coordinated Entry SSO Reporting Group(1686)" 
#   [7]  "Region  3 Coordinated Entry SSO Reporting Group(1999)" 
#   [8]  "Region  4 Coordinated Entry SSO Reporting Group(2037)" 
#   [9]  "Region  5 Coordinated Entry SSO Reporting Group(1687)" 
#   [10] "Region  6 Coordinated Entry SSO Reporting Group(2048)" 
#   [11] "Region  7 Coordinated Entry SSO Reporting Group(2049)" 
#   [12] "Region  9 Coordinated Entry Reporting Group(2051)"     
#   [13] "Region  8 Coordinated Entry Reporting Group(2040)"   

# 7. EDA Provider:         -Default Provider-
# 8. Start Date:           1/25/2023 12:00:00 AM
# 9. End Date Plus 1 Day:  1/26/2023 12:00:00 AM

# get filename----
ch.input.name <- list.files(pattern = "^A003 -")

# load data---
ch.clientdetail.df <- readWorkbook(ch.input.name, sheet = "Client Detail", 
                                   colNames = F, 
                                   rowNames = F, 
                                   detectDates = F, 
                                   skipEmptyCols = T, 
                                   skipEmptyRows = F, 
                                   rows = NULL, 
                                   cols = 1:13, 
                                   fillMergedCells = T) %>%
  as_tibble()

# tidy----
# which rows to skip on load (empty, column headers, etc.)
loadrows <- ch.clientdetail.df$X1 %>% 
  as.numeric()
loadrows <- which(!is.na(loadrows))+1
rm(ch.clientdetail.df)

# load client detail again----
# this time, capturing only the rows we need
cldet.df <- readWorkbook(ch.input.name, sheet = "Client Detail", 
                         colNames = F, 
                         rowNames = F, 
                         detectDates = F, 
                         skipEmptyCols = T, 
                         #skipEmptyRows = F, 
                         rows = loadrows, 
                         cols = 1:13, 
                         fillMergedCells = T) %>%
  as_tibble()

# manually pull column names from xlsx file----
client.colnames.txt <- "HMIS ID	Household ID	Household Type	Relationship to Head of Household	Entry Date	Disabling Condition?	Prior Living Situation	Length of Stay in Previous Place	Approximate Date Homelessness Started	Regardless of where they stayed last night - Number of times the client has been on the streets, in ES, or SH in the past three years including today(5167)	Total number of months homeless on the street, in ES or SH in the past three years(5168)	Chronic Status" %>%
  strsplit(., split = "\t") %>%
  unlist()

# change column names----
# change 'HMIS ID' to 'PersonalID'
client.colnames.txt[client.colnames.txt == "HMIS ID"] <- "PersonalID"
# change 'Household ID' to 'HouseholdID'
client.colnames.txt[client.colnames.txt == "Household ID"] <- "HouseholdID"
# change 'Chronic Status' to 'ChronicStatus'
client.colnames.txt[client.colnames.txt == "Chronic Status"] <- "ChronicStatus"
# change 'Entry Date' to 'EntryDate' 
client.colnames.txt[client.colnames.txt == "Entry Date"] <- "EntryDate"

# update column names with manual changes (form above)----
colnames(cldet.df) <- client.colnames.txt

# remove unneeded cols----
cldet.df <- cldet.df[,c("PersonalID", 
                        #"HouseholdID", 
                        "EntryDate",
                        "ChronicStatus")]
# convert EntryDate from dbl to date----
cldet.df$EntryDate <- datetimeutils::convert_date(x = cldet.df$EntryDate, 
                                                  type = "excel", 
                                                  fraction = F, tz = "America/New_York") 

# add a new join_by row for date that's class(character)
cldet.df$EntryDate_char <- as.character(cldet.df$EntryDate)

# cleanup ----
rm(ch.input.name, 
   client.colnames.txt, 
   loadrows)
