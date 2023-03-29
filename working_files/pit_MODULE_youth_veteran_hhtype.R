# simulate youth_ veteran_ households scenarios
# Libraries----
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(data.table)
library(devtools)
library(openxlsx)
library(ggplot2)

rm(list=ls())
cat('\f')
gc()
csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data"
setwd(csv.file.dir)

# Functions----
#devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/pit_survey_calculations.R?raw=TRUE")
# updated to reflect dev branch ^^^
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev-PIT_week_12/pit_survey_calculations.R?raw=TRUE")

rm(list=ls()[!ls() %in% c("get_youth.hh.info", 
                          "calc_age", 
                          "fun_rel2hoh", 
                          "fun_1.8_def", 
                          "make_abbr", 
                          "hh_age.unknown", 
                          "hh_wal1a1c", 
                          "hh_wo.c", 
                          "hh_w.o.C", 
                          "py_u18", 
                          "py_18.24", 
                          "uy", 
                          "hh_vet", 
                          "hh_youth")])

# data----
pit.night    <- ymd(20230125)
a.enrollment <- read_csv("Enrollment.csv")
a.client     <- read_csv("Client.csv")


# tidy----
a.client$age                       <- calc_age(a.client$DOB, 
                                               decimal.month = F,
                                               age_on_date = pit.night)
a.client$VeteranStatus_def         <- unlist(lapply(X = a.client$VeteranStatus, 
                                                    FUN = fun_1.8_def))
a.enrollment$RelationshipToHoH_def <- unlist(lapply(X = a.enrollment$RelationshipToHoH, 
                                                    FUN = fun_rel2hoh))


# REAL DATA----
real.df <- left_join(a.enrollment[,c("HouseholdID", "PersonalID", "EnrollmentID", 
                                     "RelationshipToHoH_def")], 
                     a.client[,c("PersonalID", "age", 
                                 "VeteranStatus_def")]) 


real.df$hh1_age.unknown <- NA
real.df$hh1_vet         <- NA
real.df$hh1_w.o.C       <- NA
real.df$hh1_wal1a1c     <- NA
real.df$hh1_wo.c        <- NA
real.df$hh1_youth       <- NA
real.df$py1_18.24       <- NA
real.df$py1_u18         <- NA
real.df$uy1             <- NA


for(i in unique(real.df$HouseholdID)){
 #print(i)
  if(!is.na(i)){
    real.df$hh1_age.unknown[real.df$HouseholdID == i] <- hh_age.unknown(ages = real.df$age[real.df$HouseholdID == i])
    real.df$hh1_vet[real.df$HouseholdID == i]         <- hh_vet(vet.statuses = real.df$VeteranStatus_def[real.df$HouseholdID == i])
    real.df$hh1_w.o.C[real.df$HouseholdID == i]       <- hh_w.o.C(ages = real.df$age[real.df$HouseholdID == i])
    real.df$hh1_wal1a1c[real.df$HouseholdID == i]     <- hh_wal1a1c(ages = real.df$age[real.df$HouseholdID == i])
    real.df$hh1_wo.c[real.df$HouseholdID == i]        <- hh_wo.c(ages = real.df$age[real.df$HouseholdID == i])
    real.df$hh1_youth[real.df$HouseholdID == i]       <- hh_youth(ages = real.df$age[real.df$HouseholdID == i])
    real.df$py1_18.24[real.df$HouseholdID == i]       <- py_18.24(ages = real.df$age[real.df$HouseholdID == i], 
                                                                  rel2hohs = real.df$RelationshipToHoH_def[real.df$HouseholdID == i])
    real.df$py1_u18[real.df$HouseholdID == i]         <- py_u18(ages = real.df$age[real.df$HouseholdID == i], 
                                                                rel2hohs = real.df$RelationshipToHoH_def[real.df$HouseholdID == i])
    real.df$uy1[real.df$HouseholdID == i]             <- uy(ages = real.df$age[real.df$HouseholdID == i], 
                                                            rel2hohs = real.df$RelationshipToHoH_def[real.df$HouseholdID == i])
  }
}

colnames(real.df)
unique(real.df$RelationshipToHoH_def)

real.df.out <- real.df %>%
  group_by(HouseholdID,
           hh1_youth, 
           hh1_vet, 
           hh1_age.unknown,       
           hh1_w.o.C, 
           hh1_wal1a1c, 
           hh1_wo.c, 
           py1_u18, 
           py1_18.24, 
           uy1) %>%
  summarise(hh_size = n_distinct(PersonalID), 
            #n_hhid = n_distinct(HouseholdID), 
            n_vets  = sum(VeteranStatus_def == "Yes", na.rm = T),
            n_HoH   = sum(RelationshipToHoH_def == "Self (head of household)", na.rm = T),
            n_ChildOfHoH = sum(RelationshipToHoH_def == "Head of household’s Child", na.rm = T),
            min_age = min(age), 
            max_age = max(age),
            max_known.age = max(age,na.rm = T),
            nAge_0to17  = sum(age < 18, na.rm = T), 
            nAge_18to24 = sum(age >= 18 & age <= 24, na.rm = T), 
            nAge_25up   = sum(age >= 25, na.rm = T)) %>%
  ungroup() %>% 
  mutate(., 
         hh1_youth       = ifelse(hh1_youth, "YOUTH", ""),
         hh1_age.unknown = ifelse(hh1_age.unknown, "AGE_UNKNOWN", ""),
         hh1_vet         = ifelse(hh1_vet == "Yes", "VETERAN", ""),
         hh1_w.o.C       = ifelse(hh1_w.o.C, "hh_with_only_children", ""), 
         hh1_wal1a1c     = ifelse(hh1_wal1a1c, "hh_with_1adult1child", ""), 
         hh1_wo.c        = ifelse(hh1_wo.c, "hh_without_children", ""), 
         py1_u18         = ifelse(py1_u18, "py_under18", ""), 
         py1_18.24       = ifelse(py1_18.24, "py_18to24", ""), 
         uy1             = ifelse(uy1, "unaccomp_youth", "")) %>%
  mutate(., 
         out_hh_type = paste(hh1_youth, 
                             hh1_age.unknown, 
                             hh1_vet, 
                             hh1_w.o.C, 
                             hh1_wal1a1c, 
                             hh1_wo.c, 
                             py1_u18, 
                             py1_18.24, 
                             uy1, 
                             sep = "-")) %>%
  mutate(., 
         out_hh_type = gsub(pattern = "^-{1,}|-{1,}$", "", out_hh_type), 
         out_hh_type = gsub(pattern = "-{1,}", ", ", out_hh_type))



real.df.out$out_hh_type %>% unique()

not.cols <- colnames(real.df.out) %>%
  grep(pattern = "^py1|^hh1|^uy1", x = ., value = T)

keep.cols <- colnames(real.df.out)[!colnames(real.df.out) %in%
                        not.cols]

final.out <- real.df.out[colnames(real.df.out) %in% keep.cols]

library(ggplot2)
