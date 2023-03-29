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
                          "hh_vet")])





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
            min_age = min(age), 
            max_age = max(age), 
            max_know.age = max(age,na.rm = T)) %>%
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
         out_hh_type = gsub(pattern = "-{1,}", "--", out_hh_type))



real.df.out$out_hh_type %>% unique()
# for(i in unique(real.df$HouseholdID)){
#   try(real.df$youth_vet_hhtype[real.df$HouseholdID == i] <- 
#     get_youth.hh.info(hh_pid.ages.v   = real.df$age[real.df$HouseholdID == i], 
#                       relations2hoh.v = real.df$RelationshipToHoH_def[real.df$HouseholdID == i], 
#                       vetstatus.v     = real.df$VeteranStatus_def[real.df$HouseholdID == i]))
# }
# 
# 
# 
# sum.df <- real.df %>%
#   group_by(HouseholdID, youth_vet_hhtype) %>%
#   summarise(n_ppl = n_distinct(PersonalID), 
#             anyVets.Yes = any(VeteranStatus_def == "Yes"), 
#             anyVets.No = any(VeteranStatus_def == "No"), 
#             anyVets.NA = any(is.na(VeteranStatus_def)), 
#             min.age = min(age), 
#             max.age = max(age), 
#             max.age_known = max(age, na.rm = T),
#             anyAge.NA = any(is.na(age)), 
#             output.NA = any(is.na(youth_vet_hhtype) + all(is.na(youth_vet_hhtype))))
# 
# sum.df[sum.df$output.NA,]  
# 
# group_by(hh_nppl,
#            any_vets = vetstat == "Yes",
#            youth_vet_hh_type) %>%
#   summarise(min.age = min(age), 
#             max.age = max(age), 
#             max.known_age = max(age,na.rm = T)) %>%
#   # flags / issues 
#   mutate(., 
#          flag.youth_ageNA = is.na(min.age) & 
#            grepl(pattern = "^YOUTH - | - YOUTH - ", 
#                  x = youth_vet_hh_type), 
#          flag.typeNA = is.na(youth_vet_hh_type), 
#          flag.vet.too_young = (any_vets | grepl("VETERAN", youth_vet_hh_type)) & 
#            (max.known_age < 18 & !is.infinite(max.known_age)), 
#          flag.vets_in_notVetHH = any_vets & !grepl("VETERAN", youth_vet_hh_type), 
#          flag.no_vets_in_VetHH = !any_vets & grepl("VETERAN", youth_vet_hh_type))
# 
# 
# 
# 
# 
# 
# # SIMULATION----
# # data ranges----
# unique.age       <- unique(c(seq(0,32,by=1),NA))
# unique.rel2hoh   <- unique(c(a.enrollment$RelationshipToHoH_def,NA))
# unique.rel2hoh   <- ifelse(is.na(unique.rel2hoh), "unknown", unique.rel2hoh)
# unique.vetstatus <- unique(ifelse(unique(c(a.client$VeteranStatus_def,NA)) %in%
#                              c("Client doesn't Know", "Client refused") | 
#                              is.na(unique(c(a.client$VeteranStatus_def,NA))), 
#                            yes = "unknown", 
#                            no = unique(c(a.client$VeteranStatus_def,NA))))
#                              
# 
# sim_hh <- function(n_ppl, 
#                    ua = unique.age, 
#                    ur = unique.rel2hoh, 
#                    uv = unique.vetstatus){
#   require(openssl)
#   require(dplyr)
#   
#   hh_ages      <- sample(ua, size = n_ppl,replace=T) + runif(n = n_ppl)
#   hh_rel2hohs  <- sample(ur, size = n_ppl,replace=T) 
#   hh_vetstatus <- sample(uv, size = n_ppl,replace=T)
#   hh.id        <- as.character(md5(paste(sample(c(0:9,letters,LETTERS), size = 128, replace = T),sep="",collapse="")))
#   
#   out <- data.frame(hhid    = hh.id, 
#                     pid     = 1:n_ppl, 
#                     hh_nppl = n_ppl,
#                     age     = hh_ages, 
#                     rel2hoh = hh_rel2hohs, 
#                     vetstat = hh_vetstatus) %>% as_tibble()
#   
#   # calculate age-group <18, 18-24, >24
#   
#   out$age_group <- NA
#   out$age_group[out$age < 18]            <- "<18"
#   out$age_group[between(out$age, 18,24)] <- "18-24"
#   out$age_group[out$age > 24]            <- ">24"
#   
#   # id oldest hh member
#   out$oldest_hoh_mbr <- out$age == max(out$age, na.rm = T)
#   
#   return(out)
# }
# 
# sim_hh(2)
# 
# 
# sim.hh.df <- NULL
# for(i in 1:1000){
#   #set.seed(as.numeric(lubridate::ymd(20230324))+i)
#   sim.hh.df <- rbind(sim.hh.df, 
#                      sim_hh(n_ppl = sample(1:3, size = 1, prob = c(1,5,5))))
# }
# 
# # summarise households----
# hh_summary <- sim.hh.df %>%
#   group_by(hhid) %>%
#   summarise(n_pid = n_distinct(pid), 
#             min_age = min(age,na.rm = T), 
#             max_age = max(age,na.rm = T), 
#             n_vets  = sum(vetstat == "Yes"), 
#             any_vets = any(vetstat == "Yes"), 
#             n_hoh    = sum(rel2hoh == "Self (head of household)")) 
# 
# # potential household flags----
# hh_flags <- sim.hh.df %>%
#   mutate(., 
#          flag.vet_age = vetstat == "Yes" & age_group == "<18", 
#          flag.hoh_not_oldest = rel2hoh == "Self (head of household)" & 
#            !oldest_hoh_mbr, 
#          flag.multiple_hoh = ifelse(hhid %in% 
#                                       hh_summary$hhid[hh_summary$n_hoh > 1], 
#                                     yes = T, no = F), 
#          flag.1pid_and_rel2hoh_child_of_hoh_or_similar = hh_nppl == 1 & 
#            grepl(pattern = "^Head of household", 
#                  x = rel2hoh))
# 
# 
# 
# sim.hh.df[sim.hh.df$hh_nppl == 1 & 
#             grepl(pattern = "^Head of household", 
#                   x = sim.hh.df$rel2hoh),]
# 
# table(hh_flags$flag.1pid_and_rel2hoh_child_of_hoh_or_similar)
# 
# flagged.hhids <- hh_flags$hhid[hh_flags$flag.hoh_not_oldest | 
#                                  hh_flags$flag.multiple_hoh | 
#                                  hh_flags$flag.vet_age] %>%
#   unique()
# 
# # non-flagged dataset----
# sim.hh.df_noflags <- sim.hh.df %>%
#   .[!.$hhid %in% flagged.hhids,]
# 
# sim.hh.df_noflags %>%
#   group_by(hh_nppl, rel2hoh, age_group, vetstat) %>%
#   summarise(n.hh = n_distinct(hhid))
# 
# # run logic----
# 
# 
# sim.hh.df_noflags$youth_vet_hh_type <- NA
# for(i in unique(sim.hh.df_noflags$hhid)){
#   try(sim.hh.df_noflags$youth_vet_hh_type[sim.hh.df_noflags$hhid == i] <- 
#     get_youth.hh.info(hh_pid.ages.v   = sim.hh.df_noflags$age[sim.hh.df_noflags$hhid == i], 
#                       relations2hoh.v = sim.hh.df_noflags$rel2hoh[sim.hh.df_noflags$hhid == i], 
#                       vetstatus.v     = sim.hh.df_noflags$vetstat[sim.hh.df_noflags$hhid == i]))
# }
# 
# 
# 
# 
# sim.hh.df_noflags[sim.hh.df_noflags$hhid %in%
#                     sample(sim.hh.df_noflags$hhid, size = 1),
#                   c("age", "rel2hoh", 
#                     "vetstat", "age_group", "youth_vet_hh_type")] %>%
#   mutate(., 
#          age = as.integer(age))
# 
# 
# sim.hh.df_noflags %>%
#   mutate(., age = as.integer(age)) %>%
#   group_by(hh_nppl,
#            any_vets = vetstat == "Yes",
#            youth_vet_hh_type) %>%
#   summarise(min.age = min(age), 
#             max.age = max(age), 
#             max.known_age = max(age,na.rm = T)) %>%
#   # flags / issues 
#   mutate(., 
#          flag.youth_ageNA = is.na(min.age) & 
#            grepl(pattern = "^YOUTH - | - YOUTH - ", 
#                  x = youth_vet_hh_type), 
#          flag.typeNA = is.na(youth_vet_hh_type), 
#          flag.vet.too_young = (any_vets | grepl("VETERAN", youth_vet_hh_type)) & 
#            (max.known_age < 18 & !is.infinite(max.known_age)), 
#          flag.vets_in_notVetHH = any_vets & !grepl("VETERAN", youth_vet_hh_type), 
#          flag.no_vets_in_VetHH = !any_vets & grepl("VETERAN", youth_vet_hh_type))
