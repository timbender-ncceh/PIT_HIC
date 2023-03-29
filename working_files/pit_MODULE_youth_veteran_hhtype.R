# MODULE to produce youth_ veteran_household_type

temp.enrollment <- read_csv("Enrollment.csv")
temp.client     <- read_csv("Client.csv")

# tidy----
temp.client$age                       <- calc_age(temp.client$DOB, 
                                               decimal.month = F,
                                               age_on_date = pit.night)
temp.client$VeteranStatus_def         <- unlist(lapply(X = temp.client$VeteranStatus, 
                                                    FUN = fun_1.8_def))
temp.enrollment$RelationshipToHoH_def <- unlist(lapply(X = temp.enrollment$RelationshipToHoH, 
                                                    FUN = fun_rel2hoh))


# REAL DATA----
real.df <- left_join(temp.enrollment[,c("HouseholdID", "PersonalID", "EnrollmentID", 
                                     "RelationshipToHoH_def")], 
                     temp.client[,c("PersonalID", "age", 
                                 "VeteranStatus_def")]) 

rm(temp.client,temp.enrollment)

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
rm(i)

real.df <- real.df %>%
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



# minimum colums
real.df <- real.df[,c("PersonalID", "EnrollmentID", "HouseholdID", 
                      "out_hh_type")]
