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
yvhh.df <- left_join(temp.enrollment[,c("HouseholdID", "PersonalID", "EnrollmentID", 
                                     "RelationshipToHoH_def")], 
                     temp.client[,c("PersonalID", "age", 
                                 "VeteranStatus_def")]) %>%
  mutate(., 
         hh1_age.unknown = NA,
         hh1_vet         = NA,
         hh1_w.o.C       = NA,
         hh1_wal1a1c     = NA,
         hh1_wo.c        = NA,
         hh1_youth       = NA,
         py1_18.24       = NA,
         py1_u18         = NA,
         uy1             = NA)

rm(temp.client,temp.enrollment)



for(i in unique(yvhh.df$HouseholdID)){
 #print(i)
  if(!is.na(i)){
    yvhh.df$hh1_age.unknown[yvhh.df$HouseholdID == i] <- hh_age.unknown(ages = yvhh.df$age[yvhh.df$HouseholdID == i])
    yvhh.df$hh1_vet[yvhh.df$HouseholdID == i]         <- hh_vet(vet.statuses = yvhh.df$VeteranStatus_def[yvhh.df$HouseholdID == i])
    yvhh.df$hh1_w.o.C[yvhh.df$HouseholdID == i]       <- hh_w.o.C(ages = yvhh.df$age[yvhh.df$HouseholdID == i])
    yvhh.df$hh1_wal1a1c[yvhh.df$HouseholdID == i]     <- hh_wal1a1c(ages = yvhh.df$age[yvhh.df$HouseholdID == i])
    yvhh.df$hh1_wo.c[yvhh.df$HouseholdID == i]        <- hh_wo.c(ages = yvhh.df$age[yvhh.df$HouseholdID == i])
    yvhh.df$hh1_youth[yvhh.df$HouseholdID == i]       <- hh_youth(ages = yvhh.df$age[yvhh.df$HouseholdID == i])
    yvhh.df$py1_18.24[yvhh.df$HouseholdID == i]       <- py_18.24(ages = yvhh.df$age[yvhh.df$HouseholdID == i], 
                                                                  rel2hohs = yvhh.df$RelationshipToHoH_def[yvhh.df$HouseholdID == i])
    yvhh.df$py1_u18[yvhh.df$HouseholdID == i]         <- py_u18(ages = yvhh.df$age[yvhh.df$HouseholdID == i], 
                                                                rel2hohs = yvhh.df$RelationshipToHoH_def[yvhh.df$HouseholdID == i])
    yvhh.df$uy1[yvhh.df$HouseholdID == i]             <- uy(ages = yvhh.df$age[yvhh.df$HouseholdID == i], 
                                                            rel2hohs = yvhh.df$RelationshipToHoH_def[yvhh.df$HouseholdID == i])
  }
}
rm(i)

yvhh.df <- yvhh.df %>%
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
yvhh.df <- yvhh.df[,c("PersonalID", "EnrollmentID", "HouseholdID", 
                      "out_hh_type")]
