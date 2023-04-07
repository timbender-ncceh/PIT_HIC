library(devtools)
library(dplyr)

# creates an dataframe output that shows clients who are living in a household
# with at least 1 person who has a CH condition

# Run pit_MODULE_chronicallyhomeless_ind.R ----
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev/working_files/pit_MODULE_chronicallyhomeless_ind.R?raw=TRUE")

# set CH.status.ind as tempvar----
temp.csi <- CH.status.ind %>%
  .[.$ChronicStatus == "Chronic" & 
      !is.na(.$ChronicStatus),]


# get needed data from output2A----
temp.o2A <- output2A[,c("PersonalID", "HouseholdID", "EntryDate")] %>%
  mutate(., 
         EntryDate_char = as.character(EntryDate)) %>%
  .[!colnames(.) %in% ("EntryDate")] 


CH_hhid <- inner_join(temp.o2A, 
             temp.csi)$HouseholdID %>% unique()

# CH.status.byHH <- temp.o2A[,c("PersonalID", "HouseholdID")] %>%
#   mutate(., 
#          pid_in_HH_wal1_CH = NA)
# 
# CH.status.byHH$pid_in_HH_wal1_CH <- CH.status.byHH$HouseholdID %in% 
#   CH_hhid
# 
# # remove duplicates
# CH.status.byHH <- CH.status.byHH[!duplicated(CH.status.byHH),]
# 
# CH.status.byHH %>%
#   group_by(PersonalID) %>%
#   summarise(n_hhid = n_distinct(HouseholdID), 
#             n_hh.with.ch = sum(pid_in_HH_wal1_CH)) %>%
#   .[order(.$n_hh.with.ch, decreasing = T),] %>%
#   group_by(two.plus_hh = n_hhid >= 2) %>%
#   summarise(n_pid = n_distinct(PersonalID)) %>%
#   ungroup() %>%
#   mutate(., 
#          pct_pid = n_pid/sum(n_pid))

# cleanup----
rm(temp.o2A,
   # CH_hhid,
   temp.csi)
