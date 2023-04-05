library(devtools)
library(dplyr)


rm(list=ls());cat('\f');gc()

# setwd----
a.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023"
d.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data"
setwd(a.wd)

# Vars----
vars_QA <- c("hh_age.unknown", 
             "hh_wal1a1c", 
             "hh_wo.c",
             "hh_w.o.C",
             "py_u18",
             "py_18.24",
             "uy", 
             "hh_vet",
             "hh_youth")

# Load Funs----
vars_prior <- ls()
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev/working_files/pit_survey_calculations.R?raw=TRUE")
vars_funs <- ls()[!ls() %in% vars_prior]
vars_post <- c(vars_prior, vars_QA)

# Remove vars----
rm(list = ls()[!ls() %in% c(vars_post)])
vars_post <- ls()

# Load Data----
setwd(d.wd)

files_all        <- list.files()
files_is.csv     <- grepl(pattern = "\\.csv$", files_all)
files_is.FLupper <- unlist(lapply(strsplit(files_all, ""), first)) %in% LETTERS
files_csv        <- files_all[files_is.csv & files_is.FLupper]

# Remove vars----
rm(list = ls()[!ls() %in% c(vars_post, "files_csv")])
vars_post <- ls()
