library(devtools)
library(dplyr)
library(readr)

rm(list=ls()[!ls() %in% 
               c("client", 
                 "enrollment")]);cat('\f');gc()

# setwd----
a.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023"
d.wd <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data"
setwd(a.wd)

# Vars----
pit.night     <- ymd(20230125)
vars_QA       <- c("hh_age.unknown", 
                   "hh_wal1a1c", 
                   "hh_wo.c",
                   "hh_w.o.C",
                   "py_u18",
                   "py_18.24",
                   "uy", 
                   "hh_vet",
                   "hh_youth", 
                   "calc_age", 
                   "hud_age_category", 
                   "fun_rel2hoh",
                   "fun_1.8_def")


# Load Funs----
vars_prior <- ls()
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev/working_files/pit_survey_calculations.R?raw=TRUE")
vars_funs <- ls()[!ls() %in% vars_prior]
vars_post <- c(vars_prior, vars_QA)

# Remove vars----
rm(list = ls()[!ls() %in% c(vars_post)])
vars_post <- ls()

# Identify Data----
setwd(d.wd)

files_all        <- list.files()
files_is.csv     <- grepl(pattern = "\\.csv$", files_all)
files_is.FLupper <- unlist(lapply(strsplit(files_all, ""), first)) %in% LETTERS
files_csv        <- files_all[files_is.csv & files_is.FLupper]

# Remove vars----
rm(list = ls()[!ls() %in% c(vars_post, "files_csv")])
vars_post <- ls()

# Load and Tidy Data----
# client
if(!"client" %in% ls()){
  client     <- read_csv("Client.csv")
}

if("client" %in% ls()){
  if(!"age" %in% names(client)){
    client$age         <- unlist(lapply(X = client$DOB, 
                                        FUN = calc_age, 
                                        age_on_date = pit.night))
  }
  if(!"hud_age_cat" %in% names(client)){
    client$hud_age_cat <- unlist(lapply(X = client$age, 
                                        FUN = hud_age_category))
  }
  if(!"VeteranStatus_def" %in% names(client)){
    client$VeteranStatus_def <- unlist(lapply(X = client$VeteranStatus, 
                                              FUN = fun_1.8_def))
  }
}

# enrollment
if(!"enrollment" %in% ls()){
  enrollment <- read_csv("Enrollment.csv")
}
if(!"RelationshipToHoH_def" %in% names(enrollment)){
  enrollment$RelationshipToHoH_def <- unlist(lapply(X = enrollment$RelationshipToHoH, 
                                                    FUN = fun_rel2hoh))
}




