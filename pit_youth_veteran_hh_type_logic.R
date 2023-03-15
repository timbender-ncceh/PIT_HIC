# veteran_type and 
# youth_type households


# Libraries----
library(dplyr)
library(readr)
library(glue)
library(lubridate)
library(data.table)
library(devtools)
library(openxlsx)
library(stringr)
library(igraph)
library(ggplot2)

rm(list=ls())
cat('\f')
gc()

# funs----

get_youth.hh.info <- function(hh_pid_ages.v){
  # tidy
  hh_pid_ages.v <- hh_pid_ages.v %>%
    .[order(.)]
  
  # need to return
  is_youth_hh      <- c(T,F)
  youth_hh_type    <- c("Households without children",
                        "Households with at least 1 adult and 1 child",
                        "Households with only children")
  youth_hh_subtype <- c("Unaccompanied youth (18-24)",
                        "Parenting youth (18-24)",
                        "Children of parenting youth",
                        "Unaccompanied youth (<18 & 18-24 in one hh)",
                        "Parenting youth (<18)",
                        "Unaccompanied youth (<18)", 
                        "Other people (>=25)", 
                        "Other people (<18)", 
                        "Other people (18-24)")
  is_youth_hh      <- NA
  youth_hh_subtype <- NA
  youth_hh_type <- NA
  
  # is_youth_hh
  if(min(hh_pid_ages.v) <= 24 & 
     max(hh_pid_ages.v) <= 24) {
    is_youth_hh <- T
  }else{
    is_youth_hh <- F
  }
  
  # hh_type
  if(length(hh_pid_ages.v) > 1){
    # could be hh with at least 1 adult and 1 child, or [min(age) < 25 & max(age) >= 25 ]
    if(min(hh_pid_ages.v) < 25 & 
       max(hh_pid_ages.v) >= 25){
      youth_hh_type <- "Households with at least 1 adult and 1 child"
    }
    # hh without children, or [all ages >= 25] 
    if(all(hh_pid_ages.v >= 25)) {
      youth_hh_type <- "Households without children"
    }
    # hh with only children [all ages < 25 or]
    if(all(hh_pid_ages.v < 25)){
      youth_hh_type <- "Housholds with only children"
    }
  }else{
    # cannot be hh with at least 1 adult and 1 child
    if(min(hh_pid_ages.v) < 25 & 
       max(hh_pid_ages.v) >= 25){
      youth_hh_type <- "<ERROR>"
    }
    # either hh without children or [all ages >= 25]
    if(all(hh_pid_ages.v >= 25)) {
      youth_hh_type <- "Households without children"
    }
    # hh with only children [all ages < 25]
    if(all(hh_pid_ages.v < 25)){
      youth_hh_type <- "Housholds with only children"
    }
  }
  
  print(hh_pid_ages.v)
  
  out <- c(is.youth.hh = is_youth_hh, 
           youth.hh.type = youth_hh_type, 
           youth.hh.subtype = youth_hh_subtype)
  
  return(out)
  
}

get_youth.hh.info(hh_pid_ages.v = sample(1:45, size = sample(1:3, size = 1), replace = T))











make_abbr <- function(string = "go on without and me"){
  require(dplyr)
  # returns an abbreviation for a multi-word string in by taking the first
  # letter of each word, making them upper-case and pasting together to form a
  # single word string output
  
  # error checking
  if(length(string) != 1){
    stop("Arg \"string\" must be exactly length() 1")
  }
  
  out <- strsplit(string, " ") %>%
    unlist() 
  # special case: 'and' to '&'
  out[out == "and"] <- "&"
  out <- out %>%
    strsplit(., "") %>%
    lapply(., first) %>% 
    unlist() 
  out <- out %>% toupper()
  
  # special cases----
  
  
  
  
  # # make 'of' lowercase
  # if(grepl("\\bof\\b", string, ignore.case = T)){
  #   of.which <- strsplit(x = string, split = " ") %>%
  #     unlist() %>% tolower() 
  #   of.which <- which(of.which == "of")
  #   out[of.which] <- "o"
  # }
  
  # without to w/o (lowercase)
  if(grepl("\\bwithout\\b", string, ignore.case = T)){
    without.which <- strsplit(x = string, split = " ") %>%
      unlist() %>% tolower() 
    without.which <- which(without.which == "without")
    out[without.which] <- "w/o"
  }
  out <- out %>% paste(., sep = "", collapse = "") 
  return(out)
}



# define youth household types and household subtypes----

hh_youth_types <- expand.grid(hh_category = c("youth type", "not youth type"), 
                              hh_type     = c("Households without children",
                                              "Households with at least 1 adult and 1 child",
                                              "Households with only children"),
                              hh_subtype  = c("Unaccompanied youth (18-24)",
                                              "Parenting youth (18-24)",
                                              "Children of parenting youth",
                                              "Unaccompanied youth (<18 & 18-24 in one hh)",
                                              "Parenting youth (<18)",
                                              "Unaccompanied youth (<18)", 
                                              "Other people (>=25)", 
                                              "Other people (<18)", 
                                              "Other people (18-24)"),
                              stringsAsFactors = F) %>%
  as_tibble()

hh_youth_types$hhcategory.abbr <- unlist(lapply(X = hh_youth_types$hh_category, FUN = make_abbr))
hh_youth_types$hhtype.abbr     <- unlist(lapply(X = hh_youth_types$hh_type, FUN = make_abbr)) 
hh_youth_types$hhsubtype.abbr  <- unlist(lapply(X = hh_youth_types$hh_subtype, FUN = make_abbr)) %>%
  gsub(pattern = "\\(.*$", replacement = "", x = .)

hh_youth_types.archive <- hh_youth_types

hh_youth_types$age_U18    <- grepl(pattern = "<18", x = hh_youth_types$hh_subtype)
hh_youth_types$age_18to25 <- grepl(pattern = "18-24", x = hh_youth_types$hh_subtype)
hh_youth_types$age_25PLUS <- grepl(pattern = ">=25", x = hh_youth_types$hh_subtype)


# evaluate for category, type and subtype matches

# is_youth_cat
hh_youth_types$is_youth.cat <- NA

library(igraph)

a.flow.gr <- data.frame(from = c("is youth category?"), 
                        to   = c()) %>%
  graph_from_data_frame()

# hh_youth_types <- hh_youth_types[!colnames(hh_youth_types) %in% 
#                                    c("hh_category","hh_type", "hh_subtype")]

