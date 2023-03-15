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

get_youth.hh.info <- function(hh_pid_ages.v, 
                              with_parent.child = F){
  require(dplyr)
  require(data.table)
  require(glue)
  require(crayon)
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
  
  # SUBTYPE
  c("Unaccompanied youth (18-24)",
    "Parenting youth (18-24)",
    "Children of parenting youth",
    "Unaccompanied youth (<18 & 18-24 in one hh)",
    "Parenting youth (<18)",
    "Unaccompanied youth (<18)", 
    "Other people (>=25)", 
    "Other people (<18)", 
    "Other people (18-24)")
  
  
  
  if(all(between(x = hh_pid_ages.v, 
             lower = 18, 
             upper = 24))){
    if(length(hh_pid_ages.v) > 1 & 
       with_parent.child == T){
      youth_hh_subtype <- "Unaccompanied youth (18-24)"
    }else{
      youth_hh_subtype <- "Unaccompanied youth (18-24)"
    }
    
  }
  if(all(hh_pid_ages.v <= 25)){
    youth_hh_subtype <- "Unaccompanied youth (<18 & 18-24 in one hh)"
  }
  if(all(hh_pid_ages.v < 18)){
    youth_hh_subtype <- "Unaccompanied youth (<18)"
  }
  
  
  
  
  
  out <- list(is.youth.hh = is_youth_hh, 
           youth.hh.type = youth_hh_type, 
           youth.hh.subtype = youth_hh_subtype)
  
  cat(inverse(glue("AGES:\t{paste(hh_pid_ages.v,sep=\" \", collapse = \" \")}\nPARENT:\t{with_parent.child}\n\n")))
  
  return(out)
  
}

some.ages <- sample(1:45, size = sample(1:3, size = 1), replace = T)
is.parent <- ifelse(length(some.ages) > 1 & any(some.ages > 17) & any(some.ages <= 17), sample(c(T,F), size = 1), F)

get_youth.hh.info(hh_pid_ages.v = some.ages, 
                  with_parent.child = is.parent)











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
  # make 'of' lowercase
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



