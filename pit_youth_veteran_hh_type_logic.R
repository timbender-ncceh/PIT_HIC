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
  
  
  # generate output
  out <- list(is.youth.hh = is_youth_hh, 
           youth.hh.type = youth_hh_type, 
           youth.hh.subtype = youth_hh_subtype)
  
  # print inputs to help with debugging
  cat(inverse(glue("AGES:\t{paste(hh_pid_ages.v,sep=\" \", collapse = \" \")}\nPARENT:\t{with_parent.child}\n\n")))
  
  return(out)
  
}


# testing----

# generate random household of ages and size
some.ages <- sample(1:45, size = sample(1:3, size = 1), replace = T)

# decide (randomly, but with some logic) whether there is a parent-child
# relationship in household
is.parent <- ifelse(length(some.ages) > 1 & any(some.ages > 17) & any(some.ages <= 17), sample(c(T,F), size = 1), F)

# run script
get_youth.hh.info(hh_pid_ages.v = some.ages, 
                  with_parent.child = is.parent)

# EXAMPLE 1: ----

# AGES:	2 11 22
# PARENT:	FALSE
get_youth.hh.info(hh_pid_ages.v = c(2,11,22), 
                  with_parent.child = FALSE)

# RETURNS:
#
# $is.youth.hh
# [1] TRUE
# 
# $youth.hh.type
# [1] "Housholds with only children"
# 
# $youth.hh.subtype
# [1] "Unaccompanied youth (<18 & 18-24 in one hh)"



# EXAMPLE 2:----

# AGES:	7 36
# PARENT:	TRUE
get_youth.hh.info(hh_pid_ages.v = c(7,36), 
                  with_parent.child = TRUE)

# Returns: 
#
# $is.youth.hh
# [1] FALSE
# 
# $youth.hh.type
# [1] "Households with at least 1 adult and 1 child"
# 
# $youth.hh.subtype
# [1] NA  (not yet finished)
