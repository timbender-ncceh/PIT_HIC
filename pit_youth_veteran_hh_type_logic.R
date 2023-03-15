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

# note:  veterans are not alaways hoh so don't encode logic to account for hoh
# necessarily

# funs----

hh_pid_ages.v <- c(24,2,32)
age.hoh <- 32

get_youth.hh.info <- function(hh_pid_ages.v, age.hoh = NULL){
  require(dplyr)
  require(data.table)
  require(glue)
  require(crayon)
  # tidy
  # hh_pid_ages.v <- hh_pid_ages.v %>%
  #   .[order(.)]
  
  # error checking---- 
  
  # it's ok for age_of_hoh to be null if the HH size is one;
  # you can imply it.  otherwise it's required.
  if((is.null(age.hoh) & length(hh_pid_ages.v) > 1)){
    stop("age of Head of Household (age.hoh) must be provided when household size is greater than 1")
  }
  
  # calculate age.hoh
  if(is.null(age.hoh)){
    age.hoh <- hh_pid_ages.v
  }
  
  # possible flags----
  poss_flags <- NULL
  # age of hoh < max.age
  if(age.hoh < max(hh_pid_ages.v)){
    poss_flags <- c(poss_flags, "HoH younger than oldest household member")
  }
  
  # need to return
  is_youth_hh      <- c(T,F)
  youth_hh_type    <- c("Households without children (18+)",  # children =/= youth
                        "Households with at least 1 adult and 1 child (<18 & 18+ in one hh)",
                        "Households with only children (<18)")
  youth_hh_subtype <- c("Unaccompanied youth (18-24)",        # children =/= youth
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
  if(all(hh_pid_ages.v <= 24)) { # this number at 24 is correct for identifying the hh type
    is_youth_hh <- T
  }else{
    is_youth_hh <- F
  }
  
  # hh_type
  if(length(hh_pid_ages.v) > 1){
    # could be hh with at least 1 adult and 1 child, or [min(age) < 18 & max(age) >= 18 ]
    if(min(hh_pid_ages.v) < 18 & 
       max(hh_pid_ages.v) >= 18){
      youth_hh_type <- "Households with at least 1 adult and 1 child"
    }
    # hh without children, or [all ages >= 18] 
    if(all(hh_pid_ages.v >= 18)) {
      youth_hh_type <- "Households without children"
    }
    # hh with only children [all ages < 18 or]
    if(all(hh_pid_ages.v < 18)){
      youth_hh_type <- "Housholds with only children"
    }
  }else{
    # cannot be hh with at least 1 adult and 1 child
    if(min(hh_pid_ages.v) < 18 & 
       max(hh_pid_ages.v) >= 18){
      youth_hh_type <- "<ERROR>"
    }
    # either hh without children or [all ages >= 18]
    if(all(hh_pid_ages.v >= 18)) {
      youth_hh_type <- "Households without children"
    }
    # hh with only children [all ages < 18]
    if(all(hh_pid_ages.v < 18)){
      youth_hh_type <- "Housholds with only children"
    }
  }
  
  # HH Age Group
  
  # step 1: age grouping 
  # (18-24)
  if(all(between(hh_pid_ages.v, 18, 24))){
    ag18.24 <- T
  }else{
    ag18.24 <- F
  }
  # (<18 & 18-24 in one hh)
  if(any(hh_pid_ages.v < 18) & 
     any(between(hh_pid_ages.v, 18, 24)) & 
     all(hh_pid_ages.v <= 24)){
    ag18and18.24 <- T
  }else{
    ag18and18.24 <- F
  }
  # (<18) 
  if(all(hh_pid_ages.v < 18)){
    agU18 <- T
  }else{
    agU18 <- F
  }
  # (>=25)
  if(any(hh_pid_ages.v > 24)){
    ag25O <- T
  }else{
    ag25O <- F
  }
  
  out.ages <- data.frame(variable = c("(18-24)", 
                                      "(<18 & 18-24 in one hh)", 
                                      "(<18)", 
                                      "(>=25)"), 
                         value    = c(ag18.24, 
                                      ag18and18.24, 
                                      agU18, 
                                      ag25O)) %>% 
    as_tibble()
  
  # HH subtype----
  c("Unaccompanied youth (18-24)",
    "Parenting youth (18-24)",
    "Children of parenting youth",
    "Unaccompanied youth (<18 & 18-24 in one hh)",
    "Parenting youth (<18)",
    "Unaccompanied youth (<18)", 
    "Other people (>=25)", 
    "Other people (<18)", 
    "Other people (18-24)")
  
  temp.out.age <- out.ages$variable[out.ages$value]
 
  # error check----
  if(length(temp.out.age) > 1){
    stop("ERROR: var 'temp.out.age' should be length of 1")
  }
  # unaccompanied youth
  # if is_youth household
  if(is_youth_hh){
    if(youth_hh_type == "Households with at least 1 adult and 1 child"){
      youth_hh_subtype <- c("Parenting youth", "Children of parenting youth")
    }else{
      youth_hh_subtype <- NA # this needs to eventually go away.  it's a catch-all for missing logic
    }
    
    
    
  }else{
    # it's not is_youth and thus, is the "other people"
    if(temp.out.age %in%  c("(<18)", 
                            "(18-24)", 
                            "(<18 & 18-24 in one hh)")){
      youth_hh_subtype <- "<ERROR - not possible for house to be not_youth_hh but have all people under 25>"
    }
    if(temp.out.age == ("(>=25)")){
      youth_hh_subtype <- "Other people (>=25)"
    }
    
  }
  
  
  # generate output
  out <- list(hh.input.summary = data.frame(hh_size = length(hh_pid_ages.v), 
                                            hoh_age = age.hoh, 
                                            min_age = min(hh_pid_ages.v), 
                                            max_age = max(hh_pid_ages.v)),
              is.youth.hh      = is_youth_hh, 
              youth.hh.type    = youth_hh_type, 
              youth.hh.subtype = youth_hh_subtype, 
              youth.hh.agetype = out.ages$variable[out.ages$value],
              poss.flags       = poss_flags)
  
  # print inputs to help with debugging
  #cat(inverse(glue("AGES:\t{paste(hh_pid_ages.v,sep=\" \", collapse = \" \")}\nPARENT:\t{with_parent.child}\n\n")))
  cat(inverse(glue("AGES:\t{paste(hh_pid_ages.v,sep=\" \", collapse = \" \")}\nAGEHoH:\t{age.hoh}\n\n")))
  return(out)
  
}

args(get_youth.hh.info)
get_youth.hh.info(c(1,17,19), 17)


# testing----

# generate random household of ages and size
for(i in 1:10000){
  
  some.ages <- sample(1:45, size = sample(1:3, size = 1), replace = T)
  
  if(sum(some.ages>17) > 1){
    hohage <- sample(some.ages[some.ages > 17], size = 1)
  }else{
    hohage <- max(some.ages)
  }
  
  # if(length(some.ages) > 1){
  #   hohage <- max(some.ages)
  # }else{
  #   hohage <- NULL
  # }
  
  # decide (randomly, but with some logic) whether there is a parent-child
  # relationship in household
  #is.parentchild <- ifelse(length(some.ages) > 1 & any(some.ages > 17) & any(some.ages <= 17), sample(c(T,F), size = 1), F)
  
  # run script
  test.out <- get_youth.hh.info(hh_pid_ages.v = some.ages, 
                                age.hoh = hohage)
  
  if(is.na(test.out$youth.hh.type) | is.na(test.out$youth.hh.subtype)){
    print(i);get_youth.hh.info(hh_pid_ages.v = some.ages, 
                               age.hoh =  hohage)
    break
  }
}

get_youth.hh.info(some.ages, hohage)

# EXAMPLE 1:

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



# EXAMPLE 2:

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

# QA Quality Check on full dataset----

# to do:  when you run the full hmis data through this, complete a summary of
# outputs so we have an idea of relative sizes of each category as a quality
# check.
