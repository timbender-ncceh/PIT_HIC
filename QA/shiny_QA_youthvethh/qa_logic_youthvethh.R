# Funs here----
hh_age.unknown <- function(ages){
  # if any household member has an age value of NA
  if(any(is.na(ages))){
    out <- T
  }else{
    out <- F
  }
  return(out)
}

hh_wal1a1c <- function(ages, 
                       age.upperlim=18){
  #Persons in households with at least one adult and one child (HH-type 1). This
  #category includes households with one adult and at least one child under age
  #18.
  if(any(is.na(ages))){
    out <- F
  }else{
    # must have 1+ adult 
    out <- sum(ages >= age.upperlim) >= 1 & 
      # must have 1+ child
      sum(ages < age.upperlim) >= 1
  }
  return(out)
}


hh_wo.c <- function(ages, age.upperlim = 18){
  # WITHOUT CHILDREN
  #This category includes single adults, adult couples with no children, and
  #groups of adults (including adult parents with their adult children).
  if(any(is.na(ages))){
    out <- F
  }else{
    # out <- sum(ages >= age.upperlim) >= 1 & 
    #   sum(ages < age.upperlim) == 0
    out <- all(ages >= age.upperlim)
  }
  return(out)
}

hh_w.o.C <- function(ages, age.upperlim = 18){
  # WITH ONLY CHILDREN
  #This category includes persons under age 18, including children in one-child
  #households, adolescent parents (under age 18) and their children, adolescent
  #siblings, or other household configurations composed only of children.
  if(any(is.na(ages))){
    out <- F
  }else{
    if(all(ages < age.upperlim)){
      out <- T
    }else{
      out <- F
    }
  }
  return(out)
}

py_u18 <- function(ages, rel2hohs){
  #are youth who identify as the parent or legal guardian of one or more
  #children who are present with or sleeping in the same place as that youth
  #parent, where there is no person over age 24 in the household. Parenting
  #youth are either a subset of households with at least one adult and one child
  #if the parenting youth is between 18 and 24, or households with only children
  #if the parenting youth is under 18. CoCs should report the numbers of
  #children in parenting youth households separately for households with
  #parenting youth under 18 and households with parenting youth who are 18 to
  #24.
  
  age_ul=18
  
  if(any(is.na(ages))){
    out <- F
  }else{
    
    out <- #between(max(ages), 18, age_ul) & 
      all(ages < 18) &  
      # rel2hoh as child
      any(grepl(pattern = "^Head of Household's child$",
                x = rel2hohs), 
          na.rm = T)
  }
  return(out)
}

py_18.24 <- function(ages, rel2hohs, 
                     age_ul=24){
  require(data.table)
  #are youth who identify as the parent or legal guardian of one or more
  #children who are present with or sleeping in the same place as that youth
  #parent, where there is no person over age 24 in the household. Parenting
  #youth are either a subset of households with at least one adult and one child
  #if the parenting youth is between 18 and 24, or households with only children
  #if the parenting youth is under 18. CoCs should report the numbers of
  #children in parenting youth households separately for households with
  #parenting youth under 18 and households with parenting youth who are 18 to
  #24.
  if(any(is.na(ages))){
    out <- F
  }else{
    # oldest hh member is 18-24
    out <- between(max(ages), 18, age_ul) & 
      # max age <= 24
      max(ages) <= 24 & 
      # min age < 18
      min(ages) < 18 &
      
      # rel2hoh as child
      any(grepl(pattern = "^Head of Household's child$",
                x = rel2hohs), 
          na.rm = T)
  }
  return(out)
}

uy <- function(ages1, rel2hohs1){
  #are persons under age 25 who are not presenting or sleeping in the same place
  #as their parent or legal guardian, any household member over age 24, or their
  #own children. Unaccompanied youth may be a subset of any household type: they
  #are a subset of households without children if all household members are 18
  #to 24. They are a subset of households with at least one adult and one child
  #if the household includes at least one household member under 18, at least
  #one member between 18 and 24, and no members over age 24. They are a subset
  #of households with only children if all household members are under 18.
  age.upperlim = 24
  if(any(is.na(ages1)) | 
     # this cannot be households 25+
     any(ages1 > 24) | 
     # this cannot be age_NA
     any(is.na(ages1)) | 
     # this cannot be... pu
     py_18.24(ages=ages1, rel2hohs=rel2hohs1)|
     py_u18(ages=ages1,rel2hohs=rel2hohs1)){
    out <- F
  }else{
    if(all(ages1 <= 24)){
      out <- T
    }else{
      out <- F
    }
  }
  return(out)
}
hh_vet <- function(vet.statuses){
  #Households with one or more veterans who might be presenting with other
  #persons.
  if(any(vet.statuses == "Yes", na.rm = T) |
     any(vet.statuses == T, na.rm = T) |
     any(vet.statuses == 1, na.rm = T)){
    out <- "Yes"
  }else{
    out <- "No"
  }
  return(out)
}
hh_youth <- function(ages, age.upperlim = 24){
  # all household members are 24 or under
  if(any(is.na(ages))){
    out <- F
  }else{
    if(all(ages <= age.upperlim)){
      out <- T
    }else{
      out <- F
    }
  }
  return(out)
}
