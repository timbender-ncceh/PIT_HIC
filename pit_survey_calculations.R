

# calculated fields----

screened_positive_disability <- function(dr = c.disabilities$DisabilityResponse,
                                         ii=c.disabilities$IndefiniteAndImpairs) {
  # DisabilityResponse (0,1,2,8,9,99,NA)----
  # 1 = yes
  # 0 = no
  # 2,8,9,99,NA = "unknown or cannot tell"
  dr1 <- (dr == 1)
  
  # IndefiniteAndImpairs (0,1,2,8,9,99,NA)
  # 1 = yes
  # 0 = no
  # 2,8,9,99,NA = "unknown or cannot tell"
  ii1 <- (ii == 1)
  
  # Logic:
  # DisabiltyResponse & IndefiniteAndImpairs 
  
  return(dr1 & ii1)
  
  
  
  # 1 = "YES"
  # 0 = "NO"
  # NA = "UNKNOWN" 
  
  # DisabilityType----
}

screened_positive_disability() %>% table(., useNA = "ifany")


calc_age <- function(dob, decimal.month = F, age_on_date = ymd(20230125)){
  require(lubridate)
  require(dplyr)
  
  # check class of dob - expected to be class 'Date'.  If Character, change to
  # 'Date'
  if(!is.Date(dob)){
    if(is.numeric(dob)){
      dob <- as.character(dob)
    }
    dob <- as_date(dob)
  }
  age <- interval(dob,age_on_date) %>%
    as.period(., unit = "years")
  age.year    <- age@year
  age.decimal <- round(age@year + age@month/12,2)
  
  if(decimal.month) {
    out <- age.decimal
  }else{
    out <- age.year
  }
  return(out)
}

hud_age_category <- function(age_yrs, 
                             breaks_upper = c(17,24,34,44,54,64,126)){
  require(dplyr)
  require(data.table)
  
  bac <- data.frame(lower = 0, upper = breaks_upper[1])
  for(i in 2:length(breaks_upper)){
    bac <- rbind(bac, 
                 data.frame(lower = breaks_upper[i-1]+1, 
                            upper = breaks_upper[i]))
  }
  
  bac <- mutate(bac, 
                name = paste(lower, upper, sep = "-"))
  
  bac$in_cat <- between(x = rep(age_yrs, nrow(bac)), 
                        lower = bac$lower, upper = bac$upper)
  
  out <- bac[bac$in_cat,]$name
  return(out)
}

hud_age_category(41)

coc_region <- function(co_name){
  require(readr)
  cr_crosswalk <- read_tsv("County	Region
Alamance	Region 6
Alexander	Region 3
Anson	Region 7
Beaufort	Region 12
Bertie	Region 12
Bladen	Region 8
Burke	Region 3
Cabarrus	Region 5
Caldwell	Region 3
Camden	Region 11
Carteret	Region 13
Caswell	Region 6
Catawba	Region 3
Chatham	Region 6
Cherokee	Region 1
Chowan	Region 11
Clay	Region 1
Columbus	Region 8
Craven	Region 13
Currituck	Region 11
Dare	Region 11
Davidson	Region 5
Davie	Region 4
Duplin	Region 10
Edgecombe	Region 9
Franklin	Region 9
Gates	Region 11
Graham	Region 1
Granville	Region 9
Greene	Region 10
Halifax	Region 9
Harnett	Region 7
Haywood	Region 1
Henderson	Region 2
Hertford	Region 11
Hoke	Region 7
Hyde	Region 12
Iredell	Region 4
Jackson	Region 1
Johnston	Region 7
Jones	Region 13
Lee	Region 7
Lenoir	Region 10
Macon	Region 1
Madison	Region 1
Martin	Region 12
Mcdowell	Region 3
Montgomery	Region 7
Moore	Region 7
Nash	Region 9
Northampton	Region 9
Onslow	Region 13
Pamlico	Region 13
Pasquotank	Region 11
Perquimans	Region 11
Person	Region 6
Pitt	Region 12
Polk	Region 2
Randolph	Region 7
Richmond	Region 7
Robeson	Region 8
Rockingham	Region 6
Rowan	Region 5
Rutherford	Region 2
Sampson	Region 10
Scotland	Region 8
Stanly	Region 5
Stokes	Region 4
Surry	Region 4
Swain	Region 1
Transylvania	Region 2
Tyrrell	Region 11
Union	Region 5
Vance	Region 9
Warren	Region 9
Washington	Region 12
Wayne	Region 10
Wilson	Region 10
Yadkin	Region 4")
  out <- cr_crosswalk[cr_crosswalk$County == co_name,]$Region
  return(out)
}

coc_region("Yadkin")
