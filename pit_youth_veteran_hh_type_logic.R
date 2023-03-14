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


# function----
df1 <- data.frame(is_HoH  = c(F), 
                  age_yrs = c(sample(c(16,22,26), size = 3, replace = T)), 
                  is_vet  = c(F))
df1[order(df1$age_yrs,decreasing = T),]$is_HoH[1] <- T

df1 <- df1 %>%
  mutate(., 
         is_vet = ifelse(test = is_HoH == T & age_yrs >= 22, 
                         yes  = sample(c(T,F), size = 1), 
                         no   = is_vet))


# get_hh_type <- function(df1){
#   # Deal with HoH First 
#   
# }


# # logic flow of figuring out hh_type
# 
# graph_from_literal(`HoH\n(exactly 1)`--+`VET: FALSE or NA`--+`AGE: <18`--+`HH.SUBTYPE\nunaccompanied_youth`--+`HH.TYPE\nyouth_hh`, 
#                    `other_ppl\n(0 or more)`--+`VET: FALSE or NA`--+`AGE: <18`) %>% plot.igraph()
# 

all.poss.hh <- expand.grid(category                   = c("youth_type",
                                                          "vet_type",
                                                          "regular_type"),
                           hh_type                    = c("hh without children",
                                                          "hh with at least 1 adult and 1 child",
                                                          "hh with only children"),
                           hh_subtype                 = c("unaccompanied youth",
                                                          "parenting youth",
                                                          "children of parenting youth",
                                                          "everyone else",
                                                          "veteran"),
                           hh_size                    = c("1", "2+"),
                           hh_age_group               = c("<18",
                                                          "18-24",
                                                          #"18+",
                                                          "25+"), 
                           stringsAsFactors = F) %>%
  as_tibble()

all.poss.hh$rid <- 1:nrow(all.poss.hh)


all.poss.hh$is_possible <- NA

# level 1 possible

# level 1 impossible
all.poss.hh$is_possible[all.poss.hh$category == "youth_type" & 
                          all.poss.hh$hh_age_group == "25+"] <- F
all.poss.hh$is_possible[all.poss.hh$category == "regular_type" & 
                          all.poss.hh$hh_subtype == "veteran"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with only children" & 
                          all.poss.hh$hh_subtype == "veteran"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_subtype %in% 
                          c("children of parenting youth", 
                            "parenting youth") & 
                          all.poss.hh$hh_size == "1"] <- F
# level 2 possible

# level 2 impossible

all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with at least 1 adult and 1 child" & 
                          all.poss.hh$hh_subtype == "parenting youth" & 
                          !all.poss.hh$hh_age_group %in% "25+"] <- T
all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with at least 1 adult and 1 child" & 
                          all.poss.hh$hh_subtype == "parenting youth" & 
                          !all.poss.hh$hh_age_group %in% "25+" & 
                          all.poss.hh$category == "regular_type"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with at least 1 adult and 1 child" & 
                          all.poss.hh$hh_subtype == "parenting youth" & 
                          all.poss.hh$hh_age_group %in% "25+"] <- F

all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with at least 1 adult and 1 child" & 
                          all.poss.hh$hh_subtype == "children of parenting youth" & 
                          !all.poss.hh$hh_age_group %in% "25+"] <- T
all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with at least 1 adult and 1 child" & 
                          all.poss.hh$hh_subtype == "children of parenting youth" & 
                          !all.poss.hh$hh_age_group %in% "25+" & 
                          all.poss.hh$category == "regular_type"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with at least 1 adult and 1 child" & 
                          all.poss.hh$hh_subtype == "children of parenting youth" & 
                          all.poss.hh$hh_age_group %in% "25+"] <- F


all.poss.hh$is_possible[all.poss.hh$hh_type == "hh with at least 1 adult and 1 child" & 
                          all.poss.hh$hh_subtype == "everyone else"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_type == "hh without children" & 
                          all.poss.hh$hh_age_group == "18-24" & 
                          all.poss.hh$category == "youth_type" & 
                          all.poss.hh$hh_size == "2+"] <- T

all.poss.hh$is_possible[all.poss.hh$hh_type == "hh without children" & 
                          all.poss.hh$hh_age_group == "18-24" & 
                          all.poss.hh$category == "youth_type" & 
                          all.poss.hh$hh_size == "1" & 
                          all.poss.hh$hh_subtype %in% 
                          c("veteran", "unaccompanied youth", 
                            "everyone else")] <- T
all.poss.hh$is_possible[all.poss.hh$hh_subtype == "unaccompanied youth" & 
                          all.poss.hh$hh_age_group != "25+" & 
                          !all.poss.hh$hh_type %in% c("hh without children") & 
                          all.poss.hh$category == "youth_type"] <- T

all.poss.hh$is_possible[all.poss.hh$hh_subtype == "unaccompanied youth" & 
                          all.poss.hh$hh_age_group != "25+" & 
                          !all.poss.hh$hh_type %in% c("hh without children") & 
                          !all.poss.hh$category == "youth_type"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_subtype == "unaccompanied youth" & 
                          all.poss.hh$hh_age_group == "25+"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_age_group == "25+" & 
                          all.poss.hh$hh_type == "hh with only children"] <- F

all.poss.hh$is_possible[all.poss.hh$hh_subtype == "everyone else" & 
                          all.poss.hh$hh_type == "hh with only children"]

all.poss.hh$is_possible[all.poss.hh$hh_subtype %in%  c("parenting youth", 
                                                       "children of parenting youth") &
                          all.poss.hh$hh_age_group == "25+"] <- F
all.poss.hh$is_possible[all.poss.hh$hh_subtype %in%  c("parenting youth", 
                                                       "children of parenting youth") &
                          all.poss.hh$hh_size == "1"] <- F

all.poss.hh$is_possible[all.poss.hh$hh_type %in%  c("hh with at least 1 adult and 1 child") &
                          all.poss.hh$hh_size == "1"] <- F

all.poss.hh$is_possible[all.poss.hh$hh_subtype == "everyone else" & 
                          all.poss.hh$hh_type == "hh with only children" & 
                          all.poss.hh$category == "youth_type"] <- F

all.poss.hh$is_possible[all.poss.hh$hh_subtype == "everyone else" & 
                          all.poss.hh$category == "vet_type"] <- F

ggplot() +
  geom_point(data = all.poss.hh[all.poss.hh$is_possible == T | 
                                  is.na(all.poss.hh$is_possible),], 
             size = 5,
             aes(x = category, y = hh_age_group, color = is_possible)) +
   geom_text(data = all.poss.hh, size = 2,
             aes(x = category, y = hh_age_group, label = rid))+
  facet_grid(hh_size~hh_type+hh_subtype, scales = "free", space = "free")+
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1), 
        strip.text.x = element_text(angle = 90))


hh_info <- function(hoh_age, 
                    hoh_is.veteran, 
                    max_pid_age, 
                    min_pid_age,
                    n_pid){
  # error checking
  if(n_pid < 1 | !is.logical(hoh_is.veteran)){
    stop("n_pid must be larger than 0")
  }
  
  # # output setup
  # hh.veteran <- NA
  # hh.youth   <- NA
  # hh.regular <- NA
  # 
  # hhst.PY         <- NA
  # hhst.kids_of_PY <- NA
  # hhst.UAY        <- NA
  # hhst.VET        <- NA
  
  # Logic 
  if(n_pid == 1){
    # household size 1
    hhst.kids_of_PY <- F
    hhst.PY         <- F
    if(hoh_age < 18){
      # and age < 18
      hh.regular <- F
      hh.veteran <- F
      hhst.UAY   <- T
      if(hoh_is.veteran == T){
        # and veteran = TRUE
        stop("can't have veteran aged < 18")
      }else{
        # else veteran = FALSE
        hhst.VET <- F
        hh.youth <- T
        hhst.UAY   <- T
      }
    }else if(hoh_age >= 18 & hoh_age < 25){
      # else age between 18 & 24
      hh.regular <- F
      hh.youth   <- T
      hhst.UAY   <- T
      if(hoh_is.veteran == T){
        # and veteran = TRUE
        hh.veteran <- T
        hhst.VET    <- T
      }else{
        # else veteran = FALSE
        hh.veteran <- F
        hhst.VET   <- F
      }
    }else if(hoh_age >= 25){
      #else age >= 25
      hh.regular <- T
      hh.youth   <- F
      hhst.UAY   <- F
      if(hoh_is.veteran == T){
        # and veteran = TRUE
        hh.veteran <- T
        hhst.VET   <- T
      }else{
        # else veteran = FALSE
        hh.veteran <- F
        hhst.VET   <- F
      }
    }else{
      # else [something has been overlooked]
      stop("logically speaking - something has been overlooked")
    }
    
  }else{
    #  household size > 1
    
    # hh.veteran
    if(hoh_is.veteran){
      hh.veteran <- T
    }else{
      hh.veteran <- F
    }
    
    # hh.youth
    if(hoh_age <= 25 & max_pid_age <= 25){
      hh.youth <- T
    }else{
      hh.youth <- F
    }
    
    # hh.regular
    if(!(hh.veteran == T | hh.youth == T)){
      hh.regular <- T
    }else{
      hh.regular <- F
    }
    
    # hhst.PY
    if(hoh_age <= 25 & max_pid_age <= 25 & 
       min_pid_age <= 17){
      hhst.PY <- T
    }else{
      hhst.PY <- F
    }
    
    # hhst.kids_of_PY
    if(hhst.PY){
      hhst.kids_of_PY <- T
    }else{
      hhst.kids_of_PY <- F
    }
    
    # hhst.UAY 
    if(max_pid_age <= 25){
      hhst.UAY <- T
    }else{
      hhst.UAY <- F
    }
    
    # hhst.VET
    if(hoh_is.veteran){
      hhst.VET <- T
    }else{
      hhst.VET <- F
    }
    
    
  }
  
  out <- data.frame(t(data.frame(hh.veteran = hh.veteran,
                                 hh.youth = hh.youth,
                                 hh.regular = hh.regular, 
                                 hhst.PY = hhst.PY, 
                                 hhst.kids_of_PY = hhst.kids_of_PY, 
                                 hhst.UAY = hhst.UAY, 
                                 hhst.VET = hhst.VET)))
  colnames(out) <- "value"
  out$variable <- rownames(out)
  rownames(out) <- 1:nrow(out)
  out <- out[,c(2,1)] %>% as_tibble()
  return(out)
}

library(glue)
hoh_age1        <- sample(c(17,19,26), size = 1)
hoh_is.veteran1 <- ifelse(hoh_age1 < 22, F, sample(c(T,F), size = 1))
max_pid_age1    <- hoh_age1
n_pid1          <- sample(1:3, size = 1)
min_pid_age1    <- ifelse(n_pid1 == 1, hoh_age1, sample(1:(hoh_age1-1), size = 1))

cat(glue("\f> hh_info({hoh_age1},{hoh_is.veteran1},{max_pid_age1},{min_pid_age1},{n_pid1})"))
hh_info(hoh_age1,hoh_is.veteran1, max_pid_age1, min_pid_age1, n_pid1)

# failing logic----

make_hhinfo_input <- function(){
  hoh_age1        <- sample(8:45,size=1)#sample(c(17,19,26), size = 1)
  hoh_is.veteran1 <- ifelse(hoh_age1 < 22, F, sample(c(T,F), size = 1))
  max_pid_age1    <- hoh_age1
  n_pid1          <- sample(1:3, size = 1)
  min_pid_age1    <- ifelse(n_pid1 == 1, hoh_age1, sample(1:(hoh_age1-1), size = 1))
  
  out <- data.frame(hoh_age2 = hoh_age1, 
                    hoh_is.veteran2 = hoh_is.veteran1, 
                    max_pid_age2 = max_pid_age1, 
                    n_pid2 = n_pid1, 
                    min_pid_age2 = min_pid_age1)
  return(out)
}

temp1 <- replicate(n = 100, make_hhinfo_input(), 
          simplify = "list") %>% 
  as.data.frame() %>% 
  t %>% as.data.frame() %>%
  as_tibble()


temp1$hoh_age2        <- unlist(temp1$hoh_age2) 
temp1$hoh_is.veteran2 <- unlist(temp1$hoh_is.veteran2)
temp1$max_pid_age2    <- unlist(temp1$max_pid_age2)
temp1$n_pid2          <- unlist(temp1$n_pid2)
temp1$min_pid_age2    <- unlist(temp1$min_pid_age2)


temp1
temp1 <- temp1[!duplicated(temp1),]

temp1 <- temp1 %>%
  .[order(.$n_pid2,.$hoh_age2,.$min_pid_age2),]

temp1$rid <- 1:nrow(temp1)

temp1$age_veteran <- 22
temp1$age_25_and_half   <- 25.5



ggplot() + 
  geom_segment(data = temp1, 
               aes(x = min_pid_age2, xend = max_pid_age2, 
                   y = rid, yend = rid, 
                   #color = max_pid_age2 <= 25)) + 
                   color = ifelse(max_pid_age2 <= 25, "25 years or under", "over 25 years old"))) + 
  geom_point(data = temp1, 
             aes(x = hoh_age2, y = rid, 
                 #color = max_pid_age2 <= 25)) + 
                 color = ifelse(max_pid_age2 <= 25, "25 years or under", "over 25 years old"))) + 
  geom_vline(data = temp1, 
             aes(xintercept = age_veteran, 
                 color = "minimum age\nVeteran")) +
  geom_vline(data = temp1, 
             aes(xintercept = age_25_and_half, 
                 color = "cutoff adult"))+
  facet_grid(unname(ifelse(temp1$n_pid2 == 1, "1 person HH", 
                           ifelse(temp1$n_pid2 == 2, "2 person HH", "3 person HH")))~
               ifelse(hoh_is.veteran2, "Veteran HoH", "Not Veteran HoH"),
             scales = "free_y", space = "free_y") +
  theme(legend.position = "bottom", legend.direction = "vertical")+
  scale_color_discrete(name = "Legend")

temp1$hh.veteran      <- NA
temp1$hh.youth        <- NA
temp1$hh.regular      <- NA
temp1$hhst.PY         <- NA
temp1$hhst.kids_of_PY <- NA
temp1$hhst.UAY        <- NA
temp1$hhst.VET        <- NA

for(i in 1:nrow(temp1)){
  temp <- hh_info(temp1$hoh_age2[i], 
          temp1$hoh_is.veteran2[i], 
          temp1$max_pid_age2[i], 
          temp1$n_pid2[i], 
          temp1$min_pid_age2[i])
  temp1$hh.veteran[i]      <- temp$value[1]
  temp1$hh.youth[i]        <- temp$value[2]
  temp1$hh.regular[i]      <- temp$value[3]
  temp1$hhst.PY[i]         <- temp$value[4]
  temp1$hhst.kids_of_PY[i] <- temp$value[5]
  temp1$hhst.UAY[i]        <- temp$value[6]
  temp1$hhst.VET[i]        <- temp$value[7]
  
  rm(temp)
}


temp1 <- temp1 %>%
  .[order(.$max_pid_age2,
          .$min_pid_age2),]

temp1$rid <- 1:nrow(temp1)


ggplot() + 
  geom_boxplot(data = temp1, 
               aes(x = 1, y = hoh_age2))

ggplot() + 
  geom_segment(data = temp1, 
               aes(x = min_pid_age2, xend = max_pid_age2, 
                   y = rid, yend = rid, 
                   color = factor(n_pid2)))+
  geom_point(data = temp1, 
             aes(x = hoh_age2, y = rid, color = factor(n_pid2)))

summary.temp <- temp1 %>%
  group_by(hh.veteran, 
           hh.youth, 
           hh.regular, 
           hhst.PY, 
           hhst.kids_of_PY, 
           hhst.UAY, 
           hhst.VET) %>%
  summarise(n = n(), 
            hoh_age.min = min(hoh_age2), 
            hoh_age.max = max(hoh_age2),
            max_pid_age.min = min(max_pid_age2), 
            max_pid_age.max = max(max_pid_age2), 
            min_pid_age.min = min(min_pid_age2), 
            min_pid_age.max = max(min_pid_age2),
            n_pid.min       = min(n_pid2),
            n_pid.max       = max(n_pid2),
            n_VET = sum(hoh_is.veteran2))

summary.temp$rid <- 1:nrow(summary.temp)


ggplot() + 
  

# categorize HH type
summary.temp$calc_HH <- NA

summary.temp$calc_HH[summary.temp$hh.veteran == F & 
                       summary.temp$hh.youth == F & 
                       summary.temp$hh.regular == T] <- "Regular_HH"
summary.temp$calc_HH[summary.temp$hh.veteran == F & 
                       summary.temp$hh.youth == T & 
                       summary.temp$hh.regular == F] <- "Youth_HH"
summary.temp$calc_HH[summary.temp$hh.veteran == T & 
                       summary.temp$hh.youth == F & 
                       summary.temp$hh.regular == F] <- "Veteran_HH"
summary.temp$calc_HH[summary.temp$hh.veteran == T & 
                       summary.temp$hh.youth == T & 
                       summary.temp$hh.regular == F] <- "VeteranYouth_HH"

summary.temp$calc_HH[is.na(summary.temp$calc_HH)]   <- "<ERROR>"

summary.temp$error_HH <- (summary.temp$hh.regular == T & summary.temp$hh.veteran == T) | 
  (summary.temp$hh.regular == T & summary.temp$hh.youth == T)

summary.temp$error_HHST <- (summary.temp$hhst.PY != summary.temp$hhst.kids_of_PY)

summary.temp$error_VET <- (summary.temp$hh.veteran != summary.temp$hhst.VET) |
((summary.temp$n_VET > 0) != summary.temp$hh.veteran) 




# POSS.SCEN----
poss.scen <- expand.grid(hoh_age = 16:26,#c(17,20,26),
                         vet = c(T,F),
                         n_pid = c(1,2),
                         #min_pid_age = c(14,17,20,26)) %>%
                         min_pid_age = 16:26) %>%
  as_tibble() %>%
  mutate(.,
         remove = F)

# removes
# min_pid_age > hoh_age
poss.scen[poss.scen$min_pid_age > poss.scen$hoh_age,]$remove <- T

# hoh_age < 21 & vet == T
poss.scen[poss.scen$hoh_age < 21 &
            poss.scen$vet == T,]$remove <- T

# n_pid == 1 & hoh_age != min_pid_age
poss.scen[poss.scen$n_pid == 1 &
            poss.scen$min_pid_age !=
            poss.scen$hoh_age,]$remove <- T

poss.scen <- poss.scen[!poss.scen$remove,] %>%
  .[order(.$n_pid,.$vet),]


ggplot(data = poss.scen,
       aes(x = hoh_age, y = min_pid_age,
           color = vet)) +
  geom_jitter(width = 0.2, height = 0.2) +
  facet_grid(~n_pid, scales = "free", space = "free")+
  scale_x_continuous(minor_breaks = seq(0,1000,by=1))+
  scale_y_continuous(minor_breaks = seq(0,1000,by=1))

# setup
poss.scen$CAT_vet   <- NA
poss.scen$CAT_reg   <- NA
poss.scen$CAT_youth <- NA

poss.scen$TYPE_wo.child    <- NA
poss.scen$TYPE_adult.child <- NA
poss.scen$TYPE_only.child  <- NA

poss.scen$SUBTYPE_unac.yth          <- NA
poss.scen$SUBTYPE_prnt.yth          <- NA
poss.scen$SUBTYPE_child.of.prnt.yth <- NA
poss.scen$SUBTYPE_veteran           <- NA
poss.scen$SUBTYPE_everyone.else     <- NA

# logic
poss.scen$CAT_vet                   <- ifelse(poss.scen$vet, T, F)
poss.scen$CAT_youth                 <- ifelse(poss.scen$hoh_age < 25, T, F)
poss.scen$CAT_reg                   <- ifelse(poss.scen$CAT_vet == F &
                                                poss.scen$CAT_youth == F, T, F)

poss.scen$TYPE_wo.child             <- poss.scen$min_pid_age >= 18#25
poss.scen$TYPE_adult.child          <- poss.scen$n_pid > 1 &
                                        poss.scen$hoh_age >= 25 &
                                        poss.scen$min_pid_age < 25
poss.scen$TYPE_only.child           <- poss.scen$hoh_age < 25

poss.scen$SUBTYPE_unac.yth          <- (poss.scen$CAT_youth == T &
                                          poss.scen$TYPE_wo.child == T &
                                          data.table::between(x = poss.scen$hoh_age, 18, 24) &
                                          poss.scen$min_pid_age <= 18 &
                                          poss.scen$n_pid >= 1) |
                                        (poss.scen$CAT_youth == T &
                                           poss.scen$TYPE_only.child == T &
                                           poss.scen$hoh_age < 18 &
                                           poss.scen$n_pid >= 1) |
                                (poss.scen$CAT_youth == T &
                                   poss.scen$TYPE_adult.child == T &
                                   data.table::between(x = poss.scen$hoh_age, 18, 24) &
                                   poss.scen$min_pid_age < 18 &
                                   poss.scen$n_pid >= 2)

poss.scen[poss.scen$SUBTYPE_unac.yth,]


# person_AGE:
# `<18`    c(17)
# `18-24`  c(19)
# `18+`    c(19,26)
# `25+`    c(26)
#
# person_VET_STATUS:
# veteran
# not_veteran
#
# HH_TYPE:
# without_children
# adult_and_child
# only_children
#
# HH_SUBTYPE:
# unaccompanied_youth
# parenting_youth
# children_of_parenting_youth
# veteran
# everyone_else
#
# HH_CATEGORY:
# regular_hh
# youth_hh
# veteran_hh




# # re-run 2023 pit data every thursday morning until 3/31/2023
#
# thurs.hmis.pulls.complete <- ymd(c(20230302)) # update this after you pull and export new data each thursday
#
# if(as.character(lubridate::wday(Sys.Date(),label=T,abbr=F))=="Thursday" &
#    !Sys.Date() %in% thurs.hmis.pulls.complete){
#   # build hmis search:
#
#   # NAME: Unsheltered PIT Custom CSV 1/22/23 - 2/4/23 (For Tim!)
#   # TYPE: NCCEH_Custom_HUD_CSV_Payload
#   # PROVIDER TYPE: Reporting Group
#   # REPORTING GROUP: BoS FY 2022 unsheltered whole CoC reporting group (2507)
#   # START DATE: 1/22/2023
#   # END DATE: 2/04/2023
#
#   print("Data goes in C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data")
#   print("https://app.smartsheet.com/sheets/9gH67xJw5FXM2FvWr5j9MmJWqX53qp5qXPcQ7V51?view=grid")
#   stop("Upload new export Today") # stops the code unless you've uploaded weeklies
# }

# # NOTE----
# print("For hud pit survey for the night of Jan 26th, Entered on January 26th, Exited on Jaunary 27th")
#
# pit.night     <- ymd(20230125) #20230125 or 20220126
# pit.week_start <- pit.night %m+% days(1) #ymd(20220127) #20230126
# pit.week_end   <- pit.night %m+% days(7) #ymd(20220222) #20230221
#
# # /NOTE----
#
#
# # resources----
# out.template <- "https://ncceh.sharepoint.com/:x:/s/DataCenter/EdQERAgSu5pGsBcN5VNGD20B3qlfQ7iOCFz9BPJi2xoADQ?e=zOvaac"

# Setup----
#csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/test_data"
csv.file.dir <- "C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023/real_data"

# Functions----
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/main/pit_survey_calculations.R?raw=TRUE")

# hmis_join <- function(x.file,
#                       y.file,
#                       jtype,# = c("full", "left", "right", "inner"),
#                       dropcols = c("DateCreated","DateUpdated",
#                                    "DateDeleted", "UserID",
#                                    "ExportID", "CoCCode")){
#   require(glue)
#   # remove cols
#   x.file <- x.file[!colnames(x.file) %in% dropcols]
#   y.file <- y.file[!colnames(y.file) %in% dropcols]
#
#   # join
#   if(jtype == "full"){
#     out <- full_join(x.file, y.file)
#   }else if(jtype == "left"){
#     out <- left_join(x.file, y.file)
#   }else if(jtype == "right"){
#     out <- right_join(x.file,y.file)
#   }else if(jtype == "inner"){
#     out <- inner_join(x.file,y.file)
#   }else{
#     stop("invalid 'jtype' var")
#   }
#   return(out)
# }
#
# is_hashed <- function(x){
#   return(nchar(x) == 64 & class(x) == "character")
# }



# Working Directory Setup----
setwd(csv.file.dir)
getwd()


# # Vars / Data Setup----

youth.df <- data.frame(def_type = "youth",
                       def_id   = NA,
                       definition = c("This group includes all homeless persons age 24 or younger and without a head of household older than 24. This means that included youth could be in families, have brothers and sisters, parents and children, but they themselves must be 24 or younger and their parents or children must be 24 or younger. We are not including youth who are living with family members who are older than 24 years old. This group combines the old “unaccompanied youth” category with persons 18-24, and young families where parents are 24 or younger.",
                                      "Category 3

Homeless Under Other Statutes

Unaccompanied youth under 25 years of age,
or families with children and youth, who do
not meet any of the other categories but are
homeless under other federal statutes, have
not had a lease and have moved 2 or more
times in the past 60 days and are likely to
remain unstable because of special needs or
barriers. "),
                       source_url = c("https://www.hudexchange.info/homelessness-assistance/resources-for-homeless-youth/snaps-and-youth-an-overview/#:~:text=This%20group%20includes%20all%20homeless,must%20be%2024%20or%20younger.",
                                      "https://files.hudexchange.info/resources/documents/HUDs-Homeless-Definition-as-it-Relates-to-Children-and-Youth.pdf"),
                       program = c("SNAP", "Children and Youth and HUD’s Homeless Definition")) %>%
  as_tibble() %>%
  mutate(.,
         def_id = 1:length(def_id)) %>%
  mutate(.,
         def_id = paste(def_type, def_id, sep = "-"))



youth.df$definition[youth.df$def_id == "youth-2"] <- gsub("\\n{2,}", ". ",
                                                          youth.df$definition[youth.df$def_id == "youth-2"]) %>%
  gsub(",. ", ", ", .) %>%
  gsub("\n", " ", .)

youth.df

# # define youth type households----
#
# all.csv.files <- list.files(pattern = "\\.csv$")
# list.csv.colnames <- list()
#
# for(i in all.csv.files){
#     list.csv.colnames[[i]] <- colnames(read_csv(i))
# };rm(i)
#
# if(!length(all.csv.files) == length(list.csv.colnames)){
#   stop("colnames list doesn't match length of filenames list")
# };rm(all.csv.files)
#
# list.csv.colnames

# define household types and household subtypes----

hh_youth_types <- expand.grid(hh_type = c("Households without children",
                        "Households with at least 1 adult and 1 child",
                        "Households with only children"),
            hh_subtype = c("Unaccompanied youth (18-24)",
                           "Parenting youth (18-24)",
                           "Children of parenting youth",
                           "Unaccompanied youth (<18 & 18-24 in one hh)",
                           "Parenting youth (<18)",
                           "Unaccompanied youth (<18)"),
            stringsAsFactors = F) %>%
  as_tibble() %>%
  mutate(.,
         hh_subtype_present = F)

hh_youth_types[hh_youth_types$hh_type ==
                 "Households without children" &
                 hh_youth_types$hh_subtype %in%
                 c("Unaccompanied youth (18-24)"),]$hh_subtype_present <- T
hh_youth_types[hh_youth_types$hh_type ==
                 "Households with at least 1 adult and 1 child" &
                 hh_youth_types$hh_subtype %in%
                 c("Parenting youth (18-24)",
                   "Children of parenting youth",
                   "Unaccompanied youth (<18 & 18-24 in one hh)"),]$hh_subtype_present <- T
hh_youth_types[hh_youth_types$hh_type ==
                 "Households with only children" &
                 hh_youth_types$hh_subtype %in%
                 c("Parenting youth (<18)",
                   "Children of parenting youth",
                   "Unaccompanied youth (<18)"),]$hh_subtype_present <- T

hh_youth_types %>%
  as.data.table() %>%
  dcast(hh_subtype ~ hh_type) %>%
  as.data.frame() %>%
  as_tibble()


# Households without children:  must have 1 HoH aged 18-24 unaccompanied youth
hh_youth_types2 <- expand.grid(category                   = c("youth_type",
                                                              "vet_type",
                                                              "regular_type"),
                               hh_type                    = c("hh without children",
                                                              "hh with at least 1 adult and 1 child",
                                                              "hh with only children"),
                               hh_subtype                 = c("unaccompanied youth",
                                                              "parenting youth",
                                                              "children of parenting youth",
                                                              "everyone else",
                                                              "veteran"),
                               hh_age_group               = c("<18",
                                                              "18-24",
                                                              "25+",
                                                              "18+"),
                               whole_house                = NA,
                               possible_subtypeXage_group = NA,
                               permitted                  = NA,
                               required                   = NA,
                               stringsAsFactors = F) %>% as_tibble()

hh_youth_types2$hh_subtype_f <- factor(hh_youth_types2$hh_subtype,
                                    levels = c("unaccompanied youth", "parenting youth",
                                               "children of parenting youth",
                                               "veteran",
                                               "everyone else"))
# plot

library(ggplot2)
ggplot() +
  geom_point(data = hh_youth_types2, size = 4,
             aes(y = hh_subtype_f, x = hh_age_group, color = permitted))+
  facet_grid(category~hh_type)+
  theme(axis.text.x = element_text(angle = 0, hjust = 1, vjust = 1),
        strip.text.y = element_text(angle = 0))

# filters----
# everyone_else in regular_type households:
# and with 1 adult and 1 child
hh_youth_types2[hh_youth_types2$hh_subtype_f == "everyone else" &
                  hh_youth_types2$category == "regular_type" &
                  hh_youth_types2$hh_type == "hh with at least 1 adult and 1 child" &
                  hh_youth_types2$hh_age_group %in% c("<18", "18+", "18-24", "25+"),]$permitted <- T
hh_youth_types2[hh_youth_types2$hh_subtype_f == "everyone else" &
                  hh_youth_types2$category == "regular_type" &
                  hh_youth_types2$hh_type == "hh with at least 1 adult and 1 child" &
                  hh_youth_types2$hh_age_group %in% c("<18", "18+", "18-24", "25+"),]$permitted <- T

# hh w/o chidlren age < 18 = not permitted

# hh w only children & age > 18 = not permitted

hh_youth_types2[hh_youth_types2$hh_subtype %in% c("unaccompanied youth", "parenting youth", "children of parenting youth") &
                  hh_youth_types2$hh_age_group == "25+",]$permitted <- F


hh_youth_types2[hh_youth_types2$hh_subtype %in% c("unaccompanied youth", "parenting youth", "children of parenting youth") &
                  hh_youth_types2$category %in%
                  c("regular_type", "vet_type"),]$permitted <- F

hh_youth_types2[hh_youth_types2$category == "youth_type" &
                  hh_youth_types2$hh_subtype == "everyone else",]$permitted <- F

hh_youth_types2[hh_youth_types2$category == "youth_type" &
                  hh_youth_types2$hh_age_group == "25+",]$permitted <- F

hh_youth_types2[hh_youth_types2$category == "youth_type" &
                  hh_youth_types2$hh_type == "hh without children" &
                  hh_youth_types2$hh_age_group == "<18" &
                  is.na(hh_youth_types2$permitted),]$permitted <- F

hh_youth_types2[is.na(hh_youth_types2$permitted) &
                   hh_youth_types2$category == "youth_type" &
                  hh_youth_types2$hh_type == "hh without children" &
                  hh_youth_types2$hh_subtype == "unaccompanied youth",]$permitted <- T

hh_youth_types2[is.na(hh_youth_types2$permitted),]


list(youth_hh_without_children = c("must have 1 HoH aged 18-24 and unaccompanied youth",
                                   "can have no "))



# Households with at least 1 adult and 1 child






# next steps----

# picking the variables to calculate the logic for these categories ^^^


