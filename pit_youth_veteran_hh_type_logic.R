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
             aes(y = hh_subtype_f, x = hh_age_group))+
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


