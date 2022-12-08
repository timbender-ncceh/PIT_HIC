library(dplyr)
library(igraph)

setwd("C:/Users/TimBender/Documents/R/ncceh/projects/pit_survey/January_2023")
rm(list=ls())
cat('\f')
gc()


# # Functions---- 
# # (chunks of code that you've written that will need to be
# # deployed more than 1 or 2 times in a single script.  allows you to easily
# # manage and modify logic one-time only if problems are found)
# 
# # Variables---- 
# # (anything that I might want to adjust on the fly in the future
# # for any reason and don't want to have to hunt down in the code at a later
# # date)
# 
# 
# # Import Data----
# # (in this case I'm building my own) 
# a.graph <- graph_from_literal(enrollmentcoc --+ enrollment --+ client --+ project --+ projectcoc --+
#                                 inventory --+ curlivingsit --+ disabilities --+ healthanddv)
# 
# plot(a.graph)
# 
# 
# 
# # Load package
# library(networkD3)
# 
# # Load energy projection data
# URL <- "https://cdn.rawgit.com/christophergandrud/networkD3/master/JSONdata/energy.json"
# Energy <- jsonlite::fromJSON(URL)
# 
# 
# # Now we have 2 data frames: a 'links' data frame with 3 columns (from, to, value), and a 'nodes' data frame that gives the name of each node.
# head( Energy$links )
# head( Energy$nodes )
# 
# 
# 
# # Thus we can plot it
# p <- sankeyNetwork(Links = Energy$links, Nodes = Energy$nodes, Source = "source",
#                    Target = "target", Value = "value", NodeID = "name",
#                    units = "TWh", fontSize = 12, nodeWidth = 30)
# p
# 
# URL <- paste0('https://cdn.rawgit.com/christophergandrud/networkD3/',
#               'master/JSONdata/energy.json')
# energy <- jsonlite::fromJSON(URL)
# 
# # Plot
# sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
#               Target = 'target', Value = 'value', NodeID = 'name',
#               units = 'TWh', fontSize = 12, nodeWidth = 30)
# 
# # Colour links
# energy$links$energy_type <- sub(' .*', '',
#                                 energy$nodes[energy$links$source + 1, 'name'])
# 
# sankeyNetwork(Links = energy$links, Nodes = energy$nodes, Source = 'source',
#               Target = 'target', Value = 'value', NodeID = 'name',
#               LinkGroup = 'energy_type', NodeGroup = NULL)



# pit stuff

# nodes----
pit.nodes <- data.frame(name = c("enrollment.csv", "enrollmentcoc.csv", 
                                 "client.csv", "project.csv", 
                                 "projectcoc.csv", "inventory.csv", 
                                 "CurrentLivingSituation.csv", "disabilities.csv", 
                                 "healthanddv.csv", 
                                 "EnrollmentID", "HouseholdID", "PersonalID", 
                                 "ProjectID", "masterOutput.csv", 
                                 "DisabilityType", 
                                 "RelationshipToHoH", 
                                 "VeteranStatus", "CurrentLivingSituation", 
                                 "CurrentlyFleeing", "DOB", 
                                 "Age (calculated)", 
                                 "NoSingleGender", "Transgender", "GenderNone", 
                                 "Female", "Male", "Ethnicity", "RaceNone", "AmIndAKNative", "Asian", 
                                 "BlackAfAmerican", "NativeHIPacific", "White", "Questioning", 
                                 "Gender (grouped)", "Race Ethnicity (grouped)",
                                 "DisablingCondition", "NCCounty",
                                 "LiteralHomelessHistory", "LivingSituation",
                                 "MentalHealthDisorderFam", "AlcoholDrugUseDisorderFam", 
                                 "EntryDate", 
                                 "Exit.csv", "ExitDate", 
                                 "enrolledOnSurveyDate (filter)", 
                                 "screened_positive_disability (filter)", 
                                 "DisabilityResponse", "IndefiniteAndImpairs", 
                                 "ProjectName", "CoCCode", "HouseholdType", 
                                 "MentalHealthStatus", 
                                 "InformationDate (cls)", 
                                 "is_HoH (calculated)"))
pit.nodes

pit.links <- rbind(data.frame(source = c(8,8,48,49), 
                              target = c(48,49,47,47), 
                              value = 1), 
                   data.frame(source = c(1), 
                              target = c(43), 
                              value = 1), 
                   data.frame(source = c(44,44,44,45,46,43), 
                              target = c(10,12,45,46,14,46), 
                              value = 1), 
                   data.frame(source = c(rep(1, length(37:42)), 
                                         37:42), 
                              target = c(37:42, rep(14, length(37:42))), 
                              value = 1), 
                   data.frame(source = c(rep(1,4), rep(2, 4)), 
                              target = c(12,13,10,11,12,13,10,11), 
                              value = c(1)),
                   data.frame(source = c(3,4,5,6), 
                              target = c(12,13,13,13), 
                              value = c(1)), 
                   data.frame(source = c(7,7,8,8,9,9), 
                              target = c(10,12,10,12,10,12), 
                              value = c(1)), 
                   data.frame(source = c(10,11,12,13), 
                              target = c(rep(14,4)), 
                              value = c(5,2,6,5) / 1), 
                   data.frame(source = c(3,17,7,18), 
                              target = c(17,14,18,14), 
                              value = c(1)*1), 
                   
                   data.frame(source = c(1,16,55), 
                              target = c(16,55,14), 
                              value = 1),
                   
                   data.frame(source = c(9,19), 
                              target = c(19,14), 
                              value = 1), 
                   data.frame(source = c(8,15,47), 
                              target = c(15,47,14), 
                              value = 1),
                   data.frame(source = c(3,20, 21), 
                              target = c(20, 21, 14), 
                              value = c(0.3, 0.3, 1)),
                   data.frame(source = c(rep(3,length(c(34,22:26))), c(34,22:26)), 
                              target = c(c(34,22:26), rep(35, length(c(34,22:26)))), 
                              value = 0.3), 
                   data.frame(source = c(35,36), 
                              target = c(14), 
                              value = 0.95), 
                   data.frame(source = c(4,50,5,51,6,52,9,53), 
                              target = c(50,14,51,14,52,14,53,14),
                              value = 1), 
                   data.frame(source = c(7,54), 
                              target = c(54,14), 
                              value = 1),
                   data.frame(source = c(rep(3,length(27:33)), 27:33), 
                             target = c(27:33, rep(36, length(27:33))), 
                             value = 0.3)) %>%
  mutate(., 
         value = ifelse(value > 1, 1, value),
         value = ifelse(value == 1, 0.9, value),
         value = ifelse(target %in% 
                          (which(pit.nodes$name %in% c("RelationshipToHoH", 
                                      "DisabilityType", 
                                      "CurrentLivingSituation", 
                                      "CurrentlyFleeing", 
                                      "VeteranStatus",
                                      "DisablingCondition", "NCCounty",
                                      "LiteralHomelessHistory", "LivingSituation",
                                      "MentalHealthDisorderFam", 
                                      "AlcoholDrugUseDisorderFam", 
                                      "EntryDate"))-1), 
                        0.3, value),
         value = ifelse(target %in% 
                          (which(pit.nodes$name %in% 
                                   c("masterOutput.csv"))-1), 
                        0.5, value),
         source = source - 1, 
         target = target - 1)



pit.links$linkGrp <- "No Group"
pit.links$linkGrp[pit.links$target %in% 
                    (which(pit.nodes$name %in% 
                             c("Gender (grouped)", 
                               "Race Ethnicity (grouped)"))-1)] <- "grouped_vars"
pit.links$linkGrp[pit.links$source %in% 
                    (which(pit.nodes$name %in% 
                             c("Gender (grouped)", 
                               "Race Ethnicity (grouped)"))-1)] <- "grouped_vars"


pit.links$linkGrp[pit.links$target %in% 
                    (which(pit.nodes$name %in% 
                             c("NoSingleGender", "Transgender", 
                               "GenderNone", 
                               "Female", "Male", "Ethnicity", 
                               "RaceNone", "AmIndAKNative", 
                               "Asian", 
                               "BlackAfAmerican", "NativeHIPacific", 
                               "White", "Questioning"))-1)] <- "grouped_vars"

pit.links$linkGrp[pit.links$target %in% 
                    (which(pit.nodes$name %in% 
                             c("Age (calculated)", "is_HoH (calculated)",
                               "RelationshipToHoH",
                               "DOB"))-1)] <- "calculated_vars"
pit.links$linkGrp[pit.links$source %in% 
                    (which(pit.nodes$name %in% 
                             c("Age (calculated)", 
                               "is_HoH (calculated)"))-1)] <- "calculated_vars"


pit.links$linkGrp[pit.links$source %in% 
                    (which(pit.nodes$name %in% 
                             c("EntryDate", "ExitDate", 
                               "enrolledOnSurveyDate (filter)",
                               "DisabilityType", "DisabilityResponse", "IndefiniteAndImpairs",
                               "screened_positive_disability (filter)"))-1) | 
                    pit.links$target %in% 
                    (which(pit.nodes$name %in% 
                             c("enrolledOnSurveyDate (filter)",
                               "DisabilityType","DisabilityResponse", "IndefiniteAndImpairs",
                               "screened_positive_disability (filter)",
                               "EntryDate", "ExitDate"))-1)] <- "filtered_vars"


pit.links$value <- 1


pit.network <- sankeyNetwork(Links = pit.links, 
                             Nodes = pit.nodes, 
                             Source = "source", 
                             Target = "target", 
                             Value = "value", 
                             NodeID = "name", 
                             units = "x", 
                             fontSize = 10, 
                             nodeWidth = 30, 
                             nodePadding = 2, 
                             LinkGroup = "linkGrp", 
                             NodeGroup = NULL);pit.network

#save the widget
library(htmlwidgets)
saveWidget(pit.network, file=paste0( getwd(), "/pit_logic_HtmlWidget/pit_network_sankey.html"))


# Cleanup Data----


# Process & Analyze Data----


# Q/A Data----

# 
