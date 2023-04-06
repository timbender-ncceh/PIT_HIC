library(shiny)
library(dplyr)
library(data.table)
library(glue)

# load functions----
devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev/QA/shiny_QA_youthvethh/qa_logic_youthvethh.R?raw=TRUE")


navbarPage(title = "Household Size:", 
           header = shiny::titlePanel(title = "QA Check Logic for Youth_Veteran_Households"), 
           tabPanel(title = "1-Person",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = "print_fun_hh00", 
                                    label = "Show Function Logic", 
                                    choices = list("hh_youth()" = hh_youth, 
                                                   "hh_vet()"         = hh_vet, 
                                                   "hh_age.unknown()" = hh_age.unknown, 
                                                   "hh_w.o.C() (with only children)" = hh_w.o.C, 
                                                   "hh_wo.c() (without children)" = hh_wo.c, 
                                                   "hh_wal1a1c() (with at least 1 adult & 1 child)" = hh_wal1a1c, 
                                                   "py_18.24()" = py_18.24, 
                                                   "py_u18()" = py_u18, 
                                                   "uy() (unaccompanied youth)" = uy))
                      ),
                      mainPanel = verbatimTextOutput("hhs_1_values.fun")
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        shiny::sliderInput(inputId = "age_input00", 
                                           label = "Age", 
                                           min = 1, max = 80, step = 1, 
                                           value = 1),
                        selectizeInput(inputId = 'rel_input00', 
                                       label = 'Relationship To HoH', 
                                       choices = c(NA, 
                                                   "Self (head of household)", 
                                                   "Head of household's child", 
                                                   "Head of household's spouse or partner", 
                                                   "Head of household's other relation member", 
                                                   "Other: non-relation member"), 
                                       multiple = F),
                        selectizeInput(inputId = 'vet_input00', 
                                       label = 'Veteran Status', 
                                       choices = c("No", "Yes", NA), 
                                       multiple = F)),
                      mainPanel(
                        verbatimTextOutput('hhs_1_values1')
                      )
                    )
                    
           ), 
           tabPanel(title = "3-Person",
                    sidebarLayout(
                      sidebarPanel(
                        selectInput(inputId = "print_fun_hh01", 
                                    label = "Show Function Logic", 
                                    choices = list("hh_youth()" = hh_youth, 
                                                   "hh_vet()"         = hh_vet, 
                                                   "hh_age.unknown()" = hh_age.unknown, 
                                                   "hh_w.o.C() (with only children)" = hh_w.o.C, 
                                                   "hh_wo.c() (without children)" = hh_wo.c, 
                                                   "hh_wal1a1c() (with at least 1 adult & 1 child)" = hh_wal1a1c, 
                                                   "py_18.24()" = py_18.24, 
                                                   "py_u18()" = py_u18, 
                                                   "uy() (unaccompanied youth)" = uy))
                      ),
                      mainPanel = verbatimTextOutput('hhs_3_values.fun')
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        shiny::sliderInput(inputId = "age_input01", 
                                           label = "Age", 
                                           min = 1, max = 80, step = 1, 
                                           value = 1),
                        selectizeInput(inputId = 'rel_input01', 
                                       label = 'Relationship To HoH', 
                                       choices = c(NA, 
                                                   "Self (head of household)", 
                                                   "Head of household's child", 
                                                   "Head of household's spouse or partner", 
                                                   "Head of household's other relation member", 
                                                   "Other: non-relation member"), 
                                       multiple = F),
                        selectizeInput(inputId = 'vet_input01', 
                                       label = 'Veteran Status', 
                                       choices = c("No", "Yes", NA), 
                                       multiple = F)),
                      mainPanel(
                        verbatimTextOutput('hhs_3_values1')
                      )
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        shiny::sliderInput(inputId = "age_input02", 
                                           label = "Age", 
                                           min = 1, max = 80, step = 1, 
                                           value = 1),
                        selectizeInput(inputId = 'rel_input02', 
                                       label = 'Relationship To HoH', 
                                       choices = c(NA, 
                                                   "Self (head of household)", 
                                                   "Head of household's child", 
                                                   "Head of household's spouse or partner", 
                                                   "Head of household's other relation member", 
                                                   "Other: non-relation member"), 
                                       multiple = F),
                        selectizeInput(inputId = 'vet_input02', 
                                       label = 'Veteran Status', 
                                       choices = c("No", "Yes", NA), 
                                       multiple = F)),
                      mainPanel(label = "Output Results:",
                                verbatimTextOutput('hhs_3_values2')
                      )
                    ),
                    sidebarLayout(
                      sidebarPanel(
                        shiny::sliderInput(inputId = "age_input03", 
                                           label = "Age", 
                                           min = 1, max = 80, step = 1, 
                                           value = 1),
                        selectizeInput(inputId = 'rel_input03', 
                                       label = 'Relationship To HoH', 
                                       choices = c(NA, 
                                                   "Self (head of household)", 
                                                   "Head of household's child", 
                                                   "Head of household's spouse or partner", 
                                                   "Head of household's other relation member", 
                                                   "Other: non-relation member"), 
                                       multiple = F),
                        selectizeInput(inputId = 'vet_input03', 
                                       label = 'Veteran Status', 
                                       choices = c("No", "Yes", NA), 
                                       multiple = F)),
                      mainPanel(
                        verbatimTextOutput('hhs_3_values3')
                      )
                    )
                    
           )
)
