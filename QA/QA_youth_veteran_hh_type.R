library(shiny)
library(dplyr)
library(data.table)
library(glue)

rm(list=ls())

ui <- fluidPage(h3("Person #1:"),
                sidebarLayout(
                  sidebarPanel(
                    shiny::sliderInput(inputId = "age_input01", 
                                       label = "Age", 
                                       min = 0, max = 80, step = 1, 
                                       value = 0),
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
                                   choices = c("Yes", "No", NA), 
                                   multiple = F)),
                  mainPanel(
                    verbatimTextOutput('values1')
                  )
                ), 
                h3("Person #2:"),
                sidebarLayout(
                  sidebarPanel(
                    shiny::sliderInput(inputId = "age_input02", 
                                       label = "Age", 
                                       min = 0, max = 80, step = 1, 
                                       value = 0),
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
                                   choices = c("Yes", "No", NA), 
                                   multiple = F)),
                  mainPanel(label = "Output Results:",
                            verbatimTextOutput('values2')
                  )
                ),
                h3("Person #3:"),
                sidebarLayout(
                  sidebarPanel(
                    shiny::sliderInput(inputId = "age_input03", 
                                       label = "Age", 
                                       min = 0, max = 80, step = 1, 
                                       value = 0),
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
                                   choices = c("Yes", "No", NA), 
                                   multiple = F)),
                  mainPanel(
                    verbatimTextOutput('values3')
                  )
                )#, 
                #title = 'Options groups for select(ize) input'
)

server <- function(input,output,session){
  updateSelectizeInput(session, 'x2', choices = list(
    Eastern = c(`Rhode Island` = 'RI', `New Jersey` = 'NJ'),
    Western = c(`Oregon` = 'OR', `Washington` = 'WA'),
    Middle = list(Iowa = 'IA')
  ), selected = 'IA')
  
  # Funs here----
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
      }else
        out <- F
    }
    return(out)
  }
  
  # Outputs here----
  output$values1 <- renderPrint({
    cat("ARGUMENT INPUTS:\n\n")
    cat(glue("Ages:\t\t{paste(c(input$age_input01,input$age_input02,input$age_input03),sep=\", \",collapse=\", \")}\n\n"))
    cat(glue("Veteran:\t{paste(c(input$vet_input01,input$vet_input02,input$vet_input03),sep=\", \",collapse=\", \")}\n\n"))
    cat(glue("Rel2HoH:\t{paste(c(input$rel_input01,input$rel_input02,input$rel_input03),sep=\", \",collapse=\", \")}\n\n"))
  })
  
  output$values2 <- renderPrint({
    cat(bold(inverse("LOGIC OUTPUTS:\n\n")))
    cat(glue("hh_vet():\t[1] {hh_vet(c(input$vet_input01,input$vet_input02,input$vet_input03))}\n\n"))
    cat(glue("hh_youth():\t[1] {hh_vet(c(input$age_input01,input$age_input02,input$age_input03))}\n\n"))
    
  })
}


# app----
shinyApp(ui = ui, server = server)
