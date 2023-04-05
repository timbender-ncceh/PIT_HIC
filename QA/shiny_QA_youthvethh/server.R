function(input,output,session){
  # load functions----
  devtools::source_url(url = "https://raw.githubusercontent.com/timbender-ncceh/PIT_HIC/dev/QA/shiny_QA_youthvethh/qa_logic_youthvethh.R?raw=TRUE")
  # Outputs here----
  
  # 1-Person Household PRINT
  # inputs
  # outputs
  output$hhs_1_values.fun <- renderPrint({
    cat(glue("#### SELECTED FUNCTION LOGIC: ####  (for display only; no ineractive functionality)\n\n{input$print_fun_hh00}\n\n"))
  })
  output$hhs_3_values.fun <- renderPrint({
    cat(glue("#### SELECTED FUNCTION LOGIC: ####  (for display only; no ineractive functionality)\n\n{input$print_fun_hh01}\n\n"))
  })
  
  output$hhs_1_values1 <- renderPrint({
    cat("#### ARGUMENT INPUTS: ####\n")
    cat(glue("Ages:\t\t{paste(c(input$age_input00),sep=\", \",collapse=\", \")}\n\n"))
    cat(glue("Veteran:\t{paste(c(input$vet_input00),sep=\", \",collapse=\", \")}\n\n"))
    cat(glue("Rel2HoH:\t{paste(c(input$rel_input00),sep=\", \",collapse=\", \")}\n\n"))
    cat("\n#### LOGIC OUTPUTS: ####\n")
    #hh_vet
    cat(glue("hh_vet():\t\t[1] {hh_vet(c(input$vet_input00))}\n\n"))
    #hh_youth
    cat(glue("hh_youth():\t\t[1] {hh_youth(c(input$age_input00))}\n\n"))
    #hh_age.unknown
    cat(glue("hh_age.unknown():\t[1] {hh_age.unknown(c(input$age_input00))}\n\n"))
    #hh_wal1a1c
    cat(glue("hh_wal1a1c():\t\t[1] {hh_wal1a1c(c(input$age_input00))}\n\n"))
    #hh_wo.c
    cat(glue("hh_wo.c():\t\t[1] {hh_wo.c(c(input$age_input00))}\n\n"))
    #hh_w.o.C
    cat(glue("hh_w.o.C():\t\t[1] {hh_w.o.C(c(input$age_input00))}\n\n"))
    #py_u18
    cat(glue("py_u18():\t\t[1] {py_u18(ages=c(input$age_input00),rel2hohs=c(input$rel_input00))}\n\n"))
    #py_18.24
    cat(glue("py_18.24():\t\t[1] {py_18.24(ages=c(input$age_input00),rel2hohs=c(input$rel_input00))}\n\n"))
    #uy
    cat(glue("uy():\t\t\t[1] {uy(ages1=c(input$age_input00),rel2hohs1=c(input$rel_input00))}\n\n"))
    #cat(glue("\n\n#### SELECTED FUNCTION LOGIC: ####\n{input$print_fun_hh00}\n\n"))
  })
  #output$hhs_1_values2 <- renderPrint()
  
  # 3-Person Household PRINT
  # inputs
  # outputs
  output$hhs_3_values1 <- renderPrint({
    cat("#### ARGUMENT INPUTS: ####\n")
    cat(glue("Ages:\t\t{paste(c(input$age_input01,input$age_input02,input$age_input03),sep=\", \",collapse=\", \")}\n\n"))
    cat(glue("Veteran:\t{paste(c(input$vet_input01,input$vet_input02,input$vet_input03),sep=\", \",collapse=\", \")}\n\n"))
    cat(glue("Rel2HoH:\t{paste(c(input$rel_input01,input$rel_input02,input$rel_input03),sep=\", \",collapse=\", \")}\n\n"))
    cat("\n#### LOGIC OUTPUTS: ####\n")
    #hh_vet
    cat(glue("hh_vet():\t\t[1] {hh_vet(c(input$vet_input01,input$vet_input02,input$vet_input03))}\n\n"))
    #hh_youth
    cat(glue("hh_youth():\t\t[1] {hh_youth(c(input$age_input01,input$age_input02,input$age_input03))}\n\n"))
    #hh_age.unknown
    cat(glue("hh_age.unknown():\t[1] {hh_age.unknown(c(input$age_input01,input$age_input02,input$age_input03))}\n\n"))
    #hh_wal1a1c
    cat(glue("hh_wal1a1c():\t\t[1] {hh_wal1a1c(c(input$age_input01,input$age_input02,input$age_input03))}\n\n"))
    #hh_wo.c
    cat(glue("hh_wo.c():\t\t[1] {hh_wo.c(c(input$age_input01,input$age_input02,input$age_input03))}\n\n"))
    #hh_w.o.C
    cat(glue("hh_w.o.C():\t\t[1] {hh_w.o.C(c(input$age_input01,input$age_input02,input$age_input03))}\n\n"))
    #py_u18
    cat(glue("py_u18():\t\t[1] {py_u18(ages=c(input$age_input01,input$age_input02,input$age_input03),rel2hohs=c(input$rel_input01,input$rel_input02,input$rel_input03))}\n\n"))
    #py_18.24
    cat(glue("py_18.24():\t\t[1] {py_18.24(ages=c(input$age_input01,input$age_input02,input$age_input03),rel2hohs=c(input$rel_input01,input$rel_input02,input$rel_input03))}\n\n"))
    #uy
    cat(glue("uy():\t\t\t[1] {uy(ages1=c(input$age_input01,input$age_input02,input$age_input03),rel2hohs1=c(input$rel_input01,input$rel_input02,input$rel_input03))}\n\n"))
    #cat(glue("\n\n#### SELECTED FUNCTION LOGIC: ####\n{input$print_fun_hh01}\n\n"))
  })
  #output$hhs_3_values2 <- renderPrint()
  
  
}
