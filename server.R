
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

shinyServer(function(input, output) {

  output$probe_correction_factors <- renderUI({
    num_probes <- as.numeric(length(input$sRIN_data$name))
    if (num_probes == 0) {
      return(NULL)
    }
    lapply(1:num_probes, function(x) {
      textInput(inputId = unlist(strsplit(input$sRIN_data$name[x], "\\.")[1]), 
                label = paste("Correction factor for probe ", unlist(strsplit(input$sRIN_data$name[x], "\\."))[1]), 
                value = 1)    
    }
    )
  })
  
  st_rin_data <- reactive({
    
    dat <- NULL
    num_datasets <- as.numeric(length(input$sRIN_data$name))
    if (num_datasets == 0) {
      return(NULL)
    }
    lapply(1:num_datasets, function(x){
      subsets <- c("X", "Y", "F532.Mean")
      temp_data <- read.table(input$sRIN_data$datapath[x], header = TRUE, skip = input$row_skip)
      sub_temp_data <- subset(temp_data, select = subsets)
      name <- paste("input$",unlist(strsplit(input$sRIN_data$name[x], "\\.")[1]), sep="")
      #test <- "sub_temp_data$F532.Mean*name"
      #sub_temp_data$F532.Mean <- eval(parse(text = test))
      sub_temp_data$F532.Mean <- sub_temp_data$F532.Mean*input$eval(input$sRIN_data$name[x])
       # paste("input$",unlist(strsplit(input$sRIN_data$name[x], "\\.")[1]), sep="")
      #lapply(1:length(temp_data[,1]), function(y){
      #dat <- cbind(dat, sub_temp_data)
      if (is.null(dat)){
        dat <<- as.data.frame(sub_temp_data)
      } else {
        dat <<- cbind(dat, sub_temp_data)
      }
      #})
    })
    dat
  })
  
  output$st_data <- DT::renderDataTable({
    st_rin_data()
  })
  
  output$data_files <- renderTable({
    input$sRIN_data$name
  })
  
  output$show_data_example <- DT::renderDataTable({
    infile <- input$sRIN_data
    if (is.null(infile)){
      return(NULL)
    }
    head(read.table(infile$datapath[1], header = TRUE, skip = input$row_skip))
  })
})
