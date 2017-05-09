
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(ggplot2)
library(DT)

header <- dashboardHeader(title = "Spatial RIN")

sidebar <- dashboardSidebar(
  uiOutput("probe_correction_factors")
)

body <- dashboardBody(
  fluidPage(
    
    tabBox(
      title = "Spatial RIN",
      id = "srin",
      width = 12,
      tabPanel("Whole Array", div(style = "overflow-x: scroll", DT::dataTableOutput("st_data"))),
      tabPanel("Under Tissue", "content"),
      tabPanel("Data Input", width = "auto",
               fluidPage(
                 column(width = 6, fileInput("sRIN_data", "Choose the sRIN data files to upload", multiple = TRUE, 
                                       accept = c("text/csv", "text/comma-separated-values", "text/plain", "csv"), buttonLabel = "Browse")),
                 column(width = 6, tableOutput("data_files"))
               ),
               fluidPage(
                 column(width = 6, textInput("row_skip", "Choose how many rows to skip when importing data", value = 26)),
                 column(width = 6, div(style = 'overflow-x: scroll', DT::dataTableOutput("show_data_example")))
               )
      )
    )
  )
)

dashboardPage(header,sidebar,body)
