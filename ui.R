
# This is the user-interface definition of a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

library(shiny)
library(shinydashboard)
library(raster)
library(rgdal)

header <- dashboardHeader(title = "Spatial RIN")

sidebar <- dashboardSidebar(
  #uiOutput("bg_removal"),
  uiOutput("background"),
  uiOutput("plot_name"),
  downloadButton("dl_plot", label = "Download the plot")
)

body <- dashboardBody(
  fluidPage(
    
    tabBox(
      title = "Spatial RIN",
      id = "srin",
      width = 12,
      tabPanel("Data Input", width = "auto",
               fluidPage(
                 column(width = 6, 
                        fileInput("sRIN_probe1", "Upload the Probe1 file here", multiple = FALSE,
                                             accept = "tiff", buttonLabel = "Browse"),
                        fileInput("sRIN_probe2", "Upload the Probe2 file here", multiple = FALSE, 
                                             accept = "tiff", buttonLabel = "Browse"),
                        fileInput("sRIN_probe3", "Upload the Probe3 file here", multiple = FALSE,
                                  accept = "tiff", buttonLabel = "Browse"),
                        fileInput("sRIN_probe4", "Upload the Probe4 file here", multiple = FALSE,
                                  accept = "tiff", buttonLabel = "Browse")
                        ),
                 column(width = 6, 
                        fileInput("no_probes", "Choose background file to upload", multiple = FALSE,
                                             accept = "tiff", buttonLabel = "Browse"))
               )
      ),
      tabPanel("sRIN Heatmap", width = "auto", 
               fluidPage(
                 plotOutput("plot_whole_array", height = 1000)
               )
      )
    )
  )
)

dashboardPage(header,sidebar,body)
