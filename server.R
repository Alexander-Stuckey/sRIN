
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
  
  #Create the input boxes for the probe correction factors once some data has been uploaded. Makes one box for each dataset.
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
  
  #Set the background level of flourescence.
  output$bg_level <- renderUI({
    if(is.null(st_rin_data)){
      return(NULL)
    }
    numericInput(inputId = "bg_level", label = "Set the background flourescence level", value = 1205)
  })
  bg_level_cutoff <- reactive({
    2*input$bg_level
  })
  
  sRIN_scale <- reactive({
    10/length(input$sRIN_data$name)
  })
  
  st_rin_data <- reactive({
    num_datasets <- as.numeric(length(input$sRIN_data$name))
    if (num_datasets == 0) {
      return(NULL)
    }
    x_pos <- read.table(input$sRIN_data$datapath[1], header = TRUE, skip = input$row_skip)
    y_pos <- read.table(input$sRIN_data$datapath[1], header = TRUE, skip = input$row_skip)
    dat <- data.frame(x_pos$X, y_pos$Y, numeric(length(x_pos$X)))
    colnames(dat) <- c("X", "Y", "sRIN")
    dat$X <- dat$X - min(dat$X)
    dat$Y <- dat$Y - min(dat$Y)

    dat$nX <- switch(input$adjust_xy,
      No = round(dat$X/30)*30,
      Yes = round(dat$X/(input$he_xdim/150))*(input$he_xdim/150)
    )
    dat$nY <- switch(input$adjust_xy,
      No = round(dat$Y/30)*30,
      Yes = round(dat$Y/(input$he_ydim/150))*(input$he_ydim/150)
    )
    
    vapply(1:num_datasets, FUN.VALUE = double(length = 22500), FUN = function(x){
      subsets <- c("X", "Y", "F532.Mean")
      temp_data <- read.table(input$sRIN_data$datapath[x], header = TRUE, skip = input$row_skip)
      sub_temp_data <- subset(temp_data, select = subsets)
      name <- paste("input$",unlist(strsplit(input$sRIN_data$name[x], "\\."))[1], sep="")
      name2 <- as.numeric(eval(parse(text = name)))
      adjusted_fluro <- sub_temp_data$F532.Mean*name2
      vapply(1:length(adjusted_fluro), FUN.VALUE = double(length = 1), function(y){
        if (adjusted_fluro[y] > bg_level_cutoff()) {
          dat$sRIN[y] <<- dat$sRIN[y] + sRIN_scale()
        } else if (adjusted_fluro[y] < input$bg_level) {
          dat$sRIN[y] <<- dat$sRIN[y] + 0
        } else {
          dat$sRIN[y] <<- dat$sRIN[y] + (sRIN_scale()*((adjusted_fluro[y]-input$bg_level)/input$bg_level))
        }
      })
    })
    dat
  })
  
  #Adjust the x and y positions based on the dimensions of the HE image (the default x and y positions from the fluroescense program is 4500x4500 pixels)
  output$adjust_xy <- renderUI({
    radioButtons(inputId = "adjust_xy", label = "Adjust spot dimensions to that of HE image?", choices = c("Yes", "No"), selected = "No")
  })
  output$HE_xdim <- renderUI({
    numericInput(inputId = "he_xdim", label = "Enter the width of the HE image, in pixels", value = 1)
  })
  output$HE_ydim <- renderUI({
    numericInput(inputId = "he_ydim", label = "Enter the height of the HE image, in pixels", value = 1)
  })
  
  output$spot_size <- renderUI({
    if (is.null(st_rin_data)){
      return(NULL)
    }
    textInput(inputId = "spot_size", label = "Input size for spots in the plot", value = 1)
  })
  
  output$plot_whole_array <- renderPlot({
    colours <- c("black", "cyan", "yellow", "red", "dark red")
    plot_aes <- aes(st_rin_data()$nX, st_rin_data()$nY, colour = st_rin_data()$sRIN)
    ggplot(st_rin_data(), plot_aes) + geom_point(size = as.numeric(input$spot_size)) + scale_color_gradientn(colours = colours) + labs(color = "sRIN") +
      ylim(max(st_rin_data()$Y), min(st_rin_data()$Y)) + xlim(min(st_rin_data()$X), max(st_rin_data()$X)) +
      theme(axis.text = element_blank(), axis.text.x = element_blank(), axis.text.y = element_blank(),
            axis.title.x = element_blank(), axis.title.y = element_blank(), panel.background=element_blank(),
            panel.border=element_blank(),panel.grid.major=element_blank(),panel.grid.minor=element_blank(),
            plot.background=element_blank(),axis.ticks=element_blank()
      )
  })
  output$plot_name <- renderUI({
    textInput(inputId = "plot_name", "Please enter a name for your plot and a file extension (e.g. plot.pdf)", value = "plot.pdf")
  })
  
  plot_width <- reactive({
    switch(input$adjust_xy,
           No = 4500/72,
           Yes = input$he_xdim/72
    )
  })
  plot_height <- reactive({
    switch(input$adjust_xy,
           No = 4500/72,
           Yes = input$he_ydim/72
    )
  })
  output$dl_plot <- downloadHandler(
    filename = function() {
      input$plot_name
    },
    content = function(file) {
      ggsave(file, device = unlist(strsplit(input$plot_name,"\\."))[2], width = plot_width(), height = plot_height(), units = "in", limitsize = FALSE)
    }
  )
  
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
