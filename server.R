
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#Make some sliders that subset the data used to plot with.
#Display mean sRIN value for the selected area.

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
      numericInput(inputId = unlist(strsplit(input$sRIN_data$name[x], "\\.")[1]), 
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
  
#Sliders to subset the array heatmap
  output$xlims <- renderUI({
    sliderInput(inputId = "xlims", label = "Set min and max values to be plotted", min = min(st_rin_data()$X), max = max(st_rin_data()$X),
                step = 30, value = c(min(st_rin_data()$X), max(st_rin_data()$X))
    )
  })
  output$ylims <- renderUI({
    sliderInput(inputId = "ylims", label = "Set min and max values to be plotted", min = min(st_rin_data()$Y), max = max(st_rin_data()$Y),
                step = 30, value = c(min(st_rin_data()$Y), max(st_rin_data()$Y))
    )
  })
  
#Subset the st_rin_data based on the sliders
  st_rin_subset <- reactive({
    xmin <- as.numeric(unlist(strsplit(paste(input$xlims, collapse = " "), split = " "))[1])
    xmax <- as.numeric(unlist(strsplit(paste(input$xlims, collapse = " "), split = " "))[2])
    ymin <- as.numeric(unlist(strsplit(paste(input$ylims, collapse = " "), split = " "))[1])
    ymax <- as.numeric(unlist(strsplit(paste(input$ylims, collapse = " "), split = " "))[2])
    subset(st_rin_data(), (st_rin_data()$X %in% c(xmin:xmax)) & (st_rin_data()$Y %in% c(ymin:ymax)))
  })
  
#Height and width for the plot when saving
  output$HE_xdim <- renderUI({
    numericInput(inputId = "he_xdim", label = "Enter the width of the image, in pixels", value = 1)
  })
  output$HE_ydim <- renderUI({
    numericInput(inputId = "he_ydim", label = "Enter the height of the image, in pixels", value = 1)
  })
  
  output$spot_size <- renderUI({
    if (is.null(st_rin_data)){
      return(NULL)
    }
    numericInput(inputId = "spot_size", label = "Input size for spots in the plot", value = 1)
  })
  
  output$average_sRIN <- renderText({
    paste("Mean sRIN value in plot: ", mean(st_rin_subset()$sRIN[st_rin_subset()$sRIN > 0]), sep = "")
  })
  
  output$plot_whole_array <- renderPlot({
    colours <- c("black", "cyan", "yellow", "red", "dark red")
    plot_aes <- aes(st_rin_subset()$X, st_rin_subset()$Y, colour = st_rin_subset()$sRIN)
    ggplot(st_rin_subset(), plot_aes) + geom_point(size = as.numeric(input$spot_size)) + scale_color_gradientn(colours = colours) + labs(color = "sRIN") +
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
    input$he_xdim/72
  })
  plot_height <- reactive({
    Yes = input$he_ydim/72
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
