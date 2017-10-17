
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
#
# http://shiny.rstudio.com
#

#Make some sliders that subset the data used to plot with.
#Display mean sRIN value for the selected area.

library(shiny)
library(shinydashboard)
library(raster)
library(rgdal)

options(shiny.maxRequestSize=30*1024^2)

shinyServer(function(input, output) {
  
  sRIN_scale <- reactive({
    10/(length(input$sRIN_data$name)+1)
  })
  
  #Toggle to set white or black background
  output$background <- renderUI({
    radioButtons("background", "What colour background do you want?", choices = c("White", "Black"), selected = "Black")
  })
  
  #Possibly add function to change threshold
  
  #Function to convert to sRIN values
  sRIN_values <- function(raster_layer, threshold, background, probe1_data) {
    str(raster_layer)
    str(threshold)
    str(background)
    str(probe1_data)
    tmp_data <- raster(ifelse(is.finite(pmin(pmax(as.matrix(raster_layer-threshold-background),0),threshold)/probe1_data*2.5),
                         pmin(pmax(as.matrix(raster_layer-threshold-background),0),threshold)/probe1_data*2.5,0))
    cat(min(values(tmp_data)))
    cat(max(values(tmp_data)))
    return(tmp_data)
  }

  plot_image <- function(){
    
    if (input$background == "White") {
      colours <- c("white", "cyan", "yellow", "red", "dark red")
    } else {
      colours <- c("black", "cyan", "yellow", "red", "dark red")
    }
    
    sRIN_background <- raster(input$no_probes$datapath)
    threshold <- quantile(sRIN_background, probs=0.75)
    
    sRIN_probe1 <- raster(input$sRIN_probe1$datapath)
    sRIN_probe2 <- raster(input$sRIN_probe2$datapath)
    sRIN_probe3 <- raster(input$sRIN_probe3$datapath)
    sRIN_probe4 <- raster(input$sRIN_probe4$datapath)
    
    probe1_data <- sRIN_values(as.matrix(sRIN_probe1), threshold, as.matrix(sRIN_background), as.matrix(sRIN_probe1))
    probe2_data <- sRIN_values(as.matrix(sRIN_probe2), threshold, as.matrix(sRIN_background), as.matrix(sRIN_probe1))
    probe3_data <- sRIN_values(as.matrix(sRIN_probe3), threshold, as.matrix(sRIN_background), as.matrix(sRIN_probe1))
    probe4_data <- sRIN_values(as.matrix(sRIN_probe4), threshold, as.matrix(sRIN_background), as.matrix(sRIN_probe1))
    
   # sRIN_data <- overlay(brick(stack(probe1_data, probe2_data, probe3_data, probe4_data)), fun = function(a,b,c,d) (a+b+c+d))
    sRIN_data <- brick(stack(probe1_data, probe2_data, probe3_data, probe4_data))
    #str(sRIN_data)
    cat(max(values(sRIN_data)))
    #st_rin_data <-brick(stack(input$sRIN_data$datapath, input$no_probes$datapath))
    #srin_calc <- letters[seq(from = 1, length.out = length(input$sRIN_data)+1)]
    #st_rin_data_overlay <- overlay(st_rin_data, fun = function(srin_calc) ((sum(srin_calc[1:length(srin_calc)-1]/4)-(srin_calc[length(srin_calc)]))), unstack = TRUE)
    
    #Change the 3 in the following line if you want to adjust how many standard deviations above the meant you want the top end to be.
    #nbrk <- seq(from = cellStats(st_rin_data, stat = "mean")[length(srin_calc)], to = 65535, length.out = 5)
    #nbrk <- append(nbrk, cellStats(st_rin_data_overlay, stat = "min"), after = 0)
   # nbrk <- append(nbrk, cellStats(st_rin_data_overlay, stat = "max"), after = length(nbrk))
    nbrk <- c(0,2,4,6,8,10)
    #The legend position is set with the first two values of the legend() method, currently set at (max(x)-65, max(y)/2). Thus, middle of the right hand side.
    #img_data <- as.matrix(st_rin_data_overlay)
    #img_data_floor <- pmax(img_data, 0)
    #image(t(apply(img_data_floor,2,rev)), col = colours, breaks = nbrk)#, xaxt="n", yaxt="n", ann=FALSE)#, xlim=c(0,nrow(st_rin_data_overlay)), ylim=c(0,ncol(st_rin_data_overlay)))#, legend=FALSE)
    image(sRIN_data, col = colours, breaks = nbrk)
    #legend(0.2, 1, legend = c("10", "7.5", "5", "2.5", "0"), fill = rev(colours))
    
  }
  
  output$plot_whole_array <- renderPlot({
    plot_image()
  })
  
  output$plot_name <- renderUI({
    textInput(inputId = "plot_name", "Please enter a name for your plot", value = "sRIN_plot.pdf")
  })
  
  output$dl_plot <- downloadHandler(
    filename = function(){
      input$plot_name
    },
    content = function(file) {
      pdf(file, width = 10, height = 10)
      plot_image()
      dev.off()
    }
  )
  
})
