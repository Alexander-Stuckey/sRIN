library(raster)
library(rgdal)

#Load probe sets. Probe 0 is the optional background probe
probe0 <- "black_plots/2017-08-16_Aligned_PNET180_G70_Probes0-4_C2_P0.tif"
probe1 <- "black_plots/2017-08-16_Aligned_PNET180_G70_Probes0-4_C2_P1.tif"
probe2 <- "black_plots/2017-08-16_Aligned_PNET180_G70_Probes0-4_C2_P2.tif"
probe3 <- "black_plots/2017-08-16_Aligned_PNET180_G70_Probes0-4_C2_P3.tif"
probe4 <- "black_plots/2017-08-16_Aligned_PNET180_G70_Probes0-4_C2_P4.tif"

probe0_raster <- raster(probe0)
probe1_raster <- raster(probe1)
probe2_raster <- raster(probe2)
probe3_raster <- raster(probe3)
probe4_raster <- raster(probe4)

#Set colours for plot. The first colour is the background colour. If you want black as the background, then the first two colours are black and black
colours <- c( "black", "cyan", "yellow", "red", "dark red")

#Gather all images into one object. calling plot() on this object will draw one plot for every probe
probe_brick <- brick(stack(probe0, probe1, probe2, probe3, probe4))

#Set the background threshold. Currently the 75th quantile of the background probe
threshold <- quantile(probe0_raster, probs=0.75)

#Create a dataset to use for normalisation
norm_data <- pmin(pmax(as.matrix(probe1_raster-threshold-probe0_raster),0),threshold)

#Normalise data for each probe and convert it to sRIN scale
p1_norm <- raster(ifelse(is.finite(pmin(pmax(as.matrix(probe1_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5), pmin(pmax(as.matrix(probe1_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5, 0))
p2_norm <- raster(ifelse(is.finite(pmin(pmax(as.matrix(probe2_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5), pmin(pmax(as.matrix(probe2_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5, 0))
p3_norm <- raster(ifelse(is.finite(pmin(pmax(as.matrix(probe3_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5), pmin(pmax(as.matrix(probe3_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5, 0))
p4_norm <- raster(ifelse(is.finite(pmin(pmax(as.matrix(probe4_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5), pmin(pmax(as.matrix(probe4_raster-threshold-probe0_raster),0),threshold)/norm_data*2.5, 0))

#Add all the values together in one object
sRIN <- overlay(brick(stack(p1_norm, p2_norm, p3_norm, p4_norm)), fun = function(a,b,c,d) (a+b+c+d), unstack=TRUE)
bb <- c(0,0,2.5,5,7.5,10)
image(sRIN,col=colours, breaks=bb, main = "Title", xaxt = "n", yaxt = "n", xlab = "", ylab = "")
legend(0,1,legend = c("10","7.5","5","2.5","0"),fill=rev(colours), title = "sRIN")

mean(base::subset(sRIN,values(sRIN)>0))

col2 <- c("black")
brks <- c(0,11)
image(sRIN,col=col2,breaks=brks)
legend(0,1,legend = paste("Error rate: ",round(sum(values(sRIN>10))/ncell(sRIN)*100, digits = 2), "%", sep=""))
