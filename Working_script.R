getwd()
rm(list=ls(all.names=T))
#Load readGPX Functions
source("readGPX.R")
#Verify that readGPX() and .readGPX.element() functions are loaded correctly
ls(all.names=T)
#Read a test file from the GPX_test_files_folder (test_file==Tec_to_hm_2013-09-17T205421Z.gpx)
data <- readGPX("GPX_test_files/test_file.gpx")
#2nd Read a test file from the GPX_test_files_folder (test_file==Tec_to_hm_2013-09-17T205421Z.gpx)
data <- readGPX("GPX_test_files/test_file_2.gpx")
data_a <- .readGPX.element("GPX_test_files/test_file_2.gpx", "name")

str(data$metadata)

str(data)

data2 <- data$tracks[[1]][[1]]
head(data2)
head(data2$time)
data2$delta <- c()
data2$time <- as.POSIXct(data2$time, format="%Y-%m-%dT%H:%M:%SZ")
plot(data2$lat,data2$lon)
data2$loc=paste(data2$lat,data2$lon,sep=":")

library(RgoogleMaps)
MyMap <- MapBackground(lat=data2$lat,lon=data2$lon)
PlotOnStaticMap(MyMap=MyMap,lat=data2$lat,lon=data2$lon,FUN=lines,destfile="ejm.png")


library(googleVis)
g <- gvisGeoChart(data2,"loc",options=list(region="MX"))
plot(g)
gvisGe

head(data2)
g2 <- gvisMap(data2,"loc",options=list(showLine=TRUE))
plot(g2)

data <- readGPX("2013-09-17-19-53.gpx",metadata=F,bounds=F,waypoints=F,route=F)
setwd("GPX")

str(data)
str(data$tracks)
data2 <- data$tracks[[1]][[1]]
head(data2)
head(data2$time)
data2$delta <- c()
data2$time <- as.POSIXct(data2$time, format="%Y-%m-%dT%H:%M:%SZ")
plot(data2$lat,data2$lon)
data2$loc=paste(data2$lat,data2$lon,sep=":")

library(RgoogleMaps)
MyMap <- MapBackground(lat=data2$lat,lon=data2$lon)
PlotOnStaticMap(MyMap=MyMap,lat=data2$lat,lon=data2$lon,FUN=lines,destfile="ejm.png")


library(googleVis)
g <- gvisGeoChart(data2,"loc",options=list(region="MX"))
plot(g)
gvisGe

head(data2)
g2 <- gvisMap(data2,"loc",options=list(showLine=TRUE))
plot(g2)