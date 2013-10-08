## Gráficas

##R# Load required packages
require(ggplot2)
library(googleVis)
library(ggmap)

##R# Reset par in graphics 
resetPar <- function() {
 dev.new()
 op <- par(no.readonly = TRUE)
 dev.off()
 op
}
resetPar()

##R# Test different speed columns (With spline function and without it)
x <- 1:length(df$Speed)
par(mfrow=c(2,1),bty="l",lab=c(10,10,0.5))
plot(x,speed(x),type="l",xlim=c(0,length(df$Speed)),ylim=c(0,40))
plot(df$Seconds,df$Speed2,type="l",xlim=c(0,length(df$Speed)),ylim=c(0,40))
dev.off()

##R# This plot can test the smoothing of the curve and the smooth

head(df)
str(df)
png("speed_time.png",height=1000,width=4000,pointsize=1,res=300)
ggplot(df,aes(x=Seconds))+
 geom_path(aes(y=SpeedKm1),colour="blue",linetype=1)+
 geom_path(aes(y=SpeedKm2),colour="green",linetype=1)+ 
 coord_cartesian(xlim = c(0, length(df$Seconds)), ylim=c(0, 160))
dev.off()

##R# This plot can test the smoothing of the curve
png("speed_time.png",height=1000,width=4000,pointsize=1,res=300)
ggplot(df,aes(x=Seconds))+
 geom_path(aes(y=SpeedKm1),size=2)+
 geom_path(aes(y=SpeedKm2),size=2,color="red")+
 scale_y_continuous(limits=c(0,150),breaks=seq(0,150,10))+
 scale_x_continuous(limits=c(0,length(df$Seconds)),breaks=seq(0,length(df$Seconds),5))
dev.off()

##R# This plot graphs the elevation against the distance
ggplot(df,aes(x=Dist))+
 geom_path(aes(y=Elevation,size=2))

##R# Testing speed plot
ggplot(df,aes(x=Longitude,y=Latitude))+
 geom_path(aes(colour=SpeedKm1,size=4))

##R# Speed line vs time
ggplot(df,aes(x=Seconds,y=SpeedKm1))+
 geom_line()+
 scale_y_continuous(limits=c(0,150),breaks=seq(0,150,10))+
 scale_x_continuous(limits=c(0,length(df$Seconds)),breaks=seq(0,length(df$Seconds),5))

# Testing maps with googleVis
# create a new data frame with the adequate format df2
df2 <- data.frame(loc=numeric(nrow(df)),factor=numeric(nrow(df)))
df2$loc=paste(df$Latitude[1:nrow(df)],df$Longitude[1:nrow(df)],sep=":")
df2$factor <- df$SpeedKm2
head(df)
g2 <- gvisMap(df2,"loc",
              options=list(showLine=TRUE,
                           lineColor='#81BEF7',
                           lineWidth=3,
                           mapType='normal',
                           useMapTypeControl=T,
                           showTip=TRUE,
                           enableScrollWheel=T))
plot(g2)


##Testing with ggplot 2 mapping options
ggplot(df) + 
 geom_line(aes(x=Longitude, y=Latitude, col=SpeedKm2,size=SpeedKm2))+
 coord_map(project="mercator")

## Thks to http://www.mollietaylor.com/2013/02/gps-basemaps-in-r-using-getmap.html
# using get_map function from ggmap
head(df)
mapImageData <- get_map(location = c(lon = mean(df$Longitude), 
                                     lat = mean(df$Latitude)),
                        color = "color", # bw / color
                        source = "google",
                        maptype = "roadmap",
                        # api_key = "your_api_key", # only needed for source = "cloudmade"
                        zoom = 15)

pathcolor <- "#F8971F"

ggmap(mapImageData,
      extent = "panel", # "panel" keeps in axes, etc. or "device"
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") + 
 geom_line(aes(x=Longitude, 
               y=Latitude, col=SpeedKm2,size=SpeedKm2),
           data=df)

#using get_map function from ggmap

mapImageData <- get_map(location = c(lon = mean(df$Longitude), 
                                     lat = mean(df$Latitude)),
                        color = "color", # bw / color
                        source = "google",
                        maptype = "roadmap",
                        # api_key = "your_api_key", # only needed for source = "cloudmade"
                        zoom = 15)

mapImageData <-  get_googlemap(center= c(lon = mean(df$Longitude), 
                                         lat = mean(df$Latitude)),
                               maptype="roadmap",
                               zoom=15,
                               size=c(640,640),
                               scale=2
                               )

ggmap(mapImageData,
      extent = "panel", # "panel" keeps in axes, etc. or "device"
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") + 
 geom_line(aes(x=Longitude, 
               y=Latitude, col=SpeedKm2),
           data=df,
           size=5)

