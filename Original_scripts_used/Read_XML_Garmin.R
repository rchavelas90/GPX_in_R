#### http://lwlss.net/GarminReports/GarminFunctions.R
#### Comments have been meade to ease the understanding of the script

# Read in xml files from Garmin ForeRunner
library(XML)
filename="GPX_test_files/test_file_5_Garmin"

doc=xmlParse(paste(filename,".gpx",sep=""),useInternalNodes=TRUE)

# We need to extract the simple tree structure containing
# the data which we can pass to xmlToDataFrame
top=xmlRoot(doc)
title=toString.XMLNode(top[[2]][[1]][[1]]) ##R# <Trk><name>
description=toString.XMLNode(top[[2]][[2]][[1]]) ##R# <Trk><desc>
data=toString.XMLNode(top[[2]][[3]]) ##R# <Trk><trkseg>
# All these tests in case description node is empty!
# When it is, data node is moved up one
# Really need to access nodes by name, but can't see how...
if(data=="NULL") data=toString.XMLNode(top[[2]][[2]])

if(data=="NULL") data=toString.XMLNode(top[[2]][[1]])

# Fill a data frame with interesting data
df=as.data.frame(xmlToDataFrame(data,c("numeric", "character", "integer")))
if(toString.XMLNode(top[[2]][[3]])=="NULL") {
 if(toString.XMLNode(top[[2]][[2]])=="NULL") {
  attribs=xmlSApply(top[[2]][[1]],xmlAttrs)
 }else{
  attribs=xmlSApply(top[[2]][[2]],xmlAttrs)
 }
}else{
 attribs=xmlSApply(top[[2]][[3]],xmlAttrs)
}
df$lon=as.numeric(attribs[1,])
df$lat=as.numeric(attribs[2,])

##R# Here the df is finished

colnames(df)=c("Elevation","DateTime","HeartRate","Longitude","Latitude")
df$Elevation=as.numeric(df$Elevation)
df$HeartRate=as.integer(df$HeartRate)
df$DateTime=as.character(df$DateTime)
##R# look at the structure of the columns
##R# Elevation (num) DateTime (chr) HeartRate (int) Longitude (num) Latitude (num)

# Convert timestamp to number of seconds since start of run
date=substr(df$DateTime[1],1,10) ##R# Obtains the date of the file, first 10 char
Time=substr(df$DateTime,12,19) ##R# Obtains the times of the file,  char 12-19
#head(df$DateTime)
#head(Time)
T0=strptime(Time[1],"%H:%M:%S") ## Starting time
Time=as.numeric(strptime(Time,"%H:%M:%S")-T0) ## Elapsed time
#head(Time)
df$Seconds=Time
#head(df$Seconds)

# Initialise columns
df$dNorth=0; df$dEast=0; df$dUp=0;
df$North=0; df$East=0; df$dDist=0; 
df$dDist2D=0; df$Dist2D=0
#head(df)
#write.csv(df,file="GPX_to_CSV.csv")

# Haversine formula is appropriate for calculating distances from lat/long
EarthRad=6371000 ##R# [in meters]
haverDist<-function(aLong,aLat,bLong,bLat){
 dLat=2*pi*(bLat-aLat)/360.0; dLon=2*pi*(bLong-aLong)/360.0
 a=(sin(dLat/2))^2+cos(2*pi*aLat/360)*cos(2*pi*bLat/360)*(sin(dLon/2)^2)
 return(EarthRad*2*atan2(sqrt(a),sqrt(1-a)))
}


# Calculate northings and eastings from location 0 to east and north coordinates
df$East=haverDist(df[1,"Longitude"],df[1,"Latitude"],df$Longitude,df[1,"Latitude"])*
 sign(df$Longitude-df[1,"Longitude"])
df$North=haverDist(df[1,"Longitude"],df[1,"Latitude"],df[1,"Longitude"],df$Latitude)*
 sign(df$Latitude-df[1,"Latitude"])

##R# pruebas de datos
##R# library(googleVis)
##R# head(df2)
##R# df2 <- data.frame(loc=numeric(6),factor=numeric(6))
##R# df2$loc=paste(df$Latitude[1:6],df$Longitude[1:6],sep=":")
##R# g2 <- gvisMap(df2,"loc",
##R#               options=list(showLine=TRUE,useMapTypeControl=T))
##R# plot(g2)

# Calculate changes in position for each dt
for (x in 2:(length(df$DateTime)-1)) {
 sEast=sign(df[x,"Longitude"]-df[1,"Longitude"])
 sNorth=sign(df[x,"Latitude"]-df[1,"Latitude"])
 df$dEast[x]=sEast*haverDist(df[x-1,"Longitude"],df[1,"Latitude"],df[x,"Longitude"],df[1,"Latitude"]) ##R# Compute change in Eastings 
 df$dNorth[x]=sNorth*haverDist(df[1,"Longitude"],df[x-1,"Latitude"],df[1,"Longitude"],df[x,"Latitude"]) ##R# Compute change inNortings 
 df$dUp[x]=df$Elevation[x]-df$Elevation[x-1]
 # 2D distance (ignoring hills)
 df$dDist2D[x]=haverDist(df[x-1,"Longitude"],df[x-1,"Latitude"],df[x,"Longitude"],df[x,"Latitude"])
}

##R# head(df)
df$dDist=sqrt(df$dNorth^2+df$dEast^2+df$dUp^2)
df$Dist=cumsum(df$dDist)
df$Dist2D=cumsum(df$dDist2D)
 
##R# options(digits=4)



# Fit a spline function to the GPS coordinates & elevation
east=splinefun(df$Seconds,df$East)
north=splinefun(df$Seconds,df$North)
up=splinefun(df$Seconds,df$Elevation)
dist=splinefun(df$Seconds,df$Dist)
hr=approxfun(df$Seconds,df$HeartRate) # Some gaps in heart rate record, linear interpolation more robust



# Do finite centred differencing to give smoothest rate(speed)/gradient estimates
##R# This is, to get the (Dist,Seconds) x+1 value minus the x-1 value to get the x (speed)
df$Speed=rep(0,length(df$Seconds))
df$Gradient=rep(0,length(df$Seconds))
for(x in 2:(length(df$Seconds)-1)){
 Dt=df[x+1,"Seconds"]-df[x-1,"Seconds"]
 Dd=df[x+1,"Dist"]-df[x-1,"Dist"]
 df[x,"Speed"]=Dd/Dt # m/s
 df[x,"Gradient"]=(df[x+1,"Elevation"]-df[x-1,"Elevation"])/Dd # m/m
}

##R# Complete values that werent taken into consideration with the finite centered dif.
##R# Testing speed plot(df$Seconds[1:40],df$Speed[1:40],type="o")
##R# head(df)
##R# require(ggplot2)
##R# ggplot(df,aes(x=Longitude,y=Latitude))+
##R# geom_path(aes(colour=Speed,size="50"))
##R# This plot can test the smoothing of the curve
##R# ggplot(df,aes(x=Dist))+
##R# geom_path(aes(y=Speed1,size="2"))+
##R# geom_path(aes(y=Speed2,color="red",,size="2"))

##R# This plot graphs the elevation against the distance
##R# ggplot(df,aes(x=Dist))+
##R# geom_path(aes(y=Elevation,size="2"))


df[1,"Speed"]=df[2,"Speed"]
df[length(df$Seconds),"Speed"]=df[length(df$Seconds)-1,"Speed"]
df[1,"Gradient"]=df[2,"Gradient"]
df[length(df$Seconds),"Gradient"]=df[length(df$Seconds)-1,"Gradient"]
##R# head(df)
##R# Record non-smooth speed
df$Speed1 <- df$Speed
##R# head(df$Speed1)
##R# Use only after smoothing speed



# Smooth speed as it is unrealistically noisy
df$Speed=smooth(df$Speed)
##R# Record smoothed speed
df$Speed2 <- as.numeric(df$Speed)
##R# tail(df)
##R# df[50:60,]
##R#head(df)

# Fit a spline function to rate (##R# speed)
speed=splinefun(df$Seconds,df$Speed)  # m/s
pace<-function(t) sapply(1/speed(t),max,0) 
ppace<-function(t) 1000*pace(t)/60 ##R# Unit conversion from s/m to min/km

##R# Testing the spline function and comparing its smoothness
##R# speed(0)
##R# tail(df)
##R# speed(2720)
##R# pace(2720)
##R# 1/speed(2720)
##R# pace(c(2720,2716,2711))

##R# Test different speed columns
##R# x <- 1:2720
##R# par(mfrow=c(2,1),bty="l",lab=c(10,10,0.5))
##R# plot(x,speed(x),type="l",xlim=c(0,2700),ylim=c(3,6))
##R# head(df)
##R# plot(df$Seconds,df$Speed2,type="l",xlim=c(0,2700),ylim=c(3,6))
##R# dev.off()

##R# Reset par in graphics 
##R# resetPar <- function() {
##R# dev.new()
##R# op <- par(no.readonly = TRUE)
##R# dev.off()
##R# op
##R# }
##R# resetPar()

# Update dataframe with speed and pace (after spline function)
df$Speed=speed(df$Seconds)
df$Pace=pace(df$Seconds)

##R# This plot can test the smoothing of the curve and the spline (notice that it
##R# does aparently nothing)
##R# head(df)
##R# str(df)
##R# png("speed_time.png",height=1000,width=4000,pointsize=1,res=300)
##R# ggplot(df,aes(x=Seconds))+
##R# geom_path(aes(y=Speed1),colour="blue",linetype=1)+
 ##R# geom_path(aes(y=Speed2),colour="green",linetype=1)+ ##R# See that spline fun is "useless" here
##R# geom_path(aes(y=Speed),colour="red",linetype=1)+
##R# coord_cartesian(xlim = c(0, 2700), ylim=c(2, 5))
##R# dev.off()


# Generate some plots ##R# not used now
reportfile=paste("prueba",".pdf",sep="")
print(paste("Building",reportfile))
pdf(reportfile)

# Generate time interpolation points (##R# Time spaced in equal intervals  to get from 0 to max Time)
Num=2001 
minT=0; maxT=max(df$Seconds)
interT=minT+(maxT-minT)*(0:Num)/Num ##R# generate Num+1 numbers from minT to maxT (delta T * Frac)
##R#tail(df)
##R#tail(interT)

# Create a colour function for plots
##R# colorRampPalette returns a function that takes an integer argument and returns
##R# that number of colors interpolating the given seuqnece
##R# colfunc(5)
colfunc=colorRampPalette(c("navy","white", "red3"),space="Lab")
cp=colfunc(500)
getCol<-function(colFrac) cp[1+round(499*colFrac)]

##R# source("Original_scripts_used/online_Hex_Color.R")
##R# testing colors
##R# onlineHexColor(getCol(0.01))




# Generate fractional variables for colouring plots
##R# Funtion hr with parameter interT (time) [(Val-min)/(max-min)]
##R# ******* This function computes a lot of sums, may be wise to simplify ir
hrFrac=(hr(interT)-min(hr(interT)))/(max(hr(interT))-min(hr(interT))) ##R# Heart rate
upFrac=(up(interT)-min(up(interT)))/(max(up(interT))-min(up(interT))) ##R# Elevation


##R#******* This suggests to use an unknown pace for max value, be careful, could use a 95% quantile
pmax=min(c(60*7/1000,max(pace(interT)))) # Else scales ruined by stopping and walking
pFrac=(pace(interT)-min(pace(interT)))/(pmax-min(pace(interT))) ##R# Pace

##R# Plotting the histogram - see how far is the max value of pace
##R# head(df)
##R# hist(pace(interT),breaks=30)

##R# This function graphs the quantile distibtion of the pace, so that we can have a better
##R# estimate of the max pace (stops must be considered, but not at this point)
##R# library("ggplot2")
##R# qplot(Pace,data=df,geom="histogram",binwidth=0.05)
##R# c <- rnorm(10000,0,0.05) or pace(interT)
##R# quantilePace <- quantile(pFrac,probs=seq(0,1,0.01))
##R# probs <- names(quantilePace)
##R# probs <- factor(probs,levels=probs)
##R#  qplot(y=quantilePace,x=probs)
##R# quantile(pace(interT),probs=0.95)





# Calculate Color Scales
##R# get length(cp) values (500) from the min hr to max hr (same for others)
hrLevels=min(hr(interT))+(1:length(cp))*(max(hr(interT))-min(hr(interT)))/length(cp) ##R# Heart rate
upLevels=min(up(interT))+(1:length(cp))*(max(up(interT))-min(up(interT)))/length(cp) ##R# Elevation
##R# Computes the max pace, if it is greater that 7 min/km it overwrites it with #7
pmax=min(c(7,max(ppace(interT)))) # Else scales ruined by stopping and walking
pLevels=min(ppace(interT))+(1:length(cp))*(pmax-min(ppace(interT)))/length(cp) ##R# Pace


##R# Trying to understand how everything is formed :P
##R# library("ggplot2")
##R# qplot(Pace,data=df,geom="histogram",binwidth=0.05)
##R# c <- rnorm(10000,0,0.05) or pace(interT)
##R# quantilePace <- quantile(pFrac,probs=seq(0,1,0.01))
##R# probs <- names(quantilePace)
##R# probs <- factor(probs,levels=probs)
##R#  qplot(y=quantilePace,x=probs)
##R# quantile(pace(interT),probs=0.95)


# Make a plotting dataframe, calculate displacement during each timestep
##R# A data frame with the useful info
plt=data.frame(time=interT, east=east(interT), north=north(interT), up=up(interT), hr=hr(interT),
               distance=sapply(interT,dist),speed=speed(interT),pace=pace(interT))

##R# all(dist(interT)==sapply(interT,dist))
##R# def.par <- par(no.readonly = TRUE) # save default, for resetting...

# Elevation trace
layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
plot(NULL,xlab="East (m)",ylab="North (m)",xlim=c(min(df$East),max(df$East)),
     ylim=c(min(df$North),max(df$North)),main=paste(title,date))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey")
points(east(interT),north(interT),pch=16,cex=0.6,col=getCol(upFrac))
# Draw legend
image(1, upLevels,matrix(data=upLevels, ncol=length(upLevels),nrow=1),col=cp,
      xlab="",ylab="Elevation (m)",xaxt="n")
layout(1)

# Heart rate trace
layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
plot(NULL,xlab="East (m)",ylab="North (m)",xlim=c(min(df$East),max(df$East)),ylim=c(min(df$North),max(df$North)),main=paste(title,date))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey")
points(east(interT),north(interT),pch=16,cex=0.6,col=getCol(hrFrac))
# Draw legend
image(1, hrLevels,matrix(data=hrLevels, ncol=length(hrLevels),nrow=1),col=cp,xlab="",ylab="Heart Rate (bpm)",xaxt="n")
layout(1)

# Pace trace
layout(matrix(data=c(1,2), nrow=1, ncol=2), widths=c(4,1), heights=c(1,1))
plot(NULL,xlab="East (m)",ylab="North (m)",xlim=c(min(df$East),max(df$East)),ylim=c(min(df$North),max(df$North)),main=paste(title,date))
rect(par("usr")[1], par("usr")[3], par("usr")[2], par("usr")[4], col = "grey")
points(east(interT),north(interT),pch=16,cex=0.6,col=getCol(pFrac))
# Draw legend
image(1, pLevels,matrix(data=pLevels, ncol=length(pLevels),nrow=1),col=cp,xlab="",ylab="Pace (min/km)",xaxt="n")
layout(1)

#####
op<-par(mfrow=c(2,2))

# Elevation timecourse
plot(df$Seconds/60,df$Elevation,xlab="Time (min)",ylab="Elevation (m)",type="l",lwd=2,col="red")

# Heart rate timecourse
plot(df$Seconds/60,df$HeartRate,xlab="Time (min)",ylab="Heart Rate (bpm)",type="l",lwd=2,col="red")

# Distance timecourse
#plot(df$Seconds/60,df$Dist/1000,xlab="Time (min)",ylab="Distance (km)",type="l",lwd=2,col="red")

# Speed timecourse
plot(df$Seconds/60,60*df$Speed/1000,xlab="Time (min)",ylab="Speed (km/min)",type="l",lwd=2,col="red")

# Pace timecourse
pmin=max(0,1000*min(df$Pace)/60)
pmax=min(7,1000*max(df$Pace)/60)
plot(df$Seconds/60,1000*df$Pace/60,xlab="Time (min)",ylab="Pace (min/km)",type="l",lwd=2,col="red",ylim=c(pmin,pmax))
title("Performance statistics with time (min)",line=-2,outer=TRUE)
par(op)

#####
op<-par(mfrow=c(2,2))

# Elevation timecourse
plot(df$Dist/1000,df$Elevation,xlab="Distance (km)",ylab="Elevation (m)",type="l",lwd=3,col="blue")

# Heart rate timecourse
plot(df$Dist/1000,df$HeartRate,xlab="Distance (km)",ylab="Heart Rate (bpm)",type="l",lwd=2,col="blue")

# Distance timecourse
#plot(df$Dist/1000,df$Dist/1000,xlab="Distance (km)",ylab="Distance (km)",type="l",lwd=2,col="blue")

# Speed timecourse
plot(df$Dist/1000,60*df$Speed/1000,xlab="Distance (km)",ylab="Speed (km/min)",type="l",lwd=2,col="blue")

# Pace timecourse
pmin=max(0,1000*min(df$Pace)/60)
pmax=min(7,1000*max(df$Pace)/60)
plot(df$Dist/1000,1000*df$Pace/60,xlab="Distance (km)",ylab="Pace (min/km)",type="l",lwd=2,col="blue",ylim=c(pmin,pmax))
title("Performance statistics with distance (km)",line=-2,outer=TRUE)
par(op)

#####
op<-par(mfrow=c(1,2))

hist(1000*plt$pace/60,breaks=21,xlab="Pace (min/km)",ylab="Frequency",main="")
hist(plt$hr,breaks=61,xlab="Heart Rate (bpm)",ylab="Frequency",main="")

title("Frequency histograms",line=-2,outer=TRUE)
par(op)

#####
op<-par(mfrow=c(2,2))

# Pace gradient correlation
gpCor=formatC(cor(df$Gradient,1000*df$Pace/60), digits=4)
plot(df$Gradient,1000*df$Pace/60,col="red",pch=16,xlab="Gradient",ylab="Pace (min/km)",main=paste("Correlation:",gpCor))

# Heart-rate gradient correlation
# need to strip out warming up period
minHR=mean(df$HeartRate)-1.98*sd(df$HeartRate)
times=df$Seconds[df$HeartRate>=minHR]
mint=min(times)
dfHR=df[df$Seconds>mint,]
gpCor=formatC(cor(dfHR$Gradient,dfHR$HeartRate), digits=4)
plot(dfHR$Gradient,dfHR$HeartRate,col="red",pch=16,xlab="Gradient",ylab="Heart Rate (bpm)",main=paste("Correlation:",gpCor))

# Pace time correlation
gpCor=formatC(cor(df$Seconds,1000*df$Pace/60), digits=4)
plot(df$Seconds,1000*df$Pace/60,col="red",pch=16,xlab="Time (s)",ylab="Pace (min/km)",main=paste("Correlation:",gpCor),
     ylim=c(1000*min(df$Pace)/60,min(c(7,1000*max(df$Pace)/60))))

# Heart-rate time correlation
gpCor=formatC(cor(dfHR$Seconds,dfHR$HeartRate), digits=4)
plot(dfHR$Seconds,dfHR$HeartRate,col="red",pch=16,xlab="Time (s)",ylab="Heart Rate (bpm)",main=paste("Correlation:",gpCor))

par(op)

dev.off()