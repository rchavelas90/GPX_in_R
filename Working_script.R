getwd()
rm(list=ls(all.names=T))

#Load readGPX Functions
source("readGPX.R")

#Verify that readGPX() and .readGPX.element() functions are loaded correctly
ls(all.names=T)

#Read a test file from the GPX_test_files_folder (test_file==Tec_to_hm_2013-09-17T205421Z.gpx)
gpx_file <- readGPX("GPX_test_files/test_file.gpx")
#2nd Read a test file from the GPX_test_files_folder (test_file==Tec_to_hm_2013-09-17T205421Z.gpx)
gpx_file <- readGPX("GPX_test_files/test_file_4_Ixtapan.gpx")

## Data structure
str(gpx_file)
## Extract the track information
data2 <- gpx_file$tracks[[1]][[1]]
head(data2)
str(data2)

##R# This creates the column names that are appropiate for the script that generates
##R# the report
df <- data.frame(data2$ele,data2$time,0,data2$lon,data2$lat,stringsAsFactors = F)
colnames(df)=c("Elevation","DateTime","HeartRate","Longitude","Latitude")
df$Elevation <- as.numeric(df$Elevation)
str(df)
head(df)










