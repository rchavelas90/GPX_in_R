## Great-circle distance calculations in R
## http://www.r-bloggers.com/great-circle-distance-calculations-in-r/

# Convert degrees to radians (Here the longitude and latitude coordinates
# are given in radian, i.e. the latitude and longitude decimal degrees (DD) 
# converted to radians like so)
deg2rad <- function(deg) return(deg*pi/180)

# Calculates the geodesic distance between two points specified
# by radian latitude/longitude using the
# Spherical Law of Cosines (slc)
gcd.slc <- function(long1, lat1, long2, lat2) {
 R <- 6371 # Earth mean radius [km]
 d <- acos(sin(lat1)*sin(lat2) + cos(lat1)*cos(lat2) * cos(long2-long1)) * R
 return(d) # Distance in km
}

# alternative formulation that is more robust at small distances is 
# the Haversine formula 
# Calculates the geodesic distance between two points specified by 
# radian latitude/longitude using the
# Haversine formula (hf)
gcd.hf <- function(long1, lat1, long2, lat2) {
 deg2rad <- function(deg) return(deg*pi/180)
 long1 <- deg2rad(long1)
 lat1 <- deg2rad(lat1)
 long2 <- deg2rad(long2)
 lat2 <- deg2rad(lat2)
 R <- 6371000 # Earth mean radius [##R# in m]
 delta.long <- (long2 - long1)
 delta.lat <- (lat2 - lat1)
 a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
 c <- 2 * asin(min(1,sqrt(a)))
 d = R * c
 return(d) # Distance in km
}


##R# Haversine formula (Other method)
haverDist<-function(aLong,aLat,bLong,bLat){
 EarthRad=6371000
 dLat=2*pi*(bLat-aLat)/360.0; dLon=2*pi*(bLong-aLong)/360.0
 a=(sin(dLat/2))^2+cos(2*pi*aLat/360)*cos(2*pi*bLat/360)*(sin(dLon/2)^2)
 return(EarthRad*2*atan2(sqrt(a),sqrt(1-a)))
}

##R# Missing one formula 

gcd.hf(-8.75218925066292, 53.3013500645757, -8.75214423984289, 53.3013293612748)
haverDist(-8.75218925066292, 53.3013500645757, -8.75214423984289, 53.3013293612748)
deg2rad <- function(deg) return(deg*pi/180)
deg2rad(c(-8.75218925066292, 53.3013500645757, -8.75214423984289, 53.3013293612748))

gcd.hf <- function(long1, lat1, long2, lat2) {
 deg2rad <- function(deg) return(deg*pi/180)
 long1 <- deg2rad(long1)
 lat1 <- deg2rad(lat1)
 long2 <- deg2rad(long2)
 lat2 <- deg2rad(lat2)
 R <- 6371000 # Earth mean radius [##R# in m]
 delta.long <- (long2 - long1)
 delta.lat <- (lat2 - lat1)
 a <- sin(delta.lat/2)^2 + cos(lat1) * cos(lat2) * sin(delta.long/2)^2
 c <- 2 * asin(min(1,sqrt(a)))
 d = R * c
 return(d) # Distance in km
}


a <- gcd.hf(long1=-8.75197065062821, lat1=53.30123363994062,
       long2=-8.75183586962521, lat2=53.3011516649276)
sprintf("%.12f",a)
 
 
 
 
 
 