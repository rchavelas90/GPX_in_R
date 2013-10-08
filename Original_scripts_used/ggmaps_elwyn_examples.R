## As seen http://www.mollietaylor.com/2013/02/gps-basemaps-in-r-using-getmap.html
gps <- read.csv("Original_scripts_used/elwyn.csv", 
                header = TRUE)

library(ggmap)



mapImageData <- get_map(location = c(lon = mean(gps$Longitude), 
                                     lat = 33.824),
                        color = "color", # bw / color
                        source = "osm",
                        #maptype = "roadmap",
                        # api_key = "your_api_key", # only needed for source = "cloudmade"
                        zoom = 17)

pathcolor <- "#F8971F"

ggmap(mapImageData,
      extent = "device", # "panel" keeps in axes, etc. or "device"
      ylab = "Latitude",
      xlab = "Longitude",
      legend = "right") + 
 geom_path(aes(x = Longitude, # path outline
               y = Latitude),
           data = gps,
           colour = "black",
           size = 2) +
 geom_path(aes(x = Longitude, # path
               y = Latitude),
           colour = pathcolor,
           data = gps,
           size = 1.4) ## +
 ## labs(x = "Longitude", y = "Latitude") # if you do extent = "panel"
