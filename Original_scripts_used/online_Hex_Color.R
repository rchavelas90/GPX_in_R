##R# This function search colorhexa.com for any heaxadecimal color and opens the browser at given color
##R# If no color is defined it wil open white #ffffff
##R# if color is not in website it opens automatically the color browser

onlineHexColor <- function(color="#ffffff") {
 txt <- substr(color,2,7)
 txt <- tolower(txt)
 url <- paste("http://www.colorhexa.com/",txt,"#information",sep="")
 browseURL(url) 
}


