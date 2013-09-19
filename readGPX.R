
##Description of the function
##http://plotkml.r-forge.r-project.org/readGPX.html
##Function Source
##https://r-forge.r-project.org/scm/viewvc.php/pkg/R/readGPX.R?view=markup&revision=279&root=plotkml

##This function has been modified by Ricardo Chavelas
###Fixed problem with metadata loading
###Remove "Bounds" loading

##Comments were added for clear undesrtanding by RCM with ##R#

##Future possible actions: 
### Check all code to match exactly with GPX XML shema (and extenions)

##R# Loads XML pakcage
library(XML)
readGPX <- function(
 gpx.file,
 metadata = TRUE,
 waypoints = TRUE, 
 tracks = TRUE,  
 routes = TRUE   
)
 
{    
 options(warn = -1) ##R# Removes all warnings   
 
 ##R# Reads elements with readGPX.element() function
 if(metadata==TRUE) { metadata <- .readGPX.element(gpx.file, "metadata") }     
 if(waypoints==TRUE) { waypoints <- .readGPX.element(gpx.file, "wpt") }
 if(tracks==TRUE) { tracks <- .readGPX.element(gpx.file, "trk") }
 if(routes==TRUE) { routes <- .readGPX.element(gpx.file, "rte") }
 
 
 ##R# creates the named list with all the elements of the GPX file
 gpx <- list(metadata=metadata, waypoints=waypoints, tracks=tracks, routes=routes)
 return(gpx)
}

## Read various elements from a *.gpx file:

.readGPX.element <- function(gpx.file, element) {
 # element = "metadata", "wpt", "rte", "trk", ##R# "bounds"
 
 ## gives a internal C-level document pointer 
 ret <- xmlTreeParse(gpx.file, useInternalNodes = TRUE)
 # top structure ##R# as a XMLNode object: 
 top <- xmlRoot(ret)
 
 # check if there is any content: ##R# XML tag name of each of the sub nodes
 # of a given XMLNode object
 if(any(grep(element, names(top)))) {
  
  # tracks:
  if(element=="trk"){   
   ret <- NULL
   nu <- which(names(top) %in% element)
   for(c in seq_along(nu)){
    lst <- which(names(top[[nu[c]]]) %in% "trkseg")
    nm <- names(top[[nu[c]]][[lst[1]]][[1]])
    ret[[c]] <- list(NULL)
    for(i in seq_along(lst)) {
     trkpt <- top[[nu[c]]][[lst[i]]]
     ret[[c]][[i]] <- data.frame(NULL)
     ## get columns (http://www.topografix.com/GPX/1/1/#type_wptType)
     lon <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lon"))
     lat <- as.numeric(xmlSApply(trkpt, xmlGetAttr, "lat"))
     ret[[c]][[i]][1:length(lon),"lon"] <- lon
     ret[[c]][[i]][1:length(lat),"lat"] <- lat
     if(!nm[[1]]=="NULL"){
      for(j in 1:length(nm)){
       xm <- as.character(sapply(sapply(xmlChildren(trkpt), function(x) x[[nm[[j]]]]), xmlValue))
       ret[[c]][[i]][1:length(xm), nm[[j]]] <- xm 
      }
     } 
    }
    names(ret[[c]]) <- xmlValue(top[[nu[c]]][["name"]])
   }   
  }
  
  if(element=="wpt"){
   ret <- data.frame(NULL)
   nu <- which(names(top) %in% element)
   nm <- names(top[[nu[1]]])
   for(i in seq_along(nu)) {
    # coordinates:
    ret[i, "lon"] <- as.numeric(xmlGetAttr(top[[nu[i]]], "lon"))
    ret[i, "lat"] <- as.numeric(xmlGetAttr(top[[nu[i]]], "lat"))
    if(!nm[[1]]=="NULL"){
     for(j in 1:length(nm)){
      ret[i, nm[[j]]] <- xmlValue(xmlChildren(top[[nu[i]]])[[nm[[j]]]])
     }  
    }
   }
  }
  
  if(element=="rte"){
   ret <- NULL
   nu <- which(names(top) %in% element)
   for(c in seq_along(nu)){
    ret[[c]] <- data.frame(NULL)
    lst <- which(names(top[[nu[c]]]) %in% "rtept")
    nm <- names(top[[nu[c]]][[lst[1]]])
    for(i in seq_along(lst)) {
     rtept <- top[[nu[c]]][[lst[i]]]
     ret[[c]][i, "lon"] <- as.numeric(xmlGetAttr(rtept, "lon"))
     ret[[c]][i, "lat"] <- as.numeric(xmlGetAttr(rtept, "lat"))
     if(!nm[[1]]=="NULL"){
      for(j in c("name","cmt","desc","sym","type")){
       try(ret[[c]][i, j] <- xmlValue(rtept[[j]]), silent = TRUE)
      }
     } 
    }
    names(ret)[c] <- xmlValue(top[[nu[c]]][["name"]])
   }
  }
  
  # bounds (is not usefull in this version, however in newer versions may be usefull)
  if(element=="bounds"){
   nu <- which(names(top) %in% element)
   ret <- matrix(rep(NA, 4), nrow=2, dimnames = list(c("lat", "lon"), c("min", "max")))
   # coordinates:
   ret[1,1] <- as.numeric(xmlGetAttr(top[[nu[1]]], "minlon"))
   ret[1,2] <- as.numeric(xmlGetAttr(top[[nu[1]]], "maxlon"))
   ret[2,1] <- as.numeric(xmlGetAttr(top[[nu[1]]], "minlat"))
   ret[2,2] <- as.numeric(xmlGetAttr(top[[nu[1]]], "maxlat"))
  }
  
  # metadata
  ##R# loads metadata according to XML shema and names the fields that are included
  ## in the metadata of the GPX file
  if(element=="metadata"){
   lst <- c("name","desc","author","copyright","link","time","keywords","bounds",
            "extensions")
   nu <- which(names(top) %in% element)
   if(!nu[[1]]=="NULL"){  
    numlst <- which(lst%in%names(top[[nu]]))
    lst2 <- c()
    for( i in seq_along(numlst)){
     lst2[i] <- lst[numlst[i]]
    }
    ret <- data.frame(NULL)
    for(i in seq_along(numlst)) {
     ret[1,lst2[i]] <- xmlValue(top[[nu]][[i]])
    }
   }
  }
 }
 else { ret <- NULL }
 
 return(ret)
}

# enf of script;