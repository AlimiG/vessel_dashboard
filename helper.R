library(tidyverse)

 earth.dist <- function (long1, lat1, long2, lat2)
 {
   rad <- pi/180
   a1 <- lat1 * rad
   a2 <- long1 * rad
   b1 <- lat2 * rad
   b2 <- long2 * rad
   dlon <- b2 - a2
   dlat <- b1 - a1
   a <- (sin(dlat/2))^2 + cos(a1) * cos(b1) * (sin(dlon/2))^2
   c <- 2 * atan2(sqrt(a), sqrt(1 - a))
   R <- 6378.145
   d <- R * c
   return(d)
 }

 
get_row_distance <- function(dataset){
  dataset <- dataset %>% 
    mutate(distance = earth.dist(LON,LAT,lag(LON),lag(LAT))) 
  
  row <- which.max(dataset$distance)
  map_data <- data.frame(
  lat = c(dataset[row,]$LAT,dataset[row-1,]$LAT),
  lon = c(dataset[row,]$LON,dataset[row-1,]$LON),
  point = c('End Point',"Start Point"),
  dis = c(paste(as.character(round(max(dataset$distance,na.rm = TRUE),1)),'m'),paste("distance: ",as.character(max(dataset$distance,na.rm = TRUE)),'m'))
  )
   
  return(map_data)
  
}



 
 
 
 