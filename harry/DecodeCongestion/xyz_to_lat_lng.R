library(tidyverse)
library(proj4)
hospital <- read_delim("DecodeCongestionData/hospitals.csv",delim=';')

proj4string <- "+proj=utm +zone=10 +north +west +ellps=WGS84 +datum=WGS84 +units=m +no_defs "

tmp <- project(hospital[,c("X_Coord","Y_Coord")], proj4string, inverse=TRUE)

hospital$lat <- tmp$y
hospital$lng <- tmp$x

write_csv(hospital,"DecodeCongestionData/hospitals.csv")



utm_to_latlng_converter <- function(path_to_file,x_name,y_name){
  tmp <- read_delim(path_to_file,delim=';')
  
  proj4string <- "+proj=utm +zone=10 +north +west +ellps=WGS84 +datum=WGS84 +units=m +no_defs "
  
  coord <- project(tmp[,c(x_name,y_name)], proj4string, inverse=TRUE)
  
  tmp$lat <- coord$y
  tmp$lng <- coord$x
  
  write_csv(tmp,path_to_file)
}

utm_to_latlng_converter('DecodeCongestionData/seniors-centres.csv',x_name='POINT_X',y_name='POINT_Y')
