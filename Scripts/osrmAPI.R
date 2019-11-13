######
#
# OSRM for Taxi Project
#
# Author: Tae Yoon Lee
# Modifying osrmRoute from osrm package
#
# Created on: 6/24/17
# Modified on:
# Modified by:
#
# Notes:
#
#######

osrmRoute <- function(taxi.trip,overview=FALSE, 
                      osrm.server = "http://router.project-osrm.org/",
                      triptype = 'driving'){

    # build the query
  src.lat <- taxi.trip$GPSMeterOnLat
  src.lon <- taxi.trip$GPSMeterOnLong
  dst.lat <- taxi.trip$GPSMeterOffLat
  dst.lon <- taxi.trip$GPSMeterOnLong
  
  if(anyNA(c(src.lat,src.lon,dst.lat,dst.lon))) {stop("NA GPS")}
  
  req <- paste(osrm.server,
                 "route/v1/",triptype, "/", 
                src.lon, ",", src.lat,
                 ";",
                 dst.lon,",",dst.lat, 
                 "?alternatives=false&geometries=polyline&steps=false&overview=",
                 tolower(overview),
                 sep="")
    
    # Sending the query
    resRaw <- RCurl::getURL(utils::URLencode(req),
                            useragent = "'osrm' R package")
    # Deal with \\u stuff
    vres <- jsonlite::validate(resRaw)[1]
    if(!vres){
      resRaw <- gsub(pattern = "[\\]", replacement = "zorglub", x = resRaw)
    }
    # Parse the results
    res <- jsonlite::fromJSON(resRaw)
    
    # Error handling
    e <- simpleError(res$message)
    if(res$code != "Ok"){
      print('error')
      return(list(OnAddress = NA,
                  OffAddress = NA,
                  duration = NA,
                  distance =NA))}
    
    return(list(OnAddress = res$waypoints$name[1],
                OffAddress = res$waypoints$name[2],
             duration = res$routes$duration/60, # in minutes
             distance = res$routes$distance/1000) # in meters
           )
}

updateTripsOSRM <- function(trips, StartRow, NumRequests,overview=FALSE, 
                            osrm.server = "http://router.project-osrm.org/",
                            triptype = 'driving'){
  
  EndRow <- StartRow + NumRequests-1
  gps.index <- grep("GPS",colnames(trips))
  NAcount <- 0
  
  # don't request more data than you have
  if(EndRow > nrow(trips)){EndRow<-nrow(trips)}
  pb <- txtProgressBar(min = 0, max = NumRequests, style = 3)
  
  for(row in StartRow:EndRow) {
    
    setTxtProgressBar(pb, row)
    # print progress updates
    # if(row==StartRow) {
    #   print("Starting API Call:")
    #   print(" 0% Complete...")
    # } else if(row==round(0.05*(EndRow-StartRow)+StartRow, 0)){
    #   print("5% Complete...")
    # }
    # else if(row==round(0.25*(EndRow-StartRow)+StartRow, 0)){
    #   print("25% Complete...")
    # } else if(row==round(0.5*(EndRow-StartRow)+StartRow, 0)){
    #   print("50% Complete...")
    # } else if(row==round(0.75*(EndRow-StartRow)+StartRow, 0)){
    #   print("75% Complete...")
    # }
    #   else if(row==round(0.90*(EndRow-StartRow)+StartRow, 0)){
    #     print("90% Complete...")
    #   }
    # }
    
    
    # query API and store results in dataframe
    
    # skip if gps is missing and report this
   
    if(anyNA(trips[row,gps.index])) {
      NAcount <- NAcount +1
    trips$OSRM.Distance[row] <- NA
    trips$OSRM.Duration[row] <- NA
    trips$OSRM.StartAddress[row] <- NA
    trips$OSRM.StopAddress[row] <- NA
    next}
    
    result <- osrmRoute(trips[row,],overview = overview,osrm.server = osrm.server,triptype=triptype)
    
    trips$OSRM.Distance[row] <- result$distance
    trips$OSRM.Duration[row] <- result$duration
    trips$OSRM.StartAddress[row] <- result$OnAddress
    trips$OSRM.StopAddress[row] <- result$OffAddress
  }
  close(pb)
  # final status update
  print("100% Complete...")
  print(paste0("Summary of status of all ", EndRow-StartRow + 1, " requests:"))
  print(paste0("NA Count is ", NAcount, '.'))
  return(trips)
}