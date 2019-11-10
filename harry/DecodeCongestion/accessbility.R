# MC Model
# Given a geographical location (lat,long), we determine the closest location of the following
# things:
# grocery store, parks, medical clinic, church, community centre, and senior centre

library(RCurl) # https requests
library(RJSONIO) # manage JSON

# my.google.api.key <- "AIzaSyCUzx63N6oWJ_9u6bZgXbAV4wWxMhWqchs" 
# another.google.api.key <- "AIzaSyDvmlDixNMb8092-noG1OZnNJOafXkHcxs"

url <- function(lat, lng, keyword, rankby=distance,API_key, num_sample=3,return.call="json", units="metric"){
  root <- "https://maps.googleapis.com/maps/api/place/nearbysearch/"
  u <- paste0(root,
              return.call,
              "?location=", lat,",",lng,
              "&keyword=", keyword,
              "&rankby=distance",
              # "&pagetoken=",num_sample,
              "&key=", API_key)
  return(URLencode(u))
}

#test
# keyword <- 'grocery store'
# keywords <- keywords.seniors
# lat <- 49.267941
# lng <- -123.247360
# key <- my.google.api.key
# verbose=T

location_determiner <- function(lat,lng,keywords,key=google.api.key,verbose=F){
  
  location.results <- c()
  
  for(keyword in keywords){
    u <- url(lat, lng, keyword=keyword, API_key=key)
    
    if(verbose){print(u)}
    
    d.json <- getURL(u)
    d.list <- fromJSON(d.json, simplifyWithNames = FALSE)
    
    if(verbose){print(d.list)}
    
    if(d.list$status=="OK"){
      
      results <- lapply(d.list$results,function(tmp.result){
        if(anyNA(tmp.result)){
          data.frame(name=NA,
                     lat=NA,
                     lng=NA,
                     rating=NA,
                     user.rating=NA
          )
        }else{
        data.frame(name=tmp.result$name,
                          tmp.result$geometry$location %>% as.vector(),
                          rating=tmp.result$rating,
                          user.rating= tmp.result$user_ratings_total
                          )
      }})
      results <- do.call(rbind,results)
    } else {
      results <- NA
    }
      
    #   if(d.list$status=="OVER_QUERY_LIMIT") {
    #   results <- NA
    # } else {
    #   stop("Distance Matrix API request issue. Status: ", d.list$status)
    # }
    location.results[[keyword]] <- results
  }
  return(location.results)
}

# test <- location_determiner(lat=lat,lng=lng,keywords = c("park","grocery store"),key=my.google.api.key)

convert_time_UTC_seconds <- function(time="2019/12/4 12:00:00 PM",format="%Y/%m/%d %H:%M:%S"){
  #"2019/12/1 7:00:00 PM"
  time <- as.POSIXct(time,format=format)
  attr(time, "tzone") <- "UTC" 
  return(as.integer(lubridate::seconds(time)))
}

url.distance <- function(origin, destinations, time, 
                         travel.mode, transit.mode=paste0(c('train','bus'),collapse = '|'),
                          API_key, return.call="json", units="metric"){
  # origin and destination is a vector of c(Lat, Long)
  # example Distance Matrix API call:
  # https://maps.googleapis.com/maps/api/distancematrix/json?
  # units=metric&origins=49.25764,-123.1755&destinations=49.25758,-123.1718&key=YOURAPIKEY
  root <- "https://maps.google.com/maps/api/distancematrix/"
  if(travel.mode!='transit'){
  u <- paste0(root,
              return.call,
              "?units=", units,
              "&origins=", origin[1], ",", origin[2],
              "&destinations=", destinations,
              "&mode=",travel.mode,
              "&departure_time=",time,
              "&key=", API_key)
  } else{
    u <- paste0(root,
                return.call,
                "?units=", units,
                "&origins=", origin[1], ",", origin[2],
                "&destinations=", destinations,
                "&mode=",travel.mode,
                "&transit_mode=",transit.mode,
                "&departure_time=",time,
                "&key=", API_key)
  }
  return(URLencode(u))
}


#toy
# origin <- data.frame(lat=lat,lng=lng)
# destination <-origin
# destination[2,] <- c(49.25794,-123.2474)
# travel.mode = 'walking'
# key

# look <- distiance.time.determiner(origin = origin,
#                           destination = destination,
#                           travel.modes=c("driving","walking","transit","cycling"),
#                           time  =time,
#                           key=my.google.api.key)

distiance.time.determiner <- function(origin, destination, time, 
                                      travel.modes=c("driving","walking","transit","cycling"),
                                      transit.mode=paste0(c('train','bus'),collapse = '|'),
                                      key, return.call="json", units="metric",
                                      verbose=F){
  
  if (nrow(destination)>1) {destination = paste0(destination$lat,",",destination$lng,
                                                 collapse="|")}
  tmp.result <- c()
  for (travel.mode in travel.modes){
    u <- url.distance(origin=origin, destination=destination, time=time, 
                      travel.mode=travel.mode,
                      transit.mode=paste0(c('train','bus'),collapse = '|'),
                      API_key=key, return.call="json", units="metric")
    
    if(verbose){print(u)}
    
    d.json <- getURL(u)
    d.list <- fromJSON(d.json, simplifyWithNames = FALSE)
    
    if(verbose){print(d.list)}

    if(d.list$status=="OK"){
      tmp <- lapply(d.list$rows[[1]]$elements,function(x){
        if(x$status=="OK"){
          return(c(x$distance$value/1000,x$duration$value/60))
        } else{
          return(NA)
        }
      })
      tmp <- do.call(rbind,tmp)
    } else {
      stop("Distance Matrix API request issue. Status: ", d.list$status)
    }
    colnames(tmp) <- c("distance",'time')
    tmp.result[[travel.mode]] <- tmp
  }
  return(tmp.result)

  }

# reverse geocoding
# u <- "https://maps.googleapis.com/maps/api/geocode/json?latlng=49.306542,-123.208637&key=AIzaSyCUzx63N6oWJ_9u6bZgXbAV4wWxMhWqchs"
# d.json <- getURL(u)
# d.list2 <- fromJSON(d.json, simplifyWithNames = FALSE)
