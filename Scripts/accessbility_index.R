library(tidyverse)
library(sf)
library(arulesViz)
library(raster)
library(rgdal)

vancouver.boundary <- st_read('Data/local-area-boundary.shp')

st_bbox(vancouver.boundary)

s.sp <- as(vancouver.boundary,"Spatial")

chosen.regions <- c('Fairview','Shaughnessy',
                    'Mount Pleasant','Arbutus-Ridge')
plot(s.sp)

s.sp.restricted <-subset(s.sp,name %in% chosen.regions)
plot(subset(s.sp,name %in% chosen.regions))

s <- sapply(slot(s.sp.restricted , 'polygons'), 
            function(i) spsample(i, n=100, type='regular', nsig=1))

s.merged <- do.call('rbind', s)
ids <- sapply(slot(s.sp.restricted, 'polygons'), function(i) slot(i, 'ID'))
npts <- sapply(s, function(i) nrow(i@coords))
pt_id <- rep(ids, npts)
s.final <- SpatialPointsDataFrame(s.merged, data=data.frame(poly_id=pt_id))
plot(s.sp.restricted) ; points(s.final, col=s.final$poly_id, pch=3, cex=0.5)
ids <- sapply(slot(s.sp.restricted, 'polygons'), function(i) slot(i, 'ID'))
sample.coord <- coordinates(s.final)

sampled.points <- data.frame(lat=sample.coord[,2],
                             lng=sample.coord[,1],
                             region=(s.sp$name)[s.final@data$poly_id %>%
                                                  as.character %>% 
                                                  as.numeric])

test.points <- sampled.points[1:2,]

keywords.seniors <- c("grocery store",
                      # 'park',
                      # 'medical clinic',
                      # 'church',
                      'community centre')

source('accessbility.R')
my.google.api.key <- "AIzaSyCUzx63N6oWJ_9u6bZgXbAV4wWxMhWqchs" 

locations <- c()
for (i in 1:nrow(test.locations)){
  locations[[i]] <-location_determiner(lat = test.points$lat[i],lng = test.points$lng[i],
                                       keywords = keywords.seniors,
                                       key = my.google.api.key)
}

chosen.locations <- lapply(locations,function(x){
  tmp <- lapply(x,function(y){
    y[1,]
  })
  return(do.call(rbind,tmp))
})

travel.modes=c("driving",
               # "cycling",
               # "transit",
               "walking")
time <- convert_time_UTC_seconds()

chosen.destinations <- lapply(chosen.locations,
                              function(x){
                                x %>% select(lat,lng)
                              })
origin <- test.points %>% select(lat,lng)
computed.travel.info <- c()           
for(i in 1:nrow(test.points)){
  computed.travel.info[[i]] <- distiance.time.determiner(origin = origin[i,],
                            destination = chosen.locations[[i]],
                            time=time,
                            travel.modes=travel.modes,
                            key=my.google.api.key)
}
