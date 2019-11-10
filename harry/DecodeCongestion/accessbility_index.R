library(tidyverse)
library(sf)
vancouver.boundary <- st_read('DecodeCongestionData/local-area-boundary/local-area-boundary.shp')

st_bbox(vancouver.boundary)

s.sp <- as(vancouver.boundary,"Spatial")

chosen.regions <- c('Fairview','Shaughnessy',
                    'Mount Pleasant','Arbutus-Ridge')
plot(s.sp)

s.sp.restricted <-subset(s.sp,name %in% chosen.regions)
plot(subset(s.sp,name %in% chosen.regions))

s <- sapply(slot(s.sp.restricted, 'polygons'), 
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

# test.points <- sampled.points[1:2,]

keywords.seniors <- c("grocery store",
                      'park',
                      'medical clinic',
                      'church',
                      'community centre')

source('accessbility.R')
my.google.api.key <- "AIzaSyB0La6ERoMBER3YzVfE2DGull6EFu9tsbA" 

locations <- c()
for (i in 1:nrow(sampled.points)){
  tryCatch({
  locations[[i]] <-location_determiner(lat = sampled.points$lat[i],
                                       lng = sampled.points$lng[i],
                                       keywords = keywords.seniors,
                                       key = my.google.api.key)
  }, error=function(cond){
    message(cond)
    message(as.character(i))
    return(NA)
  }
  )
  # print(i)
}

chosen.locations <- lapply(locations,function(x){
  tmp <- lapply(x,function(y){
    y[1,]
  })
  return(do.call(rbind,tmp))
})
my.google.api.key <- "AIzaSyCSqS1UrSb8CgWj9hVRil0cnGNOqMKytK4"
  
travel.modes=c("walking",
               # "cycling",
               # "driving",
               "transit")

time <- convert_time_UTC_seconds()

chosen.destinations <- lapply(chosen.locations,
                              function(x){
                                x %>% select(lat,lng)
                              })

origin <- sampled.points %>% select(lat,lng)

computed.travel.info <- c()

for(i in 1:nrow(sampled.points)){
  tryCatch({
    computed.travel.info[[i]] <- distiance.time.determiner(origin = origin[i,],
                                                           destination = chosen.locations[[i]],
                                                           time=time,
                                                           travel.modes=travel.modes,
                                                           key=my.google.api.key)
  }, error=function(cond){
    message(cond)
    message(as.character(i))
    return(NA)
  }
  )
  # print(i)
}

penalization.function <- function(t,t.cut.off=15){
  if(t<=t.cut.off) { return(1) } else{ return(exp(-(t-t.cut.off)/5))}
}

# lapply(toy,function(x){
#   mean(apply(x,1,function(y){penalization.function(y[2])}))
# }) %>% unlist() %>% mean()

lapply(computed.travel.info,function(X){
  mapply(function(x,z){
    if(z=='walking'){
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off = 10)}))
    } else{
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off=15)}))
    }
  },X,names(X)) %>% mean()
}) %>% unlist() -> accessibility.index

sampled.points$index <- accessibility.index

sampled.points %>% 
  group_by(region) %>% 
  summarise(n=n(),
            region_index=mean(index),
            region_index_sd = sd(index),
            region_index_min = min(index),
            region_index_med = median(index)) -> index.region.table

ggplot(data=sampled.points,aes(x=index,colour=region)) +
  geom_density() +
  facet_grid(~region)

#plotting
lapply(computed.travel.info,function(X){
  mapply(function(x,z){
    if(z=='walking'){
      mean(apply(x,1,function(y){penalization.function(y[2])}))
    } else{
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off=20)}))
    }
  },X,names(X))
}) -> indices
indices <- do.call(rbind,indices)
sampled.points$walking.index <- indices[,1]
sampled.points$transit.index <- indices[,2]

lapply(computed.travel.info,function(X){
  mapply(function(x,z){
    if(z=='walking'){
      mean(apply(x,1,function(y){penalization.function(y[2])}))
    } else{
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off=20)}))
    }
  },X,names(X))
}) -> indices
indices <- do.call(rbind,indices)
sampled.points$walking.index <- indices[,1]
sampled.points$transit.index <- indices[,2]

sample(c(1:nrow(sampled.points)),by=sampled.points$region)

toy <- computed.travel.info[[1]]
toy.walking <- toy$walking[1,'time']
toy.transit <- toy$transit[1,'time']
toy.plot.walking <- data.frame(time=seq(0,20,by=0.01),
                       index=sapply(seq(0,20,by=0.01),function(x){penalization.function(x,10)}),colour=1)
gg.walking <- ggplot(data=toy.plot.walking,aes(x=time,y=index)) +
  geom_line() +
  geom_point() +
  annotate("point", x = 7.92, y = penalization.function(7.92,10), colour = "red",size=5)+
  theme_classic()

toy.transit <- toy$transit[1,'time']
toy.plot.transit  <- data.frame(time=seq(0,25,by=0.01),
                               index=sapply(seq(0,25,by=0.01),function(x){penalization.function(x,15)}),colour=1)

gg.transit <- ggplot(data=toy.plot.transit,aes(x=time,y=index)) +
  geom_line() +
  geom_point() +
  annotate("point", x = 4, y = penalization.function(4,15), colour = "red",size=5) +
  theme_classic()
####
for(i in 1:length(computed.travel.info)){
  computed.travel.info[[i]]$walking<- computed.travel.info[[i]]$walking %>% as.data.frame() %>%  
    mutate(index = ifelse(time<=10,1,exp(-(time-10)/5)))
  computed.travel.info[[i]]$transit<- computed.travel.info[[i]]$transit %>% as.data.frame() %>%  
    mutate(index = ifelse(time<=15,1,exp(-(time-15)/5)))
}
by.category <- c()

by.category <- lapply(c(1:5),function(s){
  tmp <- lapply(computed.travel.info,function(xx){
    mean(xx$walking[s,3],xx$transit[s,3])}) %>% unlist()
})

sampled.points$index_grocery <- by.category[[1]]
sampled.points$index_park <- by.category[[2]]
sampled.points$index_medical_clinic <- by.category[[3]]
sampled.points$church <- by.category[[4]]
sampled.points$cc <- by.category[[5]]

sampled.points %>% 
  group_by(region) %>% 
  summarise(avg_gs = mean(index_grocery),
            avg_park = mean(index_park),
            avg_mc = mean(index_medical_clinic),
            avg_church = mean(church),
            avg_cc = mean(cc)) -> tb


lapply(computed.travel.info,function(X){
  mapply(function(x,z){
    if(z=='walking'){
      apply(x,1,function(y){penalization.function(y[2],t.cut.off = 10)})
    } else{
      apply(x,1,function(y){penalization.function(y[2],t.cut.off=15)})
    }
  },X,names(X)) %>% mean()
})  -> tmp





####
additional.senior.centres <- data.frame(lat=c(49.256450,49.249691),
                                       lng=c(-123.157833,-123.159120))

travel.modes=c("walking",
               # "cycling",
               # "driving",
               "transit")

time <- convert_time_UTC_seconds()

origin <- sampled.points %>% select(lat,lng)

computed.travel.info.scenario <- c()

arbutus.index <- which(sampled.points$region=='Arbutus-Ridge')

for(i in arbutus.index){
  tryCatch({
    computed.travel.info.scenario[[i]] <- distiance.time.determiner(origin = origin[i,],
                                                           destination = additional.senior.centres,
                                                           time=time,
                                                           travel.modes=travel.modes,
                                                           key=my.google.api.key)
  }, error=function(cond){
    message(cond)
    message(as.character(i))
    return(NA)
  }
  )
  # print(i)
}

# first location
sampled.points.arbutus <- sampled.points %>% 
  filter(region=='Arbutus-Ridge')
computed.travel.info.arbutus <- computed.travel.info[arbutus.index]

first.computed.travel.info <- c()

for(i in 1:length(computed.travel.info.arbutus)){
  OLD <-computed.travel.info.arbutus[[i]]
  NEW <- computed.travel.info.scenario[[i]]
  if(NEW$walking[1,2]<OLD$walking[5,2]) {
    OLD$walking[5,] <- NEW$walking[1,]
  }
  if(NEW$transit[1,2]<OLD$transit[5,2]) {
    OLD$transit[5,] <- NEW$transit[1,]
  }
  first.computed.travel.info[[i]] <- OLD
}

# first.computed.travel.info <- mapply(function(OLD,NEW){
#   
#   if(NEW$walking[1,2]<OLD$walking[5,2]) {
#     OLD$walking[5,] <- NEW$walking[1,]
#   }
#   if(NEW$transit[1,2]<OLD$transit[5,2]) {
#     OLD$transit[5,] <- NEW$transit[1,]
#   }
#   return(list(walking=OLD$walking,transit=OLD$transit))
#   
# },computed.travel.info.arbutus,computed.travel.info.scenario)

lapply(first.computed.travel.info,function(X){
  mapply(function(x,z){
    if(z=='walking'){
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off = 10)}))
    } else{
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off=15)}))
    }
  },X,names(X)) %>% mean()
}) %>% unlist() -> accessibility.index.first

sampled.points.arbutus$index.first <- accessibility.index.first

# second location
second.computed.travel.info <- c()

for(i in 1:length(computed.travel.info.arbutus)){
  OLD <-computed.travel.info.arbutus[[i]]
  NEW <- computed.travel.info.scenario[[i]]
  if(NEW$walking[2,2]<OLD$walking[5,2]) {
    OLD$walking[5,] <- NEW$walking[2,]
  }
  if(NEW$transit[2,2]<OLD$transit[5,2]) {
    OLD$transit[5,] <- NEW$transit[2,]
  }
  second.computed.travel.info[[i]] <- OLD
}


# second.computed.travel.info <- mapply(function(OLD,NEW){
#   if(NEW$walking[2,2]<OLD$walking[5,2]) {
#     OLD$walking[5,] <- NEW$walking[2,]
#   }
#   if(NEW$transit[2,2]<OLD$transit[5,2]) {
#     OLD$transit[5,] <- NEW$transit[2,]
#   }
#   return(OLD)
# },computed.travel.info.arbutus,computed.travel.info.scenario)

lapply(second.computed.travel.info,function(X){
  mapply(function(x,z){
    if(z=='walking'){
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off = 10)}))
    } else{
      mean(apply(x,1,function(y){penalization.function(y[2],t.cut.off=15)}))
    }
  },X,names(X)) %>% mean()
}) %>% unlist() -> accessibility.index.second

sampled.points %>% 
  group_by(region) %>% 
  summarise(avg_gs = mean(index_grocery),
            avg_park = mean(index_park),
            avg_mc = mean(index_medical_clinic),
            avg_church = mean(church),
            avg_cc = mean(cc)) -> tb


sampled.points.arbutus$index.second <- accessibility.index.second

sampled.points.arbutus %>% select(index,index.first,index.second) %>% colMeans()
