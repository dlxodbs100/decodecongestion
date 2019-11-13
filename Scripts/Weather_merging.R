library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(zoo)

weather = list()

x = 1 

for (i in 7:12) { 
  name <- paste0("Data/en_climate_hourly_BC_1108395_",formatC(i, width=2, flag="0"), "-2013_P1H.csv")
  w_temp = read.csv(file = name,header = TRUE)
  weather[[x]] <- w_temp
  x = x+1
    }


for (i in 1:12) { 
  for (j in 14:18) {
    name <- paste0("Data/en_climate_hourly_BC_1108395_",formatC(i, width=2, flag="0"), "-20", formatC(j, width=2, flag="0"),"_P1H.csv")
    w_temp = read.csv(file = name,header = TRUE)
    weather[[x]] <- w_temp
    x = x+1
  }
}

for (i in 1:8) { 
  name <- paste0("Data/en_climate_hourly_BC_1108395_",formatC(i, width=2, flag="0"), "-2018_P1H.csv")
  w_temp = read.csv(file = name,header = TRUE)
  weather[[x]] <- w_temp
  x = x+1
  }

weatherall <- weather %>% bind_rows()
rm(weather)

weatherall = weatherall[ , c(6:10,28)]
weatherall = weatherall[2:nrow(weatherall), ]
weatherall$weather = na.locf(weatherall$Weather)

weatherall$Weather = NULL

weatherall$date = paste0(weatherall$Year,'-',formatC(weatherall$Month, width=2, flag="0"),'-',formatC(weatherall$Day, width=2, flag="0"))

weatherall$Day = NULL
weatherall$Month = NULL
weatherall$Year = NULL

colnames(weatherall)[1] = 'time'
colnames(weatherall)[2] = 'temp'

weatherall[weatherall==""] <- NA
weatherall[3] <- lapply(weatherall[3], na.locf)

weatherall[is.na(weatherall)] = 0 
weatherall$roadscore = tolower(weatherall$weather)
weatherall$roadscore[grepl('clear|cloudy|fog|haze|smoke',weatherall$roadscore)] = 'high'
weatherall$roadscore[grepl('rain|drizzle|thunderstorms',weatherall$roadscore)] = 'moderate'
weatherall$roadscore[grepl('snow|ice|freezing',weatherall$roadscore)] = 'low'

weatherall$visibility = tolower(weatherall$weather)
weatherall$visibility[grepl('clear',weatherall$visibility)] = 'high'
weatherall$visibility[grepl('rain|thunderstorms|cloudy|ice|freezing|drizzle',weatherall$visibility)] = 'moderate'
weatherall$visibility[grepl('snow|fog|haze|smoke',weatherall$visibility)] = 'low'

################### Sunrise ##################################
sun = read.csv(file = "Data/SunriseSunset.csv",header = TRUE)
sun = sun[,c(1,2,4,6)]

sun$Date = lapply(sun$Date,as.character)
sun$Day =  gsub("-.*$", "", sun$Date)
sun$Month =  gsub("^.*-", "", sun$Date)

sun$Month[grepl('Jan',sun$Month)] = '01'
sun$Month[grepl('Feb',sun$Month)] = '02'
sun$Month[grepl('Mar',sun$Month)] = '03'
sun$Month[grepl('Apr',sun$Month)] = '04'
sun$Month[grepl('May',sun$Month)] = '05'
sun$Month[grepl('Jun',sun$Month)] = '06'
sun$Month[grepl('Jul',sun$Month)] = '07'
sun$Month[grepl('Aug',sun$Month)] = '08'
sun$Month[grepl('Sep',sun$Month)] = '09'
sun$Month[grepl('Oct',sun$Month)] = '10'
sun$Month[grepl('Nov',sun$Month)] = '11'
sun$Month[grepl('Dec',sun$Month)] = '12'

sun$Month[sun$Month=='29'] = '02'
sun$Day[sun$Day=='Feb'] = '29'

sun$date = paste0(sun$Year,'-',sun$Month,'-',str_pad(sun$Day, width=2, side="left", pad="0"))
sun = sun[,c(3,4,7)]
sun$X = NULL
write.csv(sun,'output/sun.csv')


################### Merge ##################################
weather = merge(weatherall, sun, by=c('date'), all.x = TRUE)

write.csv(weather,'output/weather.csv')

