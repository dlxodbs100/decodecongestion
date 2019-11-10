library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)
library(zoo)
library(lattice)


incident = read.csv(file = "Data/DataSet_01.csv",header = TRUE)
incident$CDate = lapply(incident$CDate,as.character)
colnames(incident)[5]='time'

incident[, c('Month','Day','Year')]=str_split_fixed(incident$CDate, "/", 3)
incident$date = paste0(incident$Year,'-',str_pad(incident$Month, width=2, side="left", pad="0"),'-',str_pad(incident$Day, width=2, side="left", pad="0"))

incident[incident==""] = NA
incident$time = str_pad(incident$time, width=5, side="left", pad="0")

incident$CDate = NULL
incident$Month = NULL
incident$Day = NULL
incident$Year = NULL

weather = read.csv(file = "output/weather.csv",header = TRUE)
sun = read.csv(file = "output/sun.csv",header = TRUE)

incident = merge(incident, weather[ , c("temp", "weather", "roadscore","visibility","date","time")], by = c("date","time"))
incident = merge(incident, sun, by = "date")

incident$SRhour = as.numeric(gsub(":.*$", "", incident$Sunrise))
incident$SShour = as.numeric(gsub(":.*$", "", incident$Sunset))
incident$Chour = as.numeric(gsub(":.*$", "", incident$time))

incident$dark = "0"
incident$dark[incident$Chour<incident$SRhour] = "1"
incident$dark[incident$Chour>incident$SShour] = "1"

incident$Chour = NULL
incident$SRhour = NULL
incident$SShour = NULL
incident$X = NULL


write.csv(incident,'output/incident.csv')

