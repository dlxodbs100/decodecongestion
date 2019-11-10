library(ggplot2)
library(dplyr)
library(tidyverse)
library(stringr)


weather = list()

for (i in 1:12) { 
  for (j in 13:19) {
    name <- paste0("Data/en_climate_hourly_BC_1108395_12-20", formatC(j, width=2, flag="0),"_P1H.csv")
  w_temp = read.csv(file = name,header = TRUE)
  weather[[i]] <- w_temp
}

if (!file.exists("data/input/14100017-eng.zip"))
weatherall <- weather %>% bind_rows()


summary(weather)