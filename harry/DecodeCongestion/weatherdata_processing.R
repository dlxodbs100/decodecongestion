library(tidyverse)

# for obtaining the weather data, just run the following line
# weather.sh 
# more info can be found on climate.weather.gc.ca

data.path <- 'DecodeCongestionData/Weather/'
weather.files <- list.files(data.path)

weather.data <- lapply(weather.files,function(x) {read_csv(paste0(data.path,x))})

weather.data <- do.call(rbind,weather.data)

weather.data <- weather.data %>% 
  mutate(Month=as.numeric(Month),
         Day=as.numeric(Day)) %>% 
  arrange(Year,Month,Day,Time)

write_csv(weather.data,paste0(data.path,'weather_final.csv'))
