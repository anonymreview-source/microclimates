

setwd("~/Dropbox/2_supervisions/Carolina Reyes-Puig/microclimates/microclimates/dataloggers_Castro")

# check not only lacking days, but also missing hours
# main: IVILAR12
# next to check: IOLIVE22, IGULPI1
# for vipers: IBUBANUE3
# for Castro San Paio: IMODIV1

rm(list=ls())

library(tidyverse)
library(rvest)
library(readr)
library(lubridate)
library(openair)

# Download, clean and organize data

dates <- seq(as.Date("2021-05-01"), as.Date("2022-06-30"), "days")
urls <- paste("https://www.wunderground.com/dashboard/pws/IMODIV1/table", dates, dates, "daily", sep='/')


#url <-  "https://www.wunderground.com/dashboard/pws/IVILAR12/table/2022-05-10/2022-05-10/daily"
#url <-  "https://www.wunderground.com/dashboard/pws/IOLIVE22/table/2022-06-30/2022-06-30/daily"
#url <-  "https://www.wunderground.com/dashboard/pws/IGULPI1/table/2022-06-30/2022-06-30/daily"


weatherData <- list() # to store results

a <- 1 # counter

for(i in urls){

  urlx <- url(i, "rb")
  
  df <- urlx %>%
    read_html() %>%
    html_nodes("table") %>%
    html_table(fill = T)
  
  close(urlx)
  
  dfc <- tryCatch(df[[4]],  error = function(e) {
    return(NULL)
  })
  if(is.null(dfc)){ 
    weatherData[[a]] <- NA
    a = a + 1
    next 
  }
  
  dfc$Date <- dates[a]
  
  if(nrow(dfc) == 1){
    weatherData[[a]] <- NA
  }else{
    col.clean <- c(2,3,4,6,7,8,9,10,12)
    
    for(j in col.clean){
      dfc[,j] <- parse_number(dfc[,j])
    }
    weatherData[[a]] <- dfc
  }
  a = a + 1
}

sum(is.na(weatherData))

dfc <- weatherData[[1]]
dfc <- weatherData[[56]]

timep <- parse_date_time(dfc$Time, orders = '%H:%M %p')

#plot(timep, dfc$Temperature, type='l', ylab = 'Temperature (F)')
plot(timep, 5/9 * (dfc$Temperature - 32), type='l', ylab = 'Temperature (ºC)')

plot(timep, dfc$Solar, type='l')

#plot(timep, dfc$Speed, type='l', ylab = 'wind speed (mph)')
plot(timep, dfc$Speed * 0.44704, type='l', ylab = 'wind speed (m/s)')


weather <- weatherData[56][[1]]
for(i in 2:length(weatherData)){
  if(is.na(weatherData[i]) == TRUE){
    next
  } else{
    weather <- rbind(weather, weatherData[i][[1]])
  }
}

weather <- na.omit(weather)

weather$date <- parse_date_time(paste(weather$Date, weather$Time),
                                orders = 'Y-m-d %H:%M %p')

# save(weather, file='../weather/weatherCastro.RData')

# calculate hourly averages
weather_h <- data.frame(timeAverage(weather, avg.time = "hour", statistic = "mean"))
# save(weather_h, file='../weather/weatherCastro_hourly.RData')


# we already downloaded the data. Read in weather data

load(file='../weather/weatherCastro.RData')
load(file='../weather/weatherCastro_hourly.RData')

with(weather, plot(date, 5/9 * (Temperature - 32), 
                   type='l', ylab = 'Temperature (ºC)'))

with(weather_h, points(date, 5/9 * (Temperature - 32), 
                       type='l', ylab = 'Temperature (ºC)', col='red'))


with(weather_h, plot(date, Solar, type='l', ylab = 'Solar radiation (W/m^2)'))
with(weather_h, plot(date, Speed * 0.44704, 
                     type='l', ylab = 'wind speed (m/s)'))
with(weather_h, plot(date, Precip..Rate. * 25.4, type='l', ylab = 'Rainfall (mm)')) # from inches to mm




