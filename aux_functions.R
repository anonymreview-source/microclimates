

### Useful functions

# define function to read logger data
read.logger <- function(period, logger, T.ext=F, H.ext=F){
  path <- paste0(getwd(), period, logger)
  if(T.ext == T){
    logger.file <- list.files(path, pattern='*T.csv', full.names=T)
  } else if (H.ext == T){
    logger.file <- list.files(path, pattern='*H.csv', full.names=T)
  }else{
    logger.file <- list.files(path, pattern='*.csv', full.names=T)
  }
  read.csv(logger.file, header=F, dec=',', skip=20)
}


# function to convert dates and fix temperature / humidity data
clean.logger <- function(data, T.ext = F, H.ext = F){
  require(chron)
  
  dtparts <- t(as.data.frame(strsplit(data[,1],' ')))
  row.names(dtparts) <- NULL
  date <- chron(dates = dtparts[,1], times = dtparts[,2], 
                 format = c('d/m/y','h:m:s'))
  
  data2 <- as.data.frame(date)
  if(T.ext == T){
    data2$temperature <- as.double(paste(data$V3, data$V4, sep='.'))
  }else if (H.ext == T){
    data2$rh <- as.double(paste(data$V3, data$V4, sep='.'))
  }else{
    data2$temperature <- as.double(paste(data$V3, data$V4, sep='.'))
  }
  
  data2$date <- as.POSIXct(data2$date)
  return(data2)
}

# clean.logger2 for different time format
clean.logger2 <- function(data, T.ext = F, H.ext = F){
  require(chron)
  
  dtparts <- t(as.data.frame(strsplit(data[,1],' ')))
  row.names(dtparts) <- NULL
  date <- chron(dates = dtparts[,1], times = dtparts[,2], 
                 format = c('d-m-y','h:m:s'))
  
  data2 <- as.data.frame(date)
  if(T.ext == T){
    data2$temperature <- as.double(paste(data$V3, data$V4, sep='.'))
  }else if (H.ext == T){
    data2$rh <- as.double(paste(data$V3, data$V4, sep='.'))
  }else{
    data2$temperature <- as.double(paste(data$V3, data$V4, sep='.'))
  }
  
  data2$date <- as.POSIXct(data2$date)
  return(data2)
}


# function to read a bunch of loggers at the same time
readLoggers <- function(loggers, periods = c('May_July2021', 'July_November2021', 
                                             'Nov_March20212022', 'March_June2022')){
  for(period in periods){
    for(l in loggers){
      if(length(list.files(paste0(getwd(),'/dataloggers_Castro/', period, '/', l))) == 0){
        next
      } 
      else if(length(list.files(paste0(getwd(),'/dataloggers_Castro/', period, '/', l))) == 2){
        aX <- read.logger(paste0('/dataloggers_Castro/', period, '/'), l, T.ext = T)
        aXrh <- read.logger(paste0('/dataloggers_Castro/', period, '/'), l, H.ext = T)
        
        if(period == 'May_July2021'){
          assign(paste0(l,'_1'), clean.logger(aX, T.ext = T), envir = parent.frame())
          assign(paste0(l,'rh_1'), clean.logger(aXrh, H.ext = T), envir = parent.frame())
        } 
        else if (period == 'July_November2021'){
          assign(paste0(l,'_2'), clean.logger2(aX, T.ext = T), envir = parent.frame())
          assign(paste0(l,'rh_2'), clean.logger2(aXrh, H.ext = T), envir = parent.frame())
        }
        else if (period == 'Nov_March20212022'){
          assign(paste0(l,'_3'), clean.logger(aX, T.ext = T), envir = parent.frame())
          assign(paste0(l,'rh_3'), clean.logger(aXrh, H.ext = T), envir = parent.frame())
        }
        else if (period == 'March_June2022'){
          assign(paste0(l,'_4'), clean.logger(aX, T.ext = T), envir = parent.frame())
          assign(paste0(l,'rh_4'), clean.logger(aXrh, H.ext = T), envir = parent.frame())
        }
      }
      else if(length(list.files(paste0(getwd(),'/dataloggers_Castro/', period, '/', l))) == 1){
        if(period == 'May_July2021'){
          aX <- read.logger(paste0('/dataloggers_Castro/', period, '/'), l)
          assign(paste0(l,'_1'), clean.logger(aX, T.ext = T), envir = parent.frame())
        } 
        else if (period == 'July_November2021'){
          aX <- read.logger(paste0('/dataloggers_Castro/', period, '/'), l)
          assign(paste0(l,'_2'), clean.logger2(aX, T.ext = T), envir = parent.frame())
        }
        else if (period == 'Nov_March20212022'){
          aX <- read.logger(paste0('/dataloggers_Castro/', period, '/'), l)
          assign(paste0(l,'_3'), clean.logger(aX, T.ext = T), envir = parent.frame())
        }
        else if (period == 'March_June2022'){
          aX <- read.logger(paste0('/dataloggers_Castro/', period, '/'), l)
          assign(paste0(l,'_4'), clean.logger(aX, T.ext = T), envir = parent.frame())
        }
      }
    }
  }
}

# function to add logged data (air temp, hum) to weather station data
prepare.weather <- function(weather, temp, hum){
  require(lubridate)
  
  temp$date <- round_date(as.POSIXct(temp$date), unit = "hour")
  weather1 <- merge(weather, temp, by='date')
  
  hum$date <- round_date(as.POSIXct(hum$date), unit = "hour")
  weather1 <- merge(weather1, hum, by='date')
  
  return(weather1)
}


# function to retrieve output from micro objects
retrieve.output <- function(micro, weather){
  
  micro.output <- list()
  
  # retrieve ouptut
  date <- weather$date[1:nrow(micro$metout)]
  metout <- as.data.frame(micro$metout) # retrieve above ground microclimatic conditions, min shade
  shadmet <- as.data.frame(micro$shadmet) # retrieve above ground microclimatic conditions, max shade
  soil <- as.data.frame(micro$soil) # retrieve soil temperatures, minimum shade
  shadsoil <- as.data.frame(micro$shadsoil) # retrieve soil temperatures, maximum shade
  soilmoist <- as.data.frame(micro$soilmoist) # retrieve soil moisture, minimum shade
  shadmoist <- as.data.frame(micro$shadmoist) # retrieve soil moisture, maximum shade
  humid <- as.data.frame(micro$humid) # retrieve soil humidity, minimum shade
  shadhumid <- as.data.frame(micro$shadhumid) # retrieve soil humidity, maximum shade
  soilpot <- as.data.frame(micro$soilpot) # retrieve soil water potential, minimum shade
  shadpot <- as.data.frame(micro$shadpot) # retrieve soil water potential, maximum shade
  
  # append dates
  micro.output[['metout']] <- cbind(date, metout)
  micro.output[['shadmet']] <- cbind(date, shadmet)
  micro.output[['soil']] <- cbind(date, soil)
  micro.output[['shadsoil']] <- cbind(date, shadsoil)
  micro.output[['soilmoist']] <- cbind(date, soilmoist)
  micro.output[['shadmoist']] <- cbind(date, shadmoist)
  micro.output[['humid']] <- cbind(date, humid)
  micro.output[['shadhumid']] <- cbind(date, shadhumid)
  micro.output[['soilpot']] <- cbind(date, soilpot)
  micro.output[['shadpot']] <- cbind(date, shadpot)
  
  return(micro.output)
}



# run.microclimate
run.microclimate <- function(lat, lon, elev=0, slope=0, aspect=180,
                             weather, temp=NULL, hum=NULL, 
                             habitat='herb'){
  
  if(is.null(temp) | is.null(hum)){
    micro <- micro_custom(lat=lat, lon=lon, elev=elev, weather=weather, th=F, habitat=habitat,
                          slope=slope, azmuth=aspect) # run the model in Fortran
  }
  else{
    weather_p <- prepare.weather(weather, temp = temp, hum = hum)
    micro <- micro_custom(lat=lat, lon=lon, elev=elev, weather=weather_p, th=T, habitat=habitat,
                          slope=slope, azmuth=aspect) # run the model in Fortran
  }
  return(micro)
}


# plot predictions
plot.predictions <- function(temp, micro=NULL, site='1A', obj='metout', var='TALOC', col='green', th=T){
  if(th==T){
    with(temp, plot(date, temperature, type='l', col=col))
    legend(mean(temp$date), max(temp$temperature, na.rm=T), c(site, 'predictions'), 
           lty=1, lwd=2, col=c(col, 'black'), bty = 'n')
    with(get('micro1')[[obj]], points(date, get(var), type='l'))
    with(get('micro2')[[obj]], points(date, get(var), type='l'))
    with(get('micro3')[[obj]], points(date, get(var), type='l'))
    with(get('micro4')[[obj]], points(date, get(var), type='l'))
  }
  else if(is.null(micro)){
    with(temp, plot(date, temperature, type='l', col=col))
    legend(mean(temp$date), max(temp$temperature, na.rm=T), c(site, 'predictions'), 
           lty=1, lwd=2, col=c(col, 'black'), bty = 'n')
    with(get('micro')[[obj]], points(date, get(var), type='l'))
  } else{
    with(temp, plot(date, temperature, type='l', col=col))
    legend(mean(temp$date), max(temp$temperature, na.rm=T), c(site, 'predictions'), 
           lty=1, lwd=2, col=c(col, 'black'), bty = 'n')
    with(micro[[obj]], points(date, get(var), type='l'))
  }
}

# plot humidity predictions #####
plot.predictions.h <- function(humid, micro=NULL, site='1A', obj='humid', var='RH0cm', col='green', th=T){
  if(th==T){
    
    with(humid, plot(date, rh/100, type='l', col=col))
    legend(mean(humid$date), max(humid$rh/100, na.rm=T), c(site, 'predictions'), 
           lty=1, lwd=2, col=c(col, 'black'), bty = 'n')
    with(get('micro')[[obj]], points(date, get(var), type='l'))
  }
}

# site.info
site.info <- function(dl_coords, n=1){
  air_loggers <- which(dl_coords$codes %in% c('a1','a2','a3'))
  
  assign('Latitude', dl_coords$latitude[-air_loggers][n], envir = parent.frame())
  assign('Longitude', dl_coords$longitude[-air_loggers][n], envir = parent.frame())
  assign('Elevation', dl_coords$elevation[-air_loggers][n], envir = parent.frame())
  assign('slope', dl_coords$slope[-air_loggers][n], envir = parent.frame())
  assign('aspect', dl_coords$aspect[-air_loggers][n], envir = parent.frame())
}

##calculate rmse

rmse <- function(pred, obs) {
  sqrt(mean((obs - pred) ^ 2))
}
