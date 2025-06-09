

setwd("~/Dropbox/2_supervisions/Carolina Reyes-Puig/microclimates/microclimates/dataloggers_Castro")

library(chron)


rm(list=ls())

# define function to read logger data
read.logger <- function(period, logger, T.ext=F){
  path <- paste0(getwd(), period, logger)
  if(T.ext == T){
    logger.file <- list.files(path, pattern='*T.csv', full.names=T)
  }else{
    logger.file <- list.files(path, pattern='*.csv', full.names=T)
  }
  read.csv(logger.file, header=F, dec=',', skip=20)
}

# function to convert dates and fix temperature / humidity data
clean.logger <- function(data){
  dtparts <- t(as.data.frame(strsplit(data[,1],' ')))
  row.names(dtparts) <- NULL
  times <- chron(dates = dtparts[,1], times = dtparts[,2], 
                 format = c('d/m/y','h:m:s'))
  
  data2 <- as.data.frame(times)
  data2$temperature <- as.double(paste(data$V3, data$V4, sep='.'))
  return(data2)
}


loggers <- c("1A", "1B", "1C", "1D")

periods <- c('/May_July/', '/July_November/')

period <- periods[1]

for(i in 1:length(loggers)){
  if(grepl("A", loggers[i])){
    dt.sun <- read.logger(period, loggers[i], T.ext = T)
    dt.sun <- clean.logger(dt.sun)
  }
  if(grepl("B", loggers[i])){
    dt.shadow <- read.logger(period, loggers[i], T.ext = T)
    dt.shadow <- clean.logger(dt.shadow)
  }
  if(grepl("C", loggers[i])){
    dt.50 <- read.logger(period, loggers[i], T.ext = F)
    dt.50 <- clean.logger(dt.50)
  }
  if(grepl("D", loggers[i])){
    dt.buried <- read.logger(period, loggers[i], T.ext = F)
    dt.buried <- clean.logger(dt.buried)
  }
}


with(dt.sun, plot(times, temperature, type='l', col='purple'))
with(dt.50, points(times, temperature, type='l', col='gold'))
with(dt.shadow, points(times, temperature, type='l', col='brown'))
with(dt.buried, points(times, temperature, type='l', col='black'))




