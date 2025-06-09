

rm(list=ls())
setwd("~/Dropbox/2_supervisions/Carolina Reyes-Puig/microclimates/microclimates/dataloggers_Castro")


library(terra)

# # mesocosmos
# 
# meso.files <- list.files(path = '/Volumes/urdintxu/timon_microclimates/mesocosms', pattern = '*.tif', full.names = T)
# 
# for(i in meso.files){
#   r <- rast(i)
#   lname <- strsplit(strsplit(i, split = "/")[[1]][6], split = '\\.')[[1]][1]
#   assign(lname, r)
# }
# 
# plot(mesocosmos2_dem)
# plotRGB(mesocosmos2_orto1)
# points(cbind(-8.589838,41.10731), pch=20, cex=2, col='red') # datalogger


# Castro Sanpaio

castro.files <- list.files(path = '../terrain', 
                           pattern = '*.tif', full.names = T)[-c(1,2,6:8)]

for(i in castro.files){
  r <- rast(i)
  r <-  project(r, "+proj=longlat +datum=WGS84")
  lname <- strsplit(strsplit(i, split = "/")[[1]][3], split = '\\.')[[1]][1]
  assign(lname, r)
}

plot(`marrisk-2019-02-20-dsm`)
plot(`marrisk-2019-02-20-dsmshd`)
plot(`marrisk-2019-02-20-orto`)
# the weather station in Modivas is way out of the drone flight
# points(cbind(-8.69, 41.30), pch=20, col='red') 

# read dataloggers info
dl_coords <- read.csv(file = '../dataloggers_Castro/ibutton_coords.csv', dec=',', sep=';')
points(dl_coords[,c("longitude", "latitude")], cex=2, pch=20, col='red')

summary(dl_coords[,c("longitude", "latitude")])
# longitude         latitude    
# Min.   :-8.730   Min.   :41.28  
# 1st Qu.:-8.730   1st Qu.:41.28  
# Median :-8.729   Median :41.28  
# Mean   :-8.729   Mean   :41.28  
# 3rd Qu.:-8.729   3rd Qu.:41.28  
# Max.   :-8.728   Max.   :41.28  

dsm_castro_c <- crop(`marrisk-2019-02-20-dsm`, ext(c(-8.732, -8.727, 41.279, 41.282)))
orto_c <- crop(`marrisk-2019-02-20-orto`, ext(c(-8.732, -8.727, 41.279, 41.282)))

plot(orto_c)
points(dl_coords[,c("longitude", "latitude")], cex=2, pch=20, col='red')

plot(dsm_castro_c)
points(dl_coords[,c("longitude", "latitude")], cex=2, pch=20, col='red')


slope_c <- terrain(dsm_castro_c, v="slope", neighbors=8, unit="degrees") 
aspect_c <- terrain(dsm_castro_c, v="aspect", neighbors=8, unit="degrees") 
roughness_c <- terrain(dsm_castro_c, v="roughness", neighbors=8, unit="degrees") 

par(mfrow=c(1,3))
plot(slope_c, main='slope')
points(dl_coords[,c("longitude", "latitude")], pch=21)

plot(aspect_c, main='aspect')
points(dl_coords[,c("longitude", "latitude")], pch=21)

plot(roughness_c, main='roughness')
points(dl_coords[,c("longitude", "latitude")], pch=21)


#writeRaster(dsm_castro_c, filename = '../terrain/dsm.tif')
#writeRaster(orto_c, filename = '../terrain/orto.tif')
#writeRaster(slope_c, filename = '../terrain/slope.tif')
#writeRaster(aspect_c, filename = '../terrain/aspect.tif')

