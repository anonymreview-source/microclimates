rm(list = ls())
library(terra)
library(NicheMapR)
library(zoo)
library(raster)
#install.packages("stringr")
library(stringr)
library(parallel)
source('./code/micro_custom.R')
source('./code/aux_functions.R')
load('./weather/weatherCastro_hourly.RData')

##Call raster file with Castro S. Paio classification
class.castro <- rast("terrain/rasterfinal_remuestreado.tif")
#plot(class.castro, col = c("chartreuse3", "deeppink3", "gray46", "sienna"))
#res(class.castro)
class.castro_wgs84 <- aggregate(class.castro, fact = 2, fun = modal) # 1 meter resolution
#plot(class.castro_wgs84)
#res(class.castro_wgs84)

class.castro_values <- class.castro_wgs84[] 

class.castro_values <- as.character(class.castro_values)  
class.castro_values[class.castro_values == "0"] <- "VH"
class.castro_values[class.castro_values == "1"] <- "CARP"
class.castro_values[class.castro_values == "2"] <- "GRO/ROCK"
class.castro_values[class.castro_values == "3"] <- "PASA"

class.castro_wgs84[] <- class.castro_values

#plot(class.castro_wgs84)

## Converting raster to points to extract coordinates
points_df <- as.data.frame(class.castro_wgs84, xy = TRUE)

## Plotting some random coordindates to check 
#r.points <- points_df[sample(nrow(points_df), 5), ]
#plot(class.castro_wgs84, col = c("chartreuse3", "deeppink3", "gray46", "sienna"))
#points(r.points$x, r.points$y, col = "red", pch = 16, cex = 0.6)

###Prepare elev, slope, aspect
slope <- rast("terrain/slope_pt.tif")
#plot(slope)  
aspect <- rast("terrain/aspect_pt.tif")
elev <- rast("terrain/elev_pt.tif")

elev_resampled <- resample(elev, class.castro_wgs84, method = "bilinear")
aspect_resampled <- resample(aspect, class.castro_wgs84, method = "bilinear")
slope_resampled <- resample(slope, class.castro_wgs84, method = "bilinear")

elev_resampled <- mask(elev_resampled, class.castro_wgs84)
aspect_resampled <- mask(aspect_resampled, class.castro_wgs84)
slope_resampled <- mask(slope_resampled, class.castro_wgs84)

#writeRaster(elev_resampled, "elev_resampled.tif") ## make sure to bring these on Github repository because the previous are too large so, they are only work on my local pc
#writeRaster(aspect_resampled, "aspect_resampled.tif")
#writeRaster(slope_resampled, "slope_resampled.tif")

#mean_elev <- mean(na.omit(values(elev_resampled)), na.rm = TRUE)
#mean_slope <- mean(na.omit(values(slope_resampled)), na.rm = TRUE)
#mean_aspect <- mean(na.omit(values(aspect_resampled)), na.rm = TRUE)

points_df$elev <- extract(elev_resampled, cbind(points_df$x, points_df$y))$dem_srtm_pt_1sec
points_df$slope <- extract(slope_resampled, cbind(points_df$x, points_df$y))$slope
points_df$aspect <- extract(aspect_resampled, cbind(points_df$x, points_df$y))$aspect
weather <- weather_h

## Get global climate 
#get.global.climate()

## starting time 
#start_time <- Sys.time()

library(future)
library(future.apply)

# Filtrar points_subset para incluir solo el rango especificado [6614:9999]
points_subset <- points_df[40000:46726, ] # run by batches 

# Apply the function in parallel, skipping files that are already saved
library(future)
library(future.apply)
library(NicheMapR)
library(terra)
library(stringr)
library(zoo)

# Set up parallelization plan
plan(multisession, workers = parallel::detectCores() - 1)

# Filter points_subset to include only the specified range [40000:46726]
points_subset <- points_df[40000:46726, ]

# Create a list of indices for which the file does not exist
missing_indices <- which(!file.exists(
  paste0("D:/CRP/PhD/Github/microclimates/Micro_P/microCS", 
         str_pad(40000:46726, 5, pad = "0"), '.rda'))
)

# Function to run the model and save the file
run_and_save <- function(i) {
  
  # Define the file name where the object will be saved
  file_path <- paste0("D:/CRP/PhD/Github/microclimates/Micro_P/microCS", 
                      str_pad(i + 39999, 5, pad = "0"), '.rda')
  
  # Extract data for the specific point
  lat <- points_subset$y[i]
  long <- points_subset$x[i]
  habitat <- as.character(points_subset$Microh[i])
  elev <- points_subset$elev[i]
  slope <- points_subset$slope[i]
  aspect <- points_subset$aspect[i]
  
  # Run the microclimate model
  micro <- run.microclimate(lat = lat, lon = long, elev = elev, slope = slope, 
                            aspect = aspect, weather = weather, habitat = habitat)
  
  # Save the file
  save(micro, file = file_path)
}

# Run the function in parallel only for the indices of missing files
future_lapply(missing_indices, run_and_save)

### test if match 

l <- load("Micro_P/microCS38929.rda")
micro$longlat
points_df[38929,] 

longlat <- c(points_subset$x[i], lat = points_df$y[i])
micro <- micro_global(longlat)
ecto <- ectotherm() 
  
j <- load("Micro/microCS06499.rda")
micro$longlat
points_df[6499,]

