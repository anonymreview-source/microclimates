

library(NicheMapR)
library(zoo)


Latitude <- 41.30 # the latitude in decimal degrees Modivas weather station
Longitude <- -8.69 # the longitude in decimal degrees Modivas weather station
Elevation <- 79 / 3.28084 # elevation, converted from feet to metres Modivas weather station
TZoffset <- 0 # the offset from Greenwich Mean Time, in hours
ystart <- 2022 # start yeaar
yfinish <- 2022 # end year
nyears <- yfinish - ystart + 1 # number of years to run

# load weather data
load(file='../weather/weatherCastro_hourly.RData')
weather <- weather_h


### Setting the model modes

# These parameters control how the model runs. We will run the model for both sun and shade, with the soil moisture and snow options on, and also note that the hourly parameter is also on so that it uses the hourly weather data as input

writecsv <- 0 # make Fortran code write output as csv files
runshade <- 1 # run the model twice, once for each shade level (1) or just for the first shade level (0)?
runmoist <- 1 # run soil moisture model (0 = no, 1 = yes)?
snowmodel <- 1 # run the snow model (0 = no, 1 = yes)? - note that this runs slower
hourly <- 1 # run the model with hourly input data
rainhourly <- 1 # run the model with hourly rainfall input data (irrelevant if hourly = 1)
microdaily <- 1 # run microclimate model where one iteration of each day occurs and last day gives initial conditions for present day
IR <- 0 # compute clear-sky longwave radiation using Campbell and Norman (1998) eq. 10.10 (includes humidity)
solonly <- 0 # Only run SOLRAD to get solar radiation? 1=yes, 0=no
message <- 0 # do not allow the Fortran integrator to output warnings
fail <- 24*365 # how many restarts of the integrator before the Fortran program quits (avoids endless loops when solutions can't be found)

### Setting the times and location info

# These parameters relate to the geographic location and the time period over which the model will run

longlat <- c(Longitude, Latitude) # decimal degrees longitude and latitude 
doynum <- floor(nrow(weather) / 24) # number of days to run, determined by counting the number of rows in the weather dataset and dividing by 24 to get days, but keeping it as a whole number
idayst <- 1 # start day
ida <- doynum # end day
HEMIS <- ifelse(longlat[2] < 0, 2, 1) # chose hemisphere based on latitude
ALAT <- abs(trunc(longlat[2])) # degrees latitude
AMINUT <- (abs(longlat[2]) - ALAT) * 60 # minutes latitude
ALONG <- abs(trunc(longlat[1])) # degrees longitude
ALMINT <- (abs(longlat[1]) - ALONG) * 60 # minutes latitude
ALREF <- ALONG # reference longitude for time zone

### Time-independent microclimate model parameters

# Now we set the non-temporal parameters including the depths we want to simulate

EC <- 0.0167238 # Eccenricity of the earth's orbit (current value 0.0167238, ranges between 0.0034 to 0.058)
RUF <- 0.004 # Roughness height (m), , e.g. sand is 0.0005, grass may be 0.02, current allowed range: 0.00001 (snow) - 0.02 cm.
ZH <- 0 # heat transfer roughness height (m) for Campbell and Norman air temperature/wind speed profile (invoked if greater than 1, 0.02 * canopy height in m if unknown)
D0 <- 0 # zero plane displacement correction factor (m) for Campbell and Norman air temperature/wind speed profile (0.6 * canopy height in m if unknown)
# Next four parameters are segmented velocity profiles due to bushes, rocks etc. on the surface
#IF NO EXPERIMENTAL WIND PROFILE DATA SET ALL THESE TO ZERO! (then roughness height is based on the parameter RUF)
Z01 <- 0 # Top (1st) segment roughness height(m)
Z02 <- 0 # 2nd segment roughness height(m)
ZH1 <- 0 # Top of (1st) segment, height above surface(m)
ZH2 <- 0 # 2nd segment, height above surface(m)
SLE <- 0.96 # Substrate longwave IR emissivity (decimal %), typically close to 1
ERR <- 1.5 # Integrator error for soil temperature calculations
Refhyt <- 2 # Reference height (m), reference height at which air temperature, wind speed and relative humidity input data are measured
DEP <- c(0, 2.5, 5, 10, 15, 20, 30, 50, 100, 200) # Soil nodes (cm) - keep spacing close near the surface, last value is where it is assumed that the soil temperature is at the annual mean air temperature
Thcond <- 2.5 # soil minerals thermal conductivity (W/mC)
Density <- 2.56 # soil minerals density (Mg/m3)
SpecHeat <- 870 # soil minerals specific heat (J/kg-K)
BulkDensity <- 1.3 # soil bulk density (Mg/m3)
SatWater <- 0.26 # volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
REFL <- 0.20 # soil reflectance (decimal %)
ALTT <- Elevation # altitude (m)
slope <- 0 # slope (degrees, range 0-90)
azmuth <- 180 # aspect (degrees, 0 = North, range 0-360)
hori <- rep(0, 24) # enter the horizon angles (degrees) so that they go from 0 degrees azimuth (north) clockwise in 15 degree intervals
VIEWF <- 1 - sum(sin(hori * pi / 180)) / length(hori) # convert horizon angles to radians and calc view factor(s)
lamb <- 0 # Return wavelength-specific solar radiation output?
IUV <- 0 # Use gamma function for scattered solar radiation? (computationally intensive)
PCTWET <- 0 # percentage of surface area acting as a free water surface (%)
CMH2O <- 1 # precipitable cm H2O in air column, 0.1 = VERY DRY; 1.0 = MOIST AIR CONDITIONS; 2.0 = HUMID, TROPICAL CONDITIONS (note this is for the whole atmospheric profile, not just near the ground)
TIMAXS <- c(1, 1, 0, 0)   # Time of Maximums for Air Wind RelHum Cloud (h), air & Wind max's relative to solar noon, humidity and cloud cover max's relative to sunrise
TIMINS <- c(0, 0, 1, 1)   # Time of Minimums for Air Wind RelHum Cloud (h), air & Wind min's relative to sunrise, humidity and cloud cover min's relative to solar noon
minshade <- 0 # minimum available shade (%)
maxshade <- 90 # maximum available shade (%)
Usrhyt <- 0.01# local height (m) at which air temperature, relative humidity and wind speed calculatinos will be made
# Aerosol profile using GADS
relhum <- 1
optdep.summer <- as.data.frame(rungads(longlat[2],longlat[1],relhum, 0))
optdep.winter <- as.data.frame(rungads(longlat[2],longlat[1],relhum, 1))
optdep <- cbind(optdep.winter[,1],rowMeans(cbind(optdep.summer[,2],optdep.winter[,2])))
optdep <- as.data.frame(optdep)
colnames(optdep)<-c("LAMBDA","OPTDEPTH")
a <- lm(OPTDEPTH~poly(LAMBDA, 6, raw = TRUE),data = optdep)
LAMBDA <- c(290, 295, 300, 305, 310, 315, 320, 330, 340, 350, 360, 370, 380, 390, 400, 420, 440, 460, 480, 500, 520, 540, 560, 580, 600, 620, 640, 660, 680, 700, 720, 740, 760, 780, 800, 820, 840, 860, 880, 900, 920, 940, 960, 980, 1000, 1020, 1080, 1100, 1120, 1140, 1160, 1180, 1200, 1220, 1240, 1260, 1280, 1300, 1320, 1380, 1400, 1420, 1440, 1460, 1480, 1500, 1540, 1580, 1600, 1620, 1640, 1660, 1700, 1720, 1780, 1800, 1860, 1900, 1950, 2000, 2020, 2050, 2100, 2120, 2150, 2200, 2260, 2300, 2320, 2350, 2380, 2400, 2420, 2450, 2490, 2500, 2600, 2700, 2800, 2900, 3000, 3100, 3200, 3300, 3400, 3500, 3600, 3700, 3800, 3900, 4000)
TAI <- predict(a, data.frame(LAMBDA))

### Time-dependent microclimate model variables

# Now we need to specify hourly air temperature, wind speed, relative humidity, 
# solar radiation, precipitation and cloud cover. 
# The first 5 come directly from the dataset, but this dataset doesn't include 
# cloud cover so we will have to estimate it. 
# Also, it is possible to supply precomputed zenith angles for when your simulation 
# doesn't start at midnight. But, if you don't need to supply them, you give 
# the model a vector of negative values.

# To start with, we need to use the ```na.approx``` function from the ```zoo``` package to interpolate NA values.


# use na.approx function from zoo package to fill in missing data
TAIRhr <- weather$Temperature <- na.approx(5/9 * (weather$Temperature - 32)) # convert ºF to ºC
RHhr <- weather$Humidity <- na.approx(weather$Humidity)
SOLRhr <- weather$Solar <- na.approx(weather$Solar)
RAINhr <- weather$Precip..Rate. <- na.approx(weather$Precip..Rate. * 25.4) # convert rainfall from inches to mm
WNhr <- weather$Speed <- na.approx(weather$Speed * 0.44704) # convert wind speed from miles/hour to m/s
ZENhr <- TAIRhr * 0 - 1 # negative zenith angles to force model to compute them
IRDhr <- TAIRhr * 0 - 1 # negative infrared values to force model to compute them

# Now we run the microclimate model using the ```micro_global``` function with the option ```clearsky``` set to 1 to obtain 365 days of clear sky solar radiation which we can then use in comparison to the observed solar radiation to infer cloud cover.

# run global microclimate model in clear sky mode to get clear sky radiation
micro <- micro_global(loc = c(Longitude, Latitude), timeinterval = 365, clearsky = 1)
# append dates
metout <- as.data.frame(micro$metout)
# subset metout to match dates with weather data
metout <- metout[metout$DOY %in% unique(as.POSIXlt(weather$date)$yday),]
clear <- as.data.frame(cbind(weather$date, metout[,13]),stringsAsFactors = FALSE)
doy <- rep(seq(1, 365),nyears)[1:floor(nrow(weather)/24)] # days of year to run
clear <- as.data.frame(clear, stringsAsFactors = FALSE)
colnames(clear)=c("datetime", "sol")
# find the maximum observed solar and adjust the clear sky prediction to this value 
maxsol <- max(SOLRhr)
clear2 <- clear[, 2]*(maxsol / max(clear[, 2])) # get ratio of max observed to predicted max clear sky solar
# compute cloud cover from ratio of max to observed solar
sol <- SOLRhr
clr <- clear2
zenthresh <- 85
sol[metout$ZEN > zenthresh] <- NA # remove values for nighttime (and very early morning/late afternoon)
clr[metout$ZEN > zenthresh] <- NA # remove values for nighttime (and very early morning/late afternoon)
a <- ((clr - sol) / clr) * 100 # get ratio of observed to predicted solar, convert to %
a[a > 100] <- 100 # cap max 100%
a[a < 0] <- 0 # cap min at 0%
a[is.na(a)] <- 0 # replace NA with zero
a[is.infinite(a)]=0 # replace infinity with zero
a[a == 0] <- NA # change all zeros to NA for na.approx
a <- na.approx(a, na.rm = FALSE) # apply na.approx, but leave any trailing NAs
a[is.na(a)] <- 0 # make trailing NAs zero
CLDhr <- a # now we have hourly cloud cover 

# We still need to give the model vectors of daily minimum and maximum weather data, even though they are not used when ```hourly``` is set to 1, so the next code chunk summarises the minima and maxima from the hourly data. If you set ```hourly``` to zero, you can see the difference having hourly data makes to the fit of the model to the observed data on soil temperature and soil moisture.

# aggregate hourly data to daily min / max
CCMAXX <- aggregate(CLDhr, by = list(weather$Date), FUN = max)[,2]#c(100, 100) # max cloud cover (%)
CCMINN <- aggregate(CLDhr, by = list(weather$Date), FUN = min)[,2]#c(0, 15.62) # min cloud cover (%)
TMAXX <- aggregate(TAIRhr, by = list(weather$Date), FUN = max)[,2]#c(40.1, 31.6) # maximum air temperatures (°C)
TMINN <- aggregate(TAIRhr, by = list(weather$Date), FUN = min)[,2]#c(19.01, 19.57) # minimum air temperatures (°C)
RAINFALL <- aggregate(RAINhr, by = list(weather$Date), FUN = sum)[,2]#c(19.01, 19.57) # minimum air temperatures (°C)
RHMAXX <- aggregate(RHhr, by = list(weather$Date), FUN = max)[,2]#c(90.16, 80.92) # max relative humidity (%)
RHMINN <- aggregate(RHhr, by = list(weather$Date), FUN = min)[,2]#c(11.05, 27.9) # min relative humidity (%)
WNMAXX <- aggregate(WNhr, by = list(weather$Date), FUN = max)[,2]#c(1.35, 2.0) # max wind speed (m/s)
WNMINN <- aggregate(WNhr, by = list(weather$Date), FUN = min)[,2]#c(0.485, 0.610) # min wind speed (m/s)

# Finally, we need the annual mean temperature and the running mean annual temperature for use as a boundary deep soil condition, as well as daily values of maximum and minimum shade, substrate emissivity and solar reflectance, and surface wetness, which we will keep constant across all days in this simulation. Also, for the deep soil boundary condtion, we only have one year of data so we cannot compute a running 365 day mean of the air temperature and instead we simply use a constant mean annual temperature.

tannul <- mean(c(TMAXX, TMINN)) # annual mean temperature for getting monthly deep soil temperature (°C)
tannulrun <- rep(tannul, doynum) # monthly deep soil temperature (2m) (°C)
# creating the arrays of environmental variables that are assumed not to change with month for this simulation
MAXSHADES <- rep(maxshade, doynum) # daily max shade (%)
MINSHADES <- rep(minshade, doynum) # daily min shade (%)
SLES <- rep(SLE, doynum) # set up vector of ground emissivities for each day
REFLS <- rep(REFL, doynum) # set up vector of soil reflectances for each day
PCTWET <- rep(PCTWET, doynum) # set up vector of soil wetness for each day

### Soil thermal properties

# Next we need to specify the soil properties. This code sets up for one soil type in terms of thermal properties, but below we will make the soil moisture-related properties transition at a certain depth.

# set up a profile of soil properites with depth for each day to be run
Numtyps <- 1 # number of soil types
Nodes <- matrix(data = 0, nrow = 10, ncol = doynum) # array of all possible soil nodes for max time span of 20 years
Nodes[1, 1:doynum]<-10 # deepest node for first substrate type
# now make the soil properties matrix
# columns are:
#1) bulk density (Mg/m3)
#2) volumetric water content at saturation (0.1 bar matric potential) (m3/m3)
#3) thermal conductivity (W/mK)
#4) specific heat capacity (J/kg-K)
#5) mineral density (Mg/m3)
soilprops <- matrix(data = 0, nrow = 10, ncol = 5) # create an empty soil properties matrix
soilprops[1, 1]<-BulkDensity # insert soil bulk density to profile 1
soilprops[1, 2]<-SatWater # insert saturated water content to profile 1
soilprops[1, 3]<-Thcond # insert thermal conductivity to profile 1
soilprops[1, 4]<-SpecHeat # insert specific heat to profile 1
soilprops[1, 5]<-Density # insert mineral density to profile 1
soilinit <- rep(tannul, 20) # make iniital soil temps equal to mean annual

### Soil moisture properties

# Now we specify the soil moisture-related parameteres using Table 9.1 out of Campbell and Norman's 1990 book 'Environmental Biophysics'. Note that there are 19 total nodes because an extra node has been inserted between each of the 10 depths specified in the ```DEP``` array to improve the accuracy of the soil moisture calculations. First all 19 nodes are given the values for soil type 3, a sandy loam, and then the deeper nodes (greater than 15 cm) are overwritten with soil type 5 which is a silt loam. Also specified is the root density profile, the leaf area index (both default values based on Campbell's 1985 book 'Soil Physics with Basic'), and some other parameters that control how the rainfall is applied together with the initial soil moisture values.

#use Campbell and Norman Table 9.1 soil moisture properties
soiltype <- 3 # 3 = sandy loam
PE <- rep(CampNormTbl9_1[soiltype, 4],19) #air entry potential J/kg
KS <- rep(CampNormTbl9_1[soiltype, 6],19) #saturated conductivity, kg s/m3
BB <- rep(CampNormTbl9_1[soiltype, 5],19) #soil 'b' parameter
BD <- rep(BulkDensity, 19) # soil bulk density, Mg/m3
DD <- rep(Density, 19) # soil mineral density, Mg/m3
soiltype <- 5 # change deeper nodes to 5 = a silt loam
PE[10:19]<-CampNormTbl9_1[soiltype, 4] #air entry potential J/kg
KS[10:19]<-CampNormTbl9_1[soiltype, 6] #saturated conductivity, kg s/m3
BB[10:19]<-CampNormTbl9_1[soiltype, 5] #soil 'b' parameter
L <- c(0, 0, 8.2, 8.0, 7.8, 7.4, 7.1, 6.4, 5.8, 4.8, 4.0, 1.8, 0.9, 0.6, 0.8, 0.4 ,0.4, 0, 0) * 10000 # root density at each node, mm/m3 (from Campell 1985 Soil Physics with Basic, p. 131)
R1 <- 0.001 #root radius, m}\cr\cr
RW <- 2.5e+10 #resistance per unit length of root, m3 kg-1 s-1
RL <- 2e+6 #resistance per unit length of leaf, m3 kg-1 s-1
PC <- -1500 #critical leaf water potential for stomatal closure, J kg-1
SP <- 10 #stability parameter for stomatal closure equation, -
IM <- 1e-06 #maximum allowable mass balance error, kg
MAXCOUNT <- 500 #maximum iterations for mass balance, -
LAI <- rep(0.1, doynum) # leaf area index, used to partition traspiration/evaporation from PET
rainmult <- 1 # rainfall multiplier to impose catchment
maxpool <- 10 # max depth for water pooling on the surface, mm (to account for runoff)
evenrain <- 0 # spread daily rainfall evenly across 24hrs (1) or one event at midnight (0)
SoilMoist_Init <- rep(0.2, 10) # initial soil water content for each node, m3/m3
moists <- matrix(nrow = 10, ncol = doynum, data = 0) # set up an empty vector for soil moisture values through time
moists[1:10,]<-SoilMoist_Init # insert inital soil moisture

### Snow model parameters

# We also need to specify the snow model parameters - these ones tend to work well in general and at the site being considered there is no snowfall so they will not be of consequence.

snowtemp <- 1.5 # temperature at which precipitation falls as snow (used for snow model)
snowdens <- 0.375 # snow density (Mg/m3)
densfun <- c(0.5979, 0.2178, 0.001, 0.0038) # slope and intercept of linear model of snow density as a function of day of year - if it is c(0, 0) then fixed density used
snowmelt <- 1 # proportion of calculated snowmelt that doesn't refreeze
undercatch <- 1 # undercatch multipier for converting rainfall to snow
rainmelt <- 0.0125 # parameter in equation from Anderson's SNOW-17 model that melts snow with rainfall as a function of air temp
snowcond <- 0 # effective snow thermal conductivity W/mC (if zero, uses inbuilt function of density)
intercept <- 0 # snow interception fraction for when there's shade (0-1)
grasshade <- 0 # if 1, means shade is removed when snow is present, because shade is cast by grass/low veg

### Tide parameters

# Finally, we need to give the model a vector of tide conditions although we are not running the model in intertidal mode so they also will be of no consequence.

# intertidal simulation input vector (col 1 = tide in(1)/out(0), col 2 = sea water temperature in °C, col 3 = % wet from wave splash)
tides <- matrix(data = 0, nrow = 24 * doynum, ncol = 3) # matrix for tides

### Running the model

# The data inputs are all ready now and they need to be sent to the Fortran program. The single-value inputs are collected in on long vector called ```microinput``` and then this, together with the longer inputs, are then sent put into a list called ```microin``` and passed to the function ```microclimate``` which runs the model. We will return the results to a list object called ```micro```.

# microclimate input parameters list
microinput <- c(doynum, RUF, ERR, Usrhyt, Refhyt, Numtyps, Z01, Z02, ZH1, ZH2, idayst, ida, HEMIS, ALAT, AMINUT, ALONG, ALMINT, ALREF, slope, azmuth, ALTT, CMH2O, microdaily, tannul, EC, VIEWF, snowtemp, snowdens, snowmelt, undercatch, rainmult, runshade, runmoist, maxpool, evenrain, snowmodel, rainmelt, writecsv, densfun, hourly, rainhourly, lamb, IUV, RW, PC, RL, SP, R1, IM, MAXCOUNT, IR, message, fail, snowcond, intercept, grasshade, solonly, ZH, D0)
# all microclimate data input list - all these variables are expected by the input argument of the fortran micro2014 subroutine
microin <- list(microinput = microinput, tides = tides, doy = doy, SLES = SLES, DEP = DEP, Nodes = Nodes, MAXSHADES = MAXSHADES, MINSHADES = MINSHADES, TIMAXS = TIMAXS, TIMINS = TIMINS, TMAXX = TMAXX, TMINN = TMINN, RHMAXX = RHMAXX, RHMINN = RHMINN, CCMAXX = CCMAXX, CCMINN = CCMINN, WNMAXX = WNMAXX, WNMINN = WNMINN, TAIRhr = TAIRhr, RHhr = RHhr, WNhr = WNhr, CLDhr = CLDhr, SOLRhr = SOLRhr, RAINhr = RAINhr, ZENhr = ZENhr, IRDhr = IRDhr, REFLS = REFLS, PCTWET = PCTWET, soilinit = soilinit, hori = hori, TAI = TAI, soilprops = soilprops, moists = moists, RAINFALL = RAINFALL, tannulrun = tannulrun, PE = PE, KS = KS, BB = BB, BD = BD, DD = DD, L = L, LAI = LAI)
micro <- microclimate(microin) # run the model in Fortran

### Retrieving the output and plotting the results against observed values

# Now the model has run and we need to retrieve the output from the ```micro``` list and add the date/time vector to them from the dataset.

# retrieve ouptut
dates <- weather$date[1:nrow(micro$metout)]
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
metout <- cbind(dates, metout)
shadmet <- cbind(dates, shadmet)
soil <- cbind(dates, soil)
shadsoil <- cbind(dates, shadsoil)
soilmoist <- cbind(dates, soilmoist)
shadmoist <- cbind(dates, shadmoist)
humid <- cbind(dates, humid)
shadhumid <- cbind(dates, shadhumid)
soilpot <- cbind(dates, soilpot)
shadpot <- cbind(dates, shadpot)



with(weather, plot(dates, Temperature, ylim=c(5,50), type='l', col='red'))
with(metout, points(dates, TALOC, type='l'))

with(soil, plot(dates, D0cm, type='l', col='green'))
with(soil, points(dates, D2.5cm, type='l', col='red'))
with(soil, points(dates, D5cm, type='l', col='black'))

with(soilmoist, plot(dates, WC0cm, type='l', col='green'))
with(soilmoist, points(dates, WC2.5cm, type='l', col='red'))
with(soilmoist, points(dates, WC5cm, type='l', col='black'))

