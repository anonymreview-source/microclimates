# Background

This repository contains all the code and data to simulate different
microclimates.

We simulate different microclimates in Castro São Paio using NicheMapR.
The weather data comes from Modivas
(<https://www.wunderground.com/dashboard/pws/IMODIV1>).

## Repository structure

  - **/code/**
      - **aux\_functions.R**: helpful functions to handle data
          - `readLoggers`: reads and cleans datalogger data given the
            logger name and timeperiod.
          - `read.logger`: reads the data from iButton csv files.
            Internal of `readLoggers`.
          - `clean.logger`: cleans datalogger data, and formats dates.
            Internal of `readLoggers`.
          - `clean.logger2`: same as `clean.logger` but for a different
            time format. Internal of `readLoggers`.
          - `prepare.weather`: adds logger data to the weather station
            data. Internal of `run.microclimate`.
          - `retrieve.output`: retrieves output of the microclimate
            model. Internal of `run.microclimate`.
          - `run.microclimate`: runs the model, if needed it prepares
            the weather data, and finally retrieves the data from the
            model.
          - `plot.predictions`: plot simulation predictions and
            datalogger observations.
          - `site.info`: get coordinates, elevation, slope and aspect
            for each site from the datalogger table.
      - **micro\_custom.R**: Function to prepare the input for NicheMapR
        microclimate model and run the model.
      - **microclimate\_hourly\_Castro.R**: example script that shows
        how to use hourly data from weather stations as input for the
        NicheMapR microclimate model.
      - **prepare\_rasters.R**: script used to compute the *slope* and
        *aspect* in the study area, and save the files in the *terrain*
        folder.
      - **prepare\_weather.R**: script used to download and prepare the
        weather data from Modivas station, which is then saved in the
        *weather* folder. The script can download data from any station,
        if you supply its code in wunderground.
      - **read\_dataLoggers.R**: example script to read and plot
        datalogger data (Old).
  - **/dataloggers\_Castro/** datalogger data on temperature and
    humidity.
  - **/terrain/**
      - **marrisk-2019-02-20-dsm.tif**: digital elevation model of
        Castro Saõ Paio.
      - **marrisk-2019-02-20-dsmshd.tif**: digital elevation model
        (shade).
      - **marrisk-2019-02-20-orto.tif**: ortophoto.
      - **slope.tif**: slope for the area where dataloggers where
        placed.
      - **aspect.tif**: aspect for the area where dataloggers where
        placed.
      
  - **/weather/** weather data from Modiva station, downloaded from
    wunderground.
  - **microclimates\_CastroSampaio.Rmd**: Run simulations for different
    microhabitats and compare to the data recorded by iButtons placed in
    those microclimates.

