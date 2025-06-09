library(NicheMapR)
library(future)
library(future.apply)

# Define the file directory and list a sample of .rda files
file_dir <- "D:/CRP/PhD/Github/microclimates/Micro"
file_list <- list.files(file_dir, pattern = "\\.rda$", full.names = TRUE)
file_list <- sample(file_list, 5)  # Sample for testing

# Set up future for parallel execution
plan(multisession, workers = 4) # Adjust the number of workers as needed

# Function to execute the model and extract results for two species
run_model <- function(file_path) {
  # Load the file and extract `micro` object
  load(file_path)
  
  # Use filename as ID
  MicroID <- basename(file_path)
  
  # Run the ectotherm models for each species
  ecto.T <- ectotherm(
    Ww_g = 84.5, shape = 3, pct_wet = 0.01, T_F_min = 23.2, T_F_max = 34,
    T_pref = 30.3, minshades = micro$minshade, maxshades = micro$maxshade,
    T_RB_min = 15, T_B_min = 15, diurn = 1, shade_seek = 1, nocturn = 0, burrow = 1
  )
  
  ecto.L <- ectotherm(
    Ww_g = 21.958, shape = 3, pct_wet = 0.02, T_F_min = 25.4, T_F_max = 34,
    T_pref = 30.4, minshades = micro$minshade, maxshades = micro$maxshade,
    T_RB_min = 17, T_B_min = 17, diurn = 1, shade_seek = 1, nocturn = 0, burrow = 1
  )
  
  # Extract relevant data for each species
  environ.T <- data.frame(ecto.T$environ)
  masbal.T <- data.frame(ecto.T$masbal)
  
  environ.L <- data.frame(ecto.L$environ)
  masbal.L <- data.frame(ecto.L$masbal)
  
  # Calculations for Timon
  f_t.T <- sum(environ.T$ACT == 2) #forage time
  b_t.T <- sum(environ.T$ACT == 1) #basking time
  TA.T <- sum(environ.T$TA[environ.T$ACT != 0]) #environmental temperature at location
  wl.T <- sum(masbal.T$H2OCut_g[environ.T$ACT != 0]) # ewl during activity
  o2.T <- sum(masbal.T$O2_ml[environ.T$ACT != 0]) #oxygen consumption rate (ml/h) during activity
  tem_time.T <- sum(environ.T$TC >= 29.6 & environ.T$TC <= 31) # hours in Tpref
  hum_act.T <- min(environ.T$RELHUM[environ.T$ACT != 0]) # min relative humidity in activity
  shade.tmax.T <- max(environ.T$SHADE[environ.T$ACT != 0 & environ.T$DOY > 80 & environ.T$DOY <= 265]) # max shade in the warmest seasons (spring and summer)
  shade.tmen.T <- mean(environ.T$SHADE[environ.T$ACT != 0 & environ.T$DOY > 80 & environ.T$DOY <= 265]) # mean shade in the warmest seasons (spring and summer)
  
  # Calculations for Lacerta
  f_t.L <- sum(environ.L$ACT == 2) #forage time
  b_t.L <- sum(environ.L$ACT == 1) #basking time
  TA.L <- sum(environ.L$TA[environ.L$ACT != 0]) #environmental temperature at location
  wl.L <- sum(masbal.L$H2OCut_g[environ.L$ACT != 0]) # ewl during activity
  o2.L <- sum(masbal.L$O2_ml[environ.L$ACT != 0]) #oxygen consumption rate (ml/h) during activity
  tem_time.L <- sum(environ.L$TC >= 29.5 & environ.L$TC <= 31.3) # hours in Tpref
  hum_act.L <- min(environ.L$RELHUM[environ.L$ACT != 0]) # min relative humidity in activity
  shade.tmax.L <- max(environ.L$SHADE[environ.L$ACT != 0 & environ.L$DOY > 80 & environ.L$DOY <= 265]) # max shade in the warmest seasons (spring and summer)
  shade.tmen.L <- mean(environ.L$SHADE[environ.L$ACT != 0 & environ.L$DOY > 80 & environ.L$DOY <= 265]) # mean shade in the warmest seasons (spring and summer)
  
  # Combine results for each species into separate data frames
  df.ecto.Timon <- data.frame(
    Long = micro$longlat[1], Lat = micro$longlat[2], MicroID = MicroID, Species = "Timon",
    Forage_Time = f_t.T, Basking_Time = b_t.T, Water_Loss = wl.T,
    Oxygen_Consumption = o2.T, Temp_Time = tem_time.T,
    Humidity_Activity = hum_act.T, Shade_Tmax = shade.tmax.T, Shade_Tmean = shade.tmen.T
  )
  
  df.ecto.Lacerta <- data.frame(
    Long = micro$longlat[1], Lat = micro$longlat[2], MicroID = MicroID, Species = "Lacerta",
    Forage_Time = f_t.L, Basking_Time = b_t.L, Water_Loss = wl.L,
    Oxygen_Consumption = o2.L, Temp_Time = tem_time.L,
    Humidity_Activity = hum_act.L, Shade_Tmax = shade.tmax.L, Shade_Tmean = shade.tmen.L
  )
  
  # Return both data frames as a list
  list(df.ecto.Timon, df.ecto.Lacerta)
}

# Apply the function in parallel across all files
results <- future_lapply(file_list, run_model)

# Combine all results into a single data frame
final_df.ect <- do.call(rbind, unlist(results, recursive = FALSE))

# View the final combined data frame
View(final_df.ect)

