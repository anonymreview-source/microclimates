library(tidyverse)
metout <- as.data.frame(micro1$metout)
temp2A$date <- as.POSIXct(round(temp2A$date, "hours"), tz = "UTC")
metout$date <- round(metout$date, "hours")
df <- merge(metout, temp2A, by = "date")

df <- left_join(metout, temp2A, by = "date")
df <- na.omit(df)

with(df, plot(date, TALOC, type = "l"))
with(df, points(date, temperature, type = "l", col = "red"))

rmse(df$TALOC, df$temperature)

