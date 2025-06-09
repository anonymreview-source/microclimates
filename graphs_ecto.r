environ <- as.data.frame(ecto$environ) # behaviour, Tb and environment
enbal <- as.data.frame(ecto$enbal) # heat balance outputs
masbal <- as.data.frame(ecto$masbal) # mass balance outputs
metout <- as.data.frame(micro$metout) # above ground microclimate
environ <- cbind(environ,metout$SOLR) # add solar radiation for activity window plots
colnames(environ)[ncol(environ)] <- "Solar"

# append dates
days <- rep(seq(1,12),24)
days <- days[order(days)]
dates <- rep(days, length.out = length(metout$TIME)) + metout$TIME/60/24 - 1
dates2 <- seq(1,12,1) # dates for daily output
metout <- cbind(dates,metout)
environ <- cbind(dates,environ)
masbal <- cbind(dates,masbal)
enbal <- cbind(dates,enbal)

with(environ, plot(TC ~ dates, ylab = "", xlab="month of year", col = 'black', xlim = c(-0.25, 12), ylim = c(-20, 40), type = "l", yaxt = 'n'))
with(environ, points(ACT * 2 + 7 ~ dates, type = "p", pch = 16, col = "orange"))
with(environ, points(SHADE / 10 - 6 ~ dates, type = "l", col = "dark green"))
with(environ, points(DEP - 10 ~ dates, type = "l", col = "brown"))
abline(ecto$T_F_min, 0, lty = 2, col = 'blue')
abline(environ$TC, 0, lty = 2, col = 'orange')
abline(ecto$T_F_max, 0, lty = 2, col = 'red')
ytick<-seq(15, 40, by=5)
axis(side=2, at=ytick, labels = TRUE)
mtext(text = c('A', 'B', 'I'), side = 2, line = 1, at = c(11, 9, 7))
ytick<-seq(-6, 4, by=2)
axis(side=2, at=ytick, labels = FALSE)
mtext(text = seq(0, 100, 20), side = 2, line = 1, at = seq(-6, 4, 2), las = 2)
ytick<-seq(-20, -10, by=2)
axis(side=2, at=ytick, labels = FALSE)
mtext(text = rev(seq(0, 100, 20)), side = 2, line = 1, at = seq(-20, -10, 2), las = 2)
abline(h = -10, lty = 2, col = 'grey')
mtext(text = c('body temperature (°C)', 'activity', 'shade (%)', 'depth (cm)'), side = 2, line = 2.5, at = c(30, 9, 0, -15))
text(0.1, c(ecto$T_F_max + 1, ecto$T_F_min + 1), c('T_F_max', 'T_F_min'), col = c('red', 'blue'), cex = 0.75)


### activity graphs 

forage <- subset(environ, ACT == 2) # get foraging hours
bask <- subset(environ, ACT == 1) # get basking hours
night <- subset(environ, Solar == 0) # get night hours
with(night, plot(TIME ~ DOY, ylab = "Hour of Day", xlab = "Day of Year", pch = 15, cex = 2, 
                 col = 'dark blue')) # nighttime hours
with(forage, points(TIME ~ DOY, pch = 15, cex = 2, col = 'orange')) # foraging Tbs
with(bask, points(TIME ~ DOY, pch = 15, cex = 2, col = 'light blue')) # basking Tbs




#### alternativa para visualizar 

library(dplyr)

# Agrupar por mes (dates) y calcular las medias
environ_means <- environ %>%
  group_by(dates) %>%
  summarise(
    mean_TC = median(TC, na.rm = TRUE),
    mean_ACT = median(ACT, na.rm = TRUE) * 2 + 7,
    mean_SHADE = median(SHADE, na.rm = TRUE) / 10 - 6,
    mean_DEP = median(DEP, na.rm = TRUE) - 10
  )

# Graficar las medias en lugar de las líneas individuales
plot(environ_means$dates, environ_means$mean_TC, ylab = "", xlab = "month of year",
     col = 'black', xlim = c(-0.25, 12), ylim = c(-20, 40), type = "l", yaxt = 'n', lwd = 2)
points(environ_means$dates, environ_means$mean_ACT, type = "p", pch = 16, col = "orange", cex = 0.6)
lines(environ_means$dates, environ_means$mean_SHADE, type = "l", col = "dark green", lwd = 2)
lines(environ_means$dates, environ_means$mean_DEP, type = "l", col = "brown", lwd = 2)

# Líneas de referencia
abline(ecto$T_F_min, 0, lty = 2, col = 'blue')
abline(ecto$T_F_max, 0, lty = 2, col = 'red')
abline(environ$TC, 0, lty = 2, col = "orange")

# Ejes y etiquetas
ytick <- seq(15, 40, by = 5)
axis(side = 2, at = ytick, labels = TRUE)
mtext(text = c('A', 'B', 'I'), side = 2, line = 1, at = c(11, 9, 7))
ytick <- seq(-6, 4, by = 2)
axis(side = 2, at = ytick, labels = FALSE)
mtext(text = seq(0, 100, 20), side = 2, line = 1, at = seq(-6, 4, 2), las = 2)
ytick <- seq(-20, -10, by = 2)
axis(side = 2, at = ytick, labels = FALSE)
mtext(text = rev(seq(0, 100, 20)), side = 2, line = 1, at = seq(-20, -10, 2), las = 2)
abline(h = -10, lty = 2, col = 'grey')
mtext(text = c('body temperature (°C)', 'activity', 'shade (%)', 'depth (cm)'), side = 2, line = 2.5, at = c(30, 9, 0, -15))
text(0.1, c(ecto$T_F_max + 1, ecto$T_F_min + 1), c('T_F_max', 'T_F_min'), col = c('red', 'blue'), cex = 0.75)





