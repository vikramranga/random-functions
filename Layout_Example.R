## This script will create graphs for paper on badland mapping.


## Make a layout first

m <- matrix(c(1, 1, 1, 0, 0, 0, 2, 3, 4), ncol = 3, byrow = TRUE)
layout(m, heights = c(1.8, 0.4, 1.8, 1.8, 0.4, 1.8, 1.8, 0.4, 1.8), widths = c(1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3, 1.3))

#show.layout(n = 4) # you may check how it looks :-p

# Define outer and inner margins
par(oma = c(3, 3, 2, 1))
par(mar = c(0.5, 3, 1, 1))



# All the libraries
#source("C:/Users/Vikram1/Documents/libraries.R")
#library(plotrix)


#
##
### Plot the rainfall and MODIS NDVI values.
##
#


modis.bad <- read.csv("E:/MODISNDVI/MODIS 2010/NDVI_bad_2010.csv", header = TRUE)
modis.agr <- read.csv("E:/MODISNDVI/MODIS 2010/NDVI_agr_2010.csv", header = TRUE)
modis.level <- read.csv("E:/MODISNDVI/MODIS 2010/NDVI_level_2010.csv", header = TRUE)

# Modis has scale factor of 0.0001 so divide values by 10000

modis.bad <- apply(modis.bad, 2, function(x) x <- x/10000)
modis.agr <- apply(modis.agr, 2, function(x) x <- x/10000)
modis.level <- apply(modis.level, 2, function(x) x <- x/10000)

plot(apply(modis.bad, 2, mean), type = "b", axes = FALSE, pch = 17, cex = 1.5, ylim = c(0.1, 0.8))
lines(apply(modis.agr, 2, mean), pch = 16, cex = 1.5, type = "b")
lines(apply(modis.level, 2, mean), pch = 1, cex = 1.5, type = "b")

Months <- colnames(modis.bad)

axis(1, at = seq(1, 12), labels = Months, xlab = "Months")
axis(2, ylab = "NDVI Values")

# Here comes the rainfall data
#rainfall_t<-read.table("J:/Phd/rainfall data from IMD/month.mean.csv",sep=",",header=TRUE)
#lines()

legend("topright", legend = c("Badlands", "Cropland", "Levelled Land"), 
       lty = 1, # line type 1 is for solid lines.
       pch = c(17, 16, 1), # pch for legends
       pt.cex = 1.5, # point size
       pt.lwd = 1, # width of line 
       bty = "n", # box around the legends. Here it is none
       y.intersp = 0.35,
       x.intersp = 0.3) # vertical distances between legends.


grid()

# January
jan.ref <- read.csv("E:/landsat/New Landsat/mapping/Landsat January/spectral.reflectance_January.csv", header = TRUE)
#par(mfrow = c(1, 3))


par(cex = 0.8)
plot(jan.ref$bands_le_jan, type = 'b', col = 'black', pch = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.05, 0.35), cex = 1.5)
lines(jan.ref$bands_bad_jan, type = 'b', col = 'black', pch = 17, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)
lines(jan.ref$bands_ag_jan, type = 'b', col = 'black', pch = 16, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)
box()
axis(1, at = c(1, 2, 3, 4, 5, 6),labels = c(1, 2, 3, 4, 5, 7), xlab = "Landsat Band")

# break axis

axis.break(1, 5.5, style = 'slash')
axis(2, xlim = c(0.05, 0.45), ylab = "Reflectance")
grid()

# This is trick to bring the forward the points and line before grid().
lines(jan.ref$bands_le_jan, type = 'b', col = 'black', pch = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.05, 0.30), cex = 1.5)
lines(jan.ref$bands_bad_jan, type = 'b', col = 'black', pch = 17, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)
lines(jan.ref$bands_ag_jan, type = 'b', col = 'black', pch = 16, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)


legend("topleft", legend = c("Badlands", "Cropland", "Levelled Land"), 
       lty = 1, # line type 1 is for solid lines.
       pch = c(17, 16, 1), # pch for legends
       pt.cex = 1.5, # point size
       pt.lwd = 1, # width of line 
       bty = "n", # box around the legends. Here it is none
       y.intersp = 0.5,
       x.intersp = 0.3) # vertical distances between legends.


# April

april.ref <- read.csv("E:/landsat/recent/2010-04-03/spectral.reflectance_2010-04-03_April.csv", header = TRUE)

plot(april.ref$bands_le_apr, type = 'o', col = 'black', pch = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.07, 0.30), cex = 1.5)
lines(april.ref$bands_bad_apr, type = 'o', col = 'black', pch = 17, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)
lines(april.ref$bands_ag_apr, type = 'o', col = 'black', pch = 16, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)
box()
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = c(1, 2, 3, 4, 5, 7), xlab = "Landsat Band")
# break axis
axis.break(1, 5.5, style = 'slash')
axis(2, xlim = c(0.05, 0.45), ylab = "Reflectance")
grid()

lines(april.ref$bands_le_apr, type = 'o', col = 'black', pch = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.07, 0.30), cex = 1.5)
lines(april.ref$bands_bad_apr, type = 'o', col = 'black', pch = 17, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)
lines(april.ref$bands_ag_apr, type = 'o', col = 'black', pch = 16, axes = FALSE, add = TRUE, xlab = "", ylab = "", cex = 1.5)

legend("topleft", legend = c("Badlands", "Cropland", "Levelled Land"), 
       lty = 1, # line type 1 is for solid lines.
       pch = c(17, 16, 1), # pch for legends
       pt.cex = 1.5, # point size
       pt.lwd = 1, # width of line 
       bty = "n", # box around the legends. Here it is none
       y.intersp = 0.5, 
       x.intersp = 0.3) # vertical distances between legends.

# October

oct.ref <- read.csv("E:/landsat/New Landsat/mapping/Landsat October/spectral.reflectance_October.csv", header = TRUE)


plot(oct.ref$bands_le_oct, type = 'b', col = 'black', pch = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.05, 0.30), cex = 1.5)
lines(oct.ref$bands_bad_oct, type = 'b', col = 'black', pch = 17, axes = FALSE, xlab = "", ylab = "", cex = 1.5)
lines(oct.ref$bands_ag_oct, type = 'b', col = 'black', pch = 16, axes = FALSE, xlab = "", ylab = "", cex = 1.5)
box()
axis(1, at = c(1, 2, 3, 4, 5, 6), labels = c(1, 2, 3, 4, 5, 7), xlab = "Landsat Band")
# break axis

axis.break(1, 5.5, style = 'slash')
axis(2, xlim = c(0.05, 0.45), ylab = "Reflectance")
grid()

lines(oct.ref$bands_le_oct, type = 'b', col = 'black', pch = 1, axes = FALSE, xlab = "", ylab = "", ylim = c(0.05, 0.30), cex = 1.5)
lines(oct.ref$bands_bad_oct, type = 'b', col = 'black', pch = 17, axes = FALSE, xlab = "", ylab = "", cex = 1.5)
lines(oct.ref$bands_ag_oct, type = 'b', col = 'black', pch = 16, axes = FALSE, xlab = "", ylab = "", cex = 1.5)

legend("topleft", legend = c("Badlands", "Cropland", "Levelled Land"), 
       lty = 1, # line type 1 is for solid lines.
       pch = c(17, 16, 1), # pch for legends
       pt.cex = 1.5, # point size
       pt.lwd = 1, # width of line 
       bty = "n", # box around the legends. Here it is none
       y.intersp = 0.5,
       x.intersp = 0.3) # vertical distances between legends.

mtext("Landsat Bands", side = 1, outer = TRUE, line = 1.5)
