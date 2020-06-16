# Provo a caricare immagine NASA del 2000 su agricoltura e vedere se è utile

library(raster)
library(ggplot2)
library(igraph)
library(ncdf4)

setwd("/Users/enricopriarone/lab/croplands_2000")

# Carico l'immagine
agric <- raster("cropland.tif")
plot(agric)

clg <- colorRampPalette(c('yellow','green','dark green'))(100)

# Faccio un ritaglio per selezionare Italia, Spagna e Francia
extension <- c(-10, 20, 35, 52)
agric.isf <- crop(agric, extension)
plot(agric.isf, main="Terre coltivate in Italia, Spagna e Francia", col=clg)

agric.isf.hr <- aggregate(agric.isf, fact=0.5)
plot(agric.isf.hr, main="Terre coltivate in Italia, Spagna e Francia", col=clg)


setwd("/Users/enricopriarone/lab/ndvi")

ndvi_1998 <- raster("c_gls_NDVI_199806110000.nc")
ndvi_2020 <- raster("c_gls_NDVI_202006010000.nc")
plot(ndvi_1998)
plot(ndvi_2020)

clg <- colorRampPalette(c('yellow','green','dark green'))(100)

# Faccio un ritaglio per selezionare Italia, Spagna e Francia
extension <- c(7, 13, 44, 46)
ndvi_2020.po <- crop(ndvi_2020, extension)
plot(ndvi_2020.po, main="Indice NDVI in Pianura Padana", col=clg)

