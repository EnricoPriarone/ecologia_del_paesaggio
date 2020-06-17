### Codice esame
# Ho scaricato un'immagine NASA riferita ai terreni agricoli in formato .tif (raster) dal sito <https://search.earthdata.nasa.gov/search>
# Ho scaricato dati da sito Copernicus: uno relativo al LAI (2015) e cinque al NDVI (2000, 2005, 2010, 2015, 2020)
# Il primo obiettivo è comparare i dati Copernicus NDVI del 2000 con il raster NASA dello stesso anno e
# i dati Copernicus NDVI del 2015 con il LAI dello stesso anno.
# Questo per vedere quale correlazione c'è tra NDVI e distribuzione campi coltivati e tra NDVI e LAI.
# Il secondo obiettivo è fare un'analisi multitemporale dello stato della vegetazione in un'area compresa tra Spagna, Francia e Italia.
# In aggiunta potrei azzardare una previsione per il 2025.


#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

### Prima parte

# Richiamo le librerie necessarie
library(raster)
# library(ggplot2)
# library(igraph)
library(ncdf4)
# library(GGally)
# library(spatstat)
# library(rgdal)
# library(RStoolbox)
# library(gridExtra)
# library(sdm)

# Faccio il set della working directory
setwd("/Users/enricopriarone/lab/esame")

# Carico l'immagine NASA relativa a terreni agricoli
# «The downloaded grid values represent the proportion of pixel are that is either under cropland or pasture.
# The grid values range between 0 and 1.
# 0 means zero percent of pixel area under cropland or pasture present and 1 - 100% of the pixel area under cropland or pasture.» 
agric <- raster("cropland.tif")
# plot(agric)

# Creo una colour ramp palette che aiuti a ben rappresentare i valori
clg <- colorRampPalette(c('white','green','dark green'))(100)

# Faccio un ritaglio per selezionare Italia, Spagna e Francia
extension <- c(-10, 20, 35, 52)
agric.isf <- crop(agric, extension)

# Ho provato a ridurre la risoluzione dei pixel, ma è totalmente inutile: già sono grandi di per sé
# agric.isf.hr <- aggregate(agric.isf, fact=0.5)
# plot(agric.isf.hr, main="Terre coltivate in Italia, Spagna e Francia", col=clg)

# Carico l'immagine Copernicus relativa al NDVI (Normalized Difference Vegetation Index)
# [Dare informazioni su immagine e pixel]
# NDVI = (NIR-RED)/(NIR+RED)
# [Dare ulteriori spiegazioni]
ndvi_2000 <- raster("c_gls_NDVI_200007010000_GLOBE_VGT_V2.2.1.nc")
# plot(ndvi_2000)

# Faccio una colour ramp palette che sia adatta a rappresentare un'immagine di NDVI
cln <- colorRampPalette(c('light blue','light green','green'))(100)

# Faccio un ritaglio per selezionare Italia, Spagna e Francia
# extension <- c(-10, 20, 35, 52)
ndvi_2000.isf <- crop(ndvi_2000, extension)

# Graficizzo le due immagini ritagliate
par(mfrow=c(1,2))
plot(agric.isf, main="Terre coltivate in Italia, Spagna e Francia (2000)", col=clg)
plot(ndvi_2000.isf, main="NDVI in Italia, Spagna e Francia (2000)")
# Ottengo un grafico che mi permette di notare una sovrapposizione tra i dati:
# [Mettere spiegazioni]

# Carico le altre immagini Copernicus NDVI tutte insieme

# Creo una lista di pattern denominati «NDVI_»
rlist <- list.files(pattern="NDVI_")
rlist

# e una «NDVI300»
rlist2 <- list.files(pattern="NDVI300")
rlist2

# Uso funzioni «raster» e «lapply»
listafinale <- lapply(rlist, raster)
listafinale
listafinale2 <- lapply(rlist2, raster)
listafinale2

# Uso funzione «stack» per creare un pacchetto unico di dati: un'unica immagine
ndvi1000 <- stack(listafinale)
ndvi1000
ndvi300 <- stack(listafinale2)
ndvi300

# Graficizzo entrambi
par(mfrow=c(2,1))
plot(ndvi1000)
plot(ndvi300)

# Ritaglio le immagini
ext <- c(11, 15, -4, -1)
ndvi1000.isf <- crop(ndvi1000, ext)
ndvi300.isf <- crop(ndvi300, ext)
plot(ndvi1000.isf, main="NDVI in Italia, Spagna e Francia (2000, 2005, 2010)", col=clg)
plot(ndvi300.isf, main="NDVI in Italia, Spagna e Francia (2015, 2020)", col=clg)

# Inserisco immagine LAI
lai2015 <- raster("c_gls_LAI300_201601310000_GLOBE_PROBAV_V1.0.1.nc")
#plot(lai2015)

lai2015.isf <- crop(lai2015, ext)
plot(lai2015.isf, col=clg)

dev.off()



#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################



#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################



#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################


#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################



