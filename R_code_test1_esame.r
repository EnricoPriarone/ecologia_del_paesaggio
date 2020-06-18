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
library(spatstat)
library(rgdal)
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
   
# Creo una colour palette che aiuti a ben rappresentare i valori
clg <- colorRampPalette(c('white','green','dark green'))(100)

# clgg <- colorRampPalette(c('black','grey','light grey'))(100)
# cl in scala di grigi

# Faccio un ritaglio per selezionare Italia, Spagna e Francia
extension <- c(-10, 20, 35, 52)
agric.gab <- crop(agric, extension)

# Ho provato a ridurre la risoluzione dei pixel, ma è totalmente inutile (già sono grandi di per sé...):
# agric.gab.hr <- aggregate(agric.gab, fact=0.5)
# plot(agric.gab.hr, main="Terre coltivate in Italia, Spagna e Francia", col=clg)

# Carico l'immagine Copernicus relativa al NDVI (Normalized Difference Vegetation Index)
# [Dare informazioni su immagine e pixel]
# NDVI = (NIR-RED)/(NIR+RED)
# [Dare ulteriori spiegazioni]
ndvi2000 <- raster("c_gls_NDVI_200007010000_GLOBE_VGT_V2.2.1.nc")
# plot(ndvi2000)

# Faccio una colour palette che sia adatta a rappresentare un'immagine di NDVI
cln <- colorRampPalette(c('light blue','light green','green'))(100)

# Faccio un ritaglio per selezionare Italia, Spagna e Francia
# extension <- c(-10, 20, 35, 52)
ndvi2000.gab <- crop(ndvi2000, extension)

# Graficizzo le due immagini ritagliate aggiungendo anche le linee di costa
coastlines <- readOGR("ne_10m_coastline.shp")
par(mfrow=c(1,2))
plot(agric.gab, main="Terre coltivate in Italia, Spagna e Francia (2000)", col=clg)
plot(coastlines, add=T)
plot(ndvi2000.gab, main="NDVI in Italia, Spagna e Francia (2000)", col=clg)
plot(coastlines, add=T)
# Ottengo un grafico che mi permette di notare una sovrapposizione tra i dati:
# [Mettere spiegazioni]

# Faccio plot con anche confini Paesi
# Necessarie librerie «spatstat» e «rgdal»



# Uso «pairs» per vedere se c'è correlazione
# [Fare!!!]





# Carico anche l'immagine LAI e quella Copernicus del 2015 per il confronto
lai2015 <- raster("c_gls_LAI300_201601310000_GLOBE_PROBAV_V1.0.1.nc")
#plot(lai2015)

ext <- c(11, 15, -4, -1)
lai2015.gab <- crop(lai2015, ext)
plot(lai2015.gab, col=clg)
# plot(coastlines, add=T) --> volendo!

ndvi2015 <- raster("c_gls_NDVI300_201507010000_GLOBE_PROBAV_V1.0.1.nc")
# plot(ndvi2015)
ndvi2015.gab <- crop(ndvi2015, ext)
plot(ndvi2015.gab, col=clg)
# plot(coastlines, add=T)



### Seconda parte

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
# ext <- c(11, 15, -4, -1)
ndvi1000.gab <- crop(ndvi1000, ext)
ndvi300.gab <- crop(ndvi300, ext)
# Richiamo immagini per vedere valori minimi e massimi per impostare «zlim» (scala ordinate)
# ndvi1000.gab: -0.007999995, 0.9360001
# ndvi300.gab: -0.08, 0.9360001
plot(ndvi1000.gab, main="NDVI in Gabon (2000, 2005, 2010)", col=clg, zlim=c(-0.1,1))
plot(ndvi300.gab, main="NDVI in Gabon (2015, 2020)", col=clg, zlim=c(-0.1,1))

# Inserisco immagine LAI
lai2015 <- raster("c_gls_LAI300_201601310000_GLOBE_PROBAV_V1.0.1.nc")
#plot(lai2015)

lai2015.gab <- crop(lai2015, ext)
plot(lai2015.gab, col=clg)

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



