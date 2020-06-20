### Codice esame
# Ho scaricato dati Copernicus relativi al Soil Water Index, ossia l'indice di idratazione dei suoli (Albergel et Al. 2008 & Wagner et Al. 1999).
# «The Soil Water Index quantifies the moisture condition at various depths in the soil.
# «It is mainly driven by the precipitation via the process of infiltration.
# «Soil moisture is a very heterogeneous variable and varies on small scales with soil properties and drainage patterns.
# «Satellite measurements integrate over relative large-scale areas, with the presence of vegetation adding complexity to the interpretation.
# «The soil moisture, up to 5cm soil depth, is recognized as an Essential Climate Variable (ECV) by the Global Climate Observing System (GCOS).»

# Ho scaricato anche dati incendi in Africa occidentale del 7 novembre tra 2015 e 2019 da «https://firms.modaps.eosdis.nasa.gov/map/#t:adv;d:2017-11-07;l:viirs,modis_a,modis_t;@1.4,7.5,6z».
# Comparerò i dati riferiti al 7 novembre di cinque anni diversi (dal 2015 al 2019) di entrambi i set, per osservare i cambiamenti.
# Proverò a vedere se ci sono eventuali correlazioni tra gli incendi e il cambiamento di SWI.


#################################################################################################################################################
#################################################################################################################################################
#################################################################################################################################################

### Prima parte

# Richiamo le librerie necessarie
library(raster)
library(ggplot2)
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

# Per prima cosa carico le immagini Copernicus tutte insieme

'''
# Provo a creare una lista di .shp
dir <- "/Users/enricopriarone/lab/esame"
ff <- list.files(dir, pattern="\\.shp$", full.names=TRUE)

# first file
shapefile(ff[1])

# all of them into a list
x <- lapply(ff, shapefile)
'''


# Creo una lista di pattern denominati «.nc»
nclist <- list.files(pattern=".nc")
# nclist           # Così vedo l'elenco delle immagini incluse

# Uso funzioni «raster» e «lapply»
listafinale <- lapply(nclist, raster)
# listafinale      # Vedo i cinque RasterLayer

# Uso funzione «stack» per creare un pacchetto unico di dati: un'unica immagine
swi_12.5km <- stack(listafinale)
# swi_12.5km       # È di classe RasterStack

# Ritaglio le immagini
ext <- c(108, 120, -4, 8)
extension <- c(-18.3, 9.4, 2.5, 15.5) # Estesione della zona di studio
swi_12.5km_afr <- crop(swi_12.5km, extension)
swi_12.5km_afr     # Richiamando il set vedo che valori variano tra 0 e 99.5: li metto come limiti con «zlim»

# Graficizzo il set di immagini
cla <- colorRampPalette(c('brown','yellow','dark blue'))(400)
plot(swi_12.5km_afr, col=cla, main="Soil Water Index in Africa Ovest (2015-2019)", zlim=c(0,100))




# Importo uno .shp contenente i confini di Stato e lo ritaglio nella zona che mi serve
admin <- readOGR("ne_10m_admin_0_countries.shp")
admin.afr <- crop(admin, ext)


cll <- colorRampPalette(c('white', 'red', 'dark green', 'green', 'light green')) (400)
'''
# Provo a creare mappa RGB
plotRGB(swi_12.5km_afr, col=cll, zlim=c(0,100), stretch="Lin") # Non prende «main»
# plot(admin.afr, add=T)
'''




'''
# Le graficizzo insieme
par(mfrow=c(2,1))
plotRGB(swi_12.5km_ucr, r=4, g=3, b=2, zlim=c(0,100), stretch="Lin")
plot(dif_swi_1915, col=clgg)
'''


### Seconda parte

# Carico le immagini singolarmente perché con «lapply» non riesco a lavorare sulle singole immagini
swi2015r <- raster("c_gls_SWI_201511071200_GLOBE_ASCAT_V3.1.1.nc")
swi2016r <- raster("c_gls_SWI_201611071200_GLOBE_ASCAT_V3.1.1.nc")
swi2017r <- raster("c_gls_SWI_201711071200_GLOBE_ASCAT_V3.1.1.nc")
swi2018r <- raster("c_gls_SWI_201811071200_GLOBE_ASCAT_V3.1.1.nc")
swi2019r <- raster("c_gls_SWI_201911071200_GLOBE_ASCAT_V3.1.1.nc")


# Ritaglio tutte le immagini imponendo come estensione quella dell'  e faccio plot di verifica
# extension <- c(-18.3, 9.4, 2.5, 15.5)
swi2015r.afr <- crop(swi2015r, ext)
# plot(swi2015r.afr, col=cla)
swi2016r.afr <- crop(swi2016r, ext)
# plot(swi2016r.afr, col=cla)
swi2017r.afr <- crop(swi2017r, ext)
# plot(swi2017r.afr, col=cla)
swi2018r.afr <- crop(swi2018r, ext)
# plot(swi2018r.afr, col=cla)
swi2019r.afr <- crop(swi2019r, ext)
# plot(swi2019r.afr, col=cla)



# Cerco valori max e min delle immagini Copernicus del 2015 e del 2019
swi2015r.afr # 0 --> 98
swi2019r.afr # 0 --> 99.5

# Graficizzo le due immagini e via via faccio confronti tra i vari anni
# [clx <- colorRampPalette(c('red','yellow','light blue'))(100)]
par(mfrow=c(1,2))
plot(swi2015r.afr, zlim=c(0,100), col=cla)
plot(admin.afr, add=T)
plot(swi2019r.afr, zlim=c(0,100), col=cla)
plot(admin.afr, add=T)

'''
par(mfrow=c(1,2))
plot(swi2015r.afr, zlim=c(0,100), col=cla)
plot(swi2016r.afr, zlim=c(0,100), col=cla)

par(mfrow=c(1,2))
plot(swi2016r.afr, zlim=c(0,100), col=cla)
plot(swi2017r.afr, zlim=c(0,100), col=cla)

par(mfrow=c(1,2))
plot(swi2017r.afr, zlim=c(0,100), col=cla)
plot(swi2018r.afr, zlim=c(0,100), col=cla)

par(mfrow=c(1,2))
plot(swi2018r.afr, zlim=c(0,100), col=cla)
plot(swi2019r.afr, zlim=c(0,100), col=cla)
'''


# PROVA unsuperClass per i modelli  !!!!!!!


fir0107_2015r_1 <- read.table("fire_archive_M6_134109.csv", head=T)
fir0107_2015r_2 <- read.table("fire_archive_V1_134111.csv", head=T)
fir07_2015r_1 <- read.table("fire_archive_M6_134112.csv", head=T)
fir07_2015r_2 <- read.table("fire_archive_V1_134114.csv", head=T)

summary(fir0107_2015r_1)
# attach(fir0107_2015r_1)
# plot(latitude, longitude, las=1)
plot(fir0107_2015r_1$longitude, fir0107_2015r_1$latitude)
plot(admin.afr, add=T)

summary(fir07_2015r_1)
# attach(fir0107_2015r_1)
# plot(latitude, longitude, las=1)
plot(fir07_2015r_1$longitude, fir07_2015r_1$latitude)


# Carico incendi del 7 novembre delle annate da 2015 a 2019
# Li plotto insieme ai rispettivi ritagli
inc2015r <- readOGR("fire_archive_M6_134064.shp")
# plot(inc2015r)
inc2016r <- readOGR("fire_archive_M6_134067.shp")
# plot(inc2016r)
inc2017r <- readOGR("fire_archive_M6_134068.shp")
# plot(inc2017r)
inc2018r <- readOGR("fire_archive_M6_134069.shp")
# plot(inc2018r)
inc2019r <- readOGR("fire_archive_M6_134070.shp")
# plot(inc2019r)

par(mfrow=c(5,1))
plot(swi2015r.afr, zlim=c(0,100), col=cla) # «main» tolto perché non viene visualizzato
plot(inc2015r, add=T, pch=21, col="red")

plot(swi2016r.afr, zlim=c(0,100), col=cla)
plot(inc2016r, add=T, pch=21, col="red")

plot(swi2017r.afr, zlim=c(0,100), col=cla)
plot(inc2017r, add=T, pch=21, col="red")

plot(swi2018r.afr, zlim=c(0,100), col=cla)
plot(inc2018r, add=T, pch=21, col="red")

plot(swi2019r.afr, zlim=c(0,100), col=cla)
plot(inc2019r, add=T, pch=21, col="red")

# POTREI AGGIUNGERE UNA CARTA TOPOGRAFICA O SATELLITARE SOTTO

'''
plotRGB(swi_12.5km_afr, r=4, g=3, b=2, zlim=c(0,100), stretch="Lin", main="Soil Water Index in Ucraina (2015-2019)")
plot(dif_swi_1915, col=clgg)
'''



# Provo a vedere se differenza tra SWI e incendi corrisponde
dif_swi_1615 <- swi2016r.afr - swi2015r.afr
dif_swi_1615 # Range: -43, 24.5
# Se differenza dà risultato negativo significa che SWI è diminuito
# Se differenza dà risultato positivo significa che si è verificato un aumento

clr <- colorRampPalette(c('green', 'yellow', 'red')) (400)
par(mfrow=c(3,1))
plot(dif_swi_1615, col=clr)
plot(admin.afr, add=T)
plot(inc2015r, pch=21, col="blue")
plot(admin.afr, add=T)
plot(inc2016r, pch=21, col="red")
plot(admin.afr, add=T)

'''
dif_swi_1716 <- swi2017r.afr - swi2016r.afr
dif_swi_1716 # Range: -73, 24
par(mfrow=c(3,1))
plot(dif_swi_1716, col=clr)
plot(admin.afr, add=T)
plot(inc2016r, pch=21, col="blue")
plot(admin.afr, add=T)
plot(inc2017r, pch=21, col="red")
plot(admin.afr, add=T)

dif_swi_1817 <- swi2018r.afr - swi2017r.afr
dif_swi_1817 # Range: -25, 80.5
par(mfrow=c(3,1))
plot(dif_swi_1817, col=clr)
plot(admin.afr, add=T)
plot(inc2018r, pch=21, col="blue")
plot(admin.afr, add=T)
plot(inc2017r, pch=21, col="red")
plot(admin.afr, add=T)

dif_swi_1918 <- swi2019r.afr - swi2018r.afr
dif_swi_1918 # Range: -33, 39
par(mfrow=c(3,1))
plot(dif_swi_1918, col=clr)
plot(admin.afr, add=T)
plot(inc2019r, pch=21, col="blue")
plot(admin.afr, add=T)
plot(inc2018r, pch=21, col="red")
plot(admin.afr, add=T)
'''



# Faccio differenza tra immagine 2015 e immagine 2019 e la graficizzo in scala di grigi
dif_swi_1915 <- swi2019r.afr - swi2015r.afr
dif_swi_1915 # Range: -38, 47
clgg <- colorRampPalette(c('black','grey','red'))(100)
par(mfrow=c(3,1))
plot(swi2019r.afr, zlim=c(0,100))
plot(swi2015r.afr, zlim=c(0,100))
plot(dif_swi_1915, col=clgg)
# Dove è nero c'è stata una forte diminuzione, mentre dove è rosso un aumento


# PATCHES!!!!!!!!



# Provo a fare un grafico dei dati
ggplot(dif_swi_1915, aes(x=lon, y=lat, size=cases)) + geom_point()



dev.off()
