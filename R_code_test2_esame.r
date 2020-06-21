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
library(igraph)
library(ncdf4)
library(spatstat)
library(rgdal)
library(RStoolbox)


# Faccio il set della working directory
setwd("/Users/enricopriarone/lab/esame")

# Per prima cosa carico le immagini Copernicus tutte insieme

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
ext <- c(108, 120, -5, 8) # Estesione della zona di studio: Borneo
swi_12.5km_bor <- crop(swi_12.5km, ext)
swi_12.5km_bor     # Richiamando il set vedo che valori variano tra 0 e 99.5: li metto come limiti con «zlim»

# Graficizzo il set di immagini
cla <- colorRampPalette(c('brown','yellow','dark blue'))(400)
plot(swi_12.5km_bor, col=cla, main="Soil Water Index in Borneo (2015-2019)", zlim=c(0,100), las=1)



# Importo uno .shp contenente i confini di Stato e lo ritaglio nella zona che mi serve
admin <- readOGR("ne_10m_admin_0_countries.shp")
admin.bor <- crop(admin, ext)


cll <- colorRampPalette(c('white', 'red', 'dark green', 'green', 'light green')) (400)
'''
# Provo a creare mappa RGB
plotRGB(swi_12.5km_afr, col=cll, zlim=c(0,100), stretch="Lin") # Non prende «main»
# plot(admin.bor, add=T)

# Le graficizzo insieme
par(mfrow=c(2,1))
plotRGB(swi_12.5km_bor, r=4, g=3, b=2, zlim=c(0,100), stretch="Lin")
plot(dif_swi_1915, col=clgg)
'''

dev.off()

### Seconda parte

# Carico le immagini singolarmente perché con «lapply» non riesco a lavorare sulle singole immagini
swi2015r <- raster("c_gls_SWI_201511071200_GLOBE_ASCAT_V3.1.1.nc")
swi2016r <- raster("c_gls_SWI_201611071200_GLOBE_ASCAT_V3.1.1.nc")
swi2017r <- raster("c_gls_SWI_201711071200_GLOBE_ASCAT_V3.1.1.nc")
swi2018r <- raster("c_gls_SWI_201811071200_GLOBE_ASCAT_V3.1.1.nc")
swi2019r <- raster("c_gls_SWI_201911071200_GLOBE_ASCAT_V3.1.1.nc")


# Ritaglio tutte le immagini imponendo come estensione quella dell'  e faccio plot di verifica
# extension <- c(-18.3, 9.4, 2.5, 15.5)
# cla <- colorRampPalette(c('brown','yellow','dark blue'))(400)
swi2015r.bor <- crop(swi2015r, ext)
# plot(swi2015r.bor, col=cla, las=1, main="SWI Borneo 2015")
# plot(admin.bor, add=T)
swi2016r.bor <- crop(swi2016r, ext)
# plot(swi2016r.bor, col=cla, las=1, main="SWI Borneo 2016")
# plot(admin.bor, add=T)
swi2017r.bor <- crop(swi2017r, ext)
# plot(swi2017r.bor, col=cla, las=1, main="SWI Borneo 2017")
# plot(admin.bor, add=T)
swi2018r.bor <- crop(swi2018r, ext)
# plot(swi2018r.bor, col=cla, las=1, main="SWI Borneo 2018")
# plot(admin.bor, add=T)
swi2019r.bor <- crop(swi2019r, ext)
# plot(swi2019r.bor, col=cla, las=1, main="SWI Borneo 2019")
# plot(admin.bor, add=T)



# Cerco valori max e min delle immagini Copernicus del 2015 e del 2019
swi2015r.bor # 0 --> 95
swi2019r.bor # 0 --> 83.5

# Graficizzo le due immagini e via via faccio confronti tra i vari anni
# [clx <- colorRampPalette(c('red','yellow','light blue'))(100)]
par(mfrow=c(1,2))
plot(swi2015r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2015-2019 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2019r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)


par(mfrow=c(1,2))
plot(swi2015r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2015-2016 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2016r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)

par(mfrow=c(1,2))
plot(swi2016r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2016-2017 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2017r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)

par(mfrow=c(1,2))
plot(swi2017r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2017-2018 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2018r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)

par(mfrow=c(1,2))
plot(swi2018r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2018-2019 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2019r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)


dev.off()



### Seconda parte

# Carico incendi del 7 novembre delle annate da 2015 a 2019
# Li plotto insieme ai rispettivi ritagli
fire2015r <- readOGR("fire_archive_V1_134134.shp")
# plot(fire2015r)
fire2016r <- readOGR("fire_archive_V1_134135.shp")
# plot(fire2016r)
fire2017r <- readOGR("fire_archive_V1_134136.shp")
# plot(fire2017r)
fire2018r <- readOGR("fire_archive_V1_134137.shp")
# plot(fire2018r)
fire2019r <- readOGR("fire_archive_V1_134138.shp")
# plot(fire2019r)

# Graficizzo incendi e SWI
par(mfrow=c(5,1))
plot(swi2015r.bor, zlim=c(0,100), col=cla, las=1, ylab="lat") # «main» tolto perché non viene visualizzato
plot(fire2015r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2016r.bor, zlim=c(0,100), col=cla, las=1, ylab="lat")
plot(fire2016r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2017r.bor, zlim=c(0,100), col=cla, las=1, ylab="lat")
plot(fire2017r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2018r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(fire2018r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2019r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(fire2019r, add=T, pch=21, col="red")
plot(admin.bor, add=T)


'''
plotRGB(swi_12.5km_bor, r=4, g=3, b=2, zlim=c(0,100), stretch="Lin", main="Soil Water Index in Borneo (2015-2019)")
plot(dif_swi_1915, col=clgg)
'''

dev.off()


# Provo a vedere se differenza tra SWI e incendi corrisponde
dif_swi_1615 <- swi2016r.bor - swi2015r.bor
dif_swi_1615 # Range: -60, 97
# Se differenza dà risultato negativo significa che SWI è diminuito
# Se differenza dà risultato positivo significa che si è verificato un aumento

# Analizzo differenze di SWI di ogni anno confrontandoli con gli incendi
clr <- colorRampPalette(c('green', 'yellow', 'red')) (400)
par(mfrow=c(3,1))
plot(dif_swi_1615, col=clr, las=1, main="Differenza SWI Borneo 2016-2015 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2015r, pch=21, col="blue", las=1)
plot(admin.bor, add=T)
plot(fire2016r, pch=21, col="red", las=1)
plot(admin.bor, add=T)


dif_swi_1716 <- swi2017r.bor - swi2016r.bor
dif_swi_1716 # Range: -65, 51.5
par(mfrow=c(3,1))
plot(dif_swi_1716, col=clr, las=1, main="Differenza SWI Borneo 2017-2016 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2016r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2017r, pch=21, col="red")
plot(admin.bor, add=T)

dif_swi_1817 <- swi2018r.bor - swi2017r.bor
dif_swi_1817 # Range: -55.5, 46.5
par(mfrow=c(3,1))
plot(dif_swi_1817, col=clr, las=1, main="Differenza SWI Borneo 2018-2017 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2018r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2017r, pch=21, col="red")
plot(admin.bor, add=T)

dif_swi_1918 <- swi2019r.bor - swi2018r.bor
dif_swi_1918 # Range: -61.5, 29
par(mfrow=c(3,1))
plot(dif_swi_1918, col=clr, las=1, main="Differenza SWI Borneo 2019-2018 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2019r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2018r, pch=21, col="red")
plot(admin.bor, add=T)



# Faccio differenza tra immagine 2019 e immagine 2015 e la graficizzo in scala di grigi
dif_swi_1915 <- swi2019r.bor - swi2015r.bor
dif_swi_1915 # Range: -76, 57.5
clgg <- colorRampPalette(c('black','grey','red'))(100)
par(mfrow=c(3,1))
plot(swi2019r.bor, zlim=c(0,100), las=1, main="Differenza SWI 2019-2015", col=cla, xlab="long", ylab="lat")
plot(swi2015r.bor, zlim=c(0,100), col=cla, xlab="long", ylab="lat", las=1)
plot(dif_swi_1915, col=clgg, xlab="long", ylab="lat", las=1)
# Dove è nero c'è stata una forte diminuzione, mentre dove è rosso un aumento

# Graficizzo scala di grigi con incendi
par(mfrow=c(3,1))
plot(dif_swi_1915, col=clgg, las=1, main="Differenza SWI Borneo 2019-2015 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2019r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2015r, pch=21, col="red")
plot(admin.bor, add=T)

dev.off()




### Terza parte

# Uso funzione «unsuperClass()» per riclassificare immagini in 2 classi

cldy <- colorRampPalette(c('dark green', 'yellow'))(100) 

borneo2015_rec <- unsuperClass(swi2015r.bor, nClasses=2)
borneo2019_rec <- unsuperClass(swi2019r.bor, nClasses=2)
borneo2015_rec # Ha valori da 1 a 2
borneo2019_rec # Ha valori da 1 a 2

# Graficizzo le classi create per vedere a che valori corrispondono e le metto a confronto con immagini Copernicus
par(mfrow=c(2,2))
plot(borneo2015_rec$map, col=cldy, las= 1, xlab="long", ylab="lat", main="Classi SWI create (2)")
plot(swi2015r.bor, las=1, xlab="long", ylab="lat")

plot(borneo2019_rec$map, col=cldy, las= 1, xlab="long", ylab="lat")
plot(swi2019r.bor, las=1, xlab="long", ylab="lat")
# Classe 1: SWI basso
# Classe 2: SWI alto

dev.off()

# Graficizzo solo le due immagini con le classi
par(mfrow=c(1,2))
plot(borneo2015_rec$map, col=cldy, las= 1, main="Valori SWI Borneo 2015 e 2019 in 2 classi:", xlab="long", ylab="lat")
plot(borneo2019_rec$map, col=cldy, las=1, xlab="long", ylab="lat", main="1) Basso; 2) Alto")

# Uso funzione «reclassify()» per eliminare la seconda classe, corrispondente ai valori alti
# Voglio lavorare solo su valori alti, così da poterli confrontare con gli incendi
cldw <- colorRampPalette(c('dark green', 'white')) (100)
borneo2015_for <- reclassify(borneo2015_rec$map, cbind(2, NA))
borneo2019_for <- reclassify(borneo2019_rec$map, cbind(2, NA))

# Uso funzione «clump()» per suddividere quello che ho ottenuto in patches
borneo2015_for.patches <- clump(borneo2015_for)
borneo2019_for.patches <- clump(borneo2019_for)
borneo2015_for.patches
borneo2019_for.patches

# patches:
# 2015: 36
# 2019: 24

# Creo due immagini .tif con le nuove suddivisioni
writeRaster(borneo2015_for.patches, "borneo2015_for.patches.tif")
writeRaster(borneo2019_for.patches, "borneo2019_for.patches.tif")

dev.off()

# Graficizzo le immagini ottenute
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)

par(mfrow=c(1,2))
plot(borneo2015_for.patches,col=clp, las=1, main="Patches SWI Borneo 2015-2019", xlab="long", ylab="lat")
plot(borneo2019_for.patches,col=clp, las=1, xlab="long", ylab="lat")
dev.off()

# Le graficizzo confrontandole con gli incendi del 2015 e del 2019
par(mfrow=c(2,2))
plot(borneo2015_for.patches,col=clp, las=1, main="Patches basso SWI Borneo 2015-2019 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(borneo2019_for.patches,col=clp, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(borneo2015_for.patches,col=clp, las=1, xlab="long", ylab="lat")
plot(fire2015r, add=T, pch=21, col="red")
plot(admin.bor, add=T)
plot(borneo2019_for.patches,col=clp, las=1, xlab="long", ylab="lat")
plot(fire2019r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

dev.off()

# Creo un grafico con «ggplot()» che rappresenta la suddivisione in patches nel 2015 e nel 2019
time <- c("Basso SWI 2015","Basso SWI 2019")
npatches <- c(13,20)

doc <- data.frame(time,npatches)
attach(doc)

ggplot(doc, aes(x=time, y=npatches, color="red"), main="trrr") + geom_bar(stat="identity", fill="white")

dev.off()
