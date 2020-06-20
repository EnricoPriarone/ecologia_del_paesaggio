### Codice esame
# Ho scaricato dati Copernicus relativi al Surface Soil Moisture, ossia l'indice di idratazione dei suoli.
# Può essere utile anche in agricoltura. [Aggiungi roba]
# Albergel et al., 2008 + Wagner et al., 1999
# Comparerò i dati riferiti a 6-7 novembre di cinque anni diversi (dal 2015 al 2019), per osservare eventuali cambiamenti.
# Proverò a vedere se ci sono eventuali correlazioni con i dati NDVI che ho già scaricato.
# In caso affermativo proverò a fare una valutazione usando l'immagine NASA del 2000 relativa alle terre coltivate.


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

# Carico le immagini singolarmente perché con «lapply» non riesco a lavorare sulle singole immagini
swi2015r <- raster("c_gls_SWI_201511071200_GLOBE_ASCAT_V3.1.1.nc")
swi2016r <- raster("c_gls_SWI_201611071200_GLOBE_ASCAT_V3.1.1.nc")
swi2017r <- raster("c_gls_SWI_201711071200_GLOBE_ASCAT_V3.1.1.nc")
swi2018r <- raster("c_gls_SWI_201811071200_GLOBE_ASCAT_V3.1.1.nc")
swi2019r <- raster("c_gls_SWI_201911071200_GLOBE_ASCAT_V3.1.1.nc")
# Faccio qualche plot di verifica
# plot(swi2015r)
# plot(swi2016r)
# plot(swi2017r)
# plot(swi2018r)
# plot(swi2019r)

# Ritaglio tutte le immagini imponendo come estensione quella dell'  e faccio plot di verifica
extension <- c(21, 43, 44, 54)
swi2015r.ucr <- crop(swi2015r, extension)
# plot(swi2015r.ucr)
swi2016r.ucr <- crop(swi2016r, extension)
# plot(swi2016r.ucr)
swi2017r.ucr <- crop(swi2017r, extension)
# plot(swi2017r.ucr)
swi2018r.ucr <- crop(swi2018r, extension)
# plot(swi2018r.ucr)
swi2019r.ucr <- crop(swi2019r, extension)
# plot(swi2019r.ucr)

# Carico l'immagine NASA relativa a terreni agricoli
# «The downloaded grid values represent the proportion of pixel are that is either under cropland or pasture.
# The grid values range between 0 and 1.
# 0 means zero percent of pixel area under cropland or pasture present and 1 - 100% of the pixel area under cropland or pasture.» 
agric <- raster("cropland.tif")
# plot(agric)
   
# Creo una colour palette che aiuti a ben rappresentare i valori
clg <- colorRampPalette(c('dark green','green','white'))(100)

# clgg <- colorRampPalette(c('black','grey','light grey'))(100)
# cl in scala di grigi

# Faccio un ritaglio per selezionare l'Ucraina
# extension <- c(21, 43, 44, 54)
agric.ucr <- crop(agric, extension)
plot(agric.ucr)

# Aggiungo confini di Stato per inquadrare meglio l'Ucraina
admin <- readOGR("ne_10m_admin_0_countries.shp")
plot(admin, add=T)

# Ne faccio un ritaglio
admin.ucr <- crop(admin, extension)
# plot(admin.ucr)

# Plotto immagine Nasa e immagine Copernicus 2015 insieme
par(mfrow=c(1,2))
plot(swi2015r.ucr)
plot(admin.ucr, add=T)
plot(agric.ucr)
plot(admin.ucr, add=T)

# Cerco valori max e min delle immagini Copernicus del 2015 e del 2019
swi2015r.ucr # 3.5 --> 75.5
swi2019r.ucr # 4.5 --> 92.5

# Graficizzo le due immagini e via via faccio confronti tra i vari anni
cla <- colorRampPalette(c('light blue','yellow','red'))(100)
par(mfrow=c(1,2))
plot(swi2015r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)
plot(swi2019r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)

'''
par(mfrow=c(1,2))
plot(swi2015r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)
plot(swi2016r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)

par(mfrow=c(1,2))
plot(swi2016r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)
plot(swi2017r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)

par(mfrow=c(1,2))
plot(swi2017r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)
plot(swi2018r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)

par(mfrow=c(1,2))
plot(swi2018r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)
plot(swi2019r.ucr, zlim=c(0,100), col=cla)
plot(admin.ucr, add=T)
'''


# Prova unsuperClass per i modelli


# Carico le immagini Copernicus, tutte insieme

# Creo una lista di pattern denominati «.nc»
nclist <- list.files(pattern=".nc")
nclist # Così vedo l'elenco delle immagini incluse

# Uso funzioni «raster» e «lapply»
listafinale <- lapply(nclist, raster)
listafinale # Vedo i cinque RasterLayer

# Uso funzione «stack» per creare un pacchetto unico di dati: un'unica immagine
swi_12.5km <- stack(listafinale)
swi_12.5km # È di classe RasterStack

# Graficizzo
plot(swi_12.5km)

# Ritaglio le immagini
swi_12.5km_ucr <- crop(swi_12.5km, extension)
swi_12.5km_ucr # Richiamando il set vedo che valori variano circa tra 0 e 100: li metto come limiti con «zlim»
# cla <- colorRampPalette(c('brown','yellow','dark blue'))(200)
plot(swi_12.5km_ucr, col=cla, main="Soil Water Index in Ucraina (2015-2019)", zlim=c(0,100))

# Provo mappa in RGB
plotRGB(swi_12.5km_ucr, r=4, g=3, b=2, zlim=c(0,100), stretch="Lin") # Non prende «main»
plot(admin.ucr, add=T)

# Faccio differenza tra immagine 2015 e imm. 2019 e la graficizzo in scala di grigi
clgg <- colorRampPalette(c('black','grey','red'))(100)
par(mfrow=c(1,3))
plot(swi2019r.ucr, zlim=c(0,100))
plot(swi2015r.ucr, zlim=c(0,100))
plot(difimm, col=clgg)

# Le graficizzo insieme
par(mfrow=c(2,1))
plotRGB(swi_12.5km_ucr, r=4, g=3, b=2, zlim=c(0,100), stretch="Lin", main="Soil Water Index in Ucraina (2015-2019)")
plot(difimm, col=clgg)



dev.off()
