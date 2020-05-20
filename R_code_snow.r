# Analisi copertura nevosa con dati Copernicus
# https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

setwd("/Users/enricopriarone/lab")
install.packages("ncdf4")
library(ncdf4)
library(raster)

# Importiamo file .nc
# Esercizio: poi facciamo grafico
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snowmay, col=cl)

# Dobbiamo settare una nuova working directory
setwd("/Users/enricopriarone/lab/snow")

# Esercizio: importiamo l'intera serie di file
rlist = list.files(pattern=".tif", full.names=T)

# Salvo raster in una lista
# con «lappy»
list_rast = lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp, col=cl)

# Lavoriamo su dati del 2000 e del 2020
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250)) # «zlim» serve per avere la stessa scala sulle ordinate
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))
dev.off()

# Facciamo la differenza per poi graficizzarla
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue', 'white', 'red'))(100)
plot(difsnow, col=cldiff)

# Facciamo una previsione
# Carichiamo un'analisi dall'esterno attraverso funzione «source»
source("prediction.r")

# Uso dato finale perché il precedente è troppo pesante
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)

dev.off()





