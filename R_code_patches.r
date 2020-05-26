# Lavoriamo sui patch del paesaggio
# Dopo la variazione dell'area, studiamo la variazione delle patch!

setwd("/Users/enricopriarone/lab")
library(raster)
library(ggplot2)

# Occorre installare «igraph» perché non l'ha scaricatp direttamente con libreria «raster»
install.packages("igraph")
library(igraph)

# Carico le immagini
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c,col=cl)
plot(d2c,col=cl)
# foresta: classe 2
# agricoltura: classe 1

# Eliminiamo tutti i valori di agricoltura, inserendoli come valori nulli («not assigned»)
# Usiamo funzione «reclassify»
d1c.for <- reclassify(d1c, cbind(1,NA)) # «d1c.for» di solito è nome da modello; in alternativa «d1c_for»

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c,col=cl)
plot(d1c.for, col=cl)

d1c.for
# unico valore è «2»

d2c.for <- reclassify(d2c, cbind(1,NA))
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d2c,col=cl)
plot(d2c.for, col=cl)

# Graficizzo le due nuove carte
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

# Creiamo i vari patch
# facendo un clump
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# Salvo dati verso l'esterno, ossia su «lab» (creo nuovo file)
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

# Esercizio: graficizzare le due mappe una accanto all'altra
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

# Definiamo quante patch sono state create
d1c.for.patches # Ha 301 patches
d2c.for.patches # Ha 1212 patches

# Creiamo un frame
time <- c("1. Before deforestation","2. After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)
output

# Grafico finale
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
# Si vede chiaramente l'aumento delle patch dopo la deforestazione!

dev.off()
