# Codice analisi dati NO2 da Sentinel-2 ESA (Copernicus), da gennaio a marzo 2020

setwd("/Users/enricopriarone/lab")
library(raster)

# Visualizzo immagini scaricate
EN01 <- raster("EN_0001.png") # Lo "0" prima è importante perché R non li riconosce come numeri, ma come stringhe:
plot(EN01)                    # altrimenti la successiva sarebbe stata "EN10" e non "EN2"

# Esercizio: caricare (e graficizzare) tutte le altre immagini
EN02 <- raster("EN_0002.png")
plot(EN02)
EN03 <- raster("EN_0003.png")
plot(EN03)
EN04 <- raster("EN_0004.png")
plot(EN04)
EN05 <- raster("EN_0005.png")
plot(EN05)
EN06 <- raster("EN_0006.png")
plot(EN06)
EN07 <- raster("EN_0007.png")
plot(EN07)
EN08 <- raster("EN_0008.png")
plot(EN08)
EN09 <- raster("EN_0009.png")
plot(EN09)
EN10 <- raster("EN_0010.png")
plot(EN10)
EN11 <- raster("EN_0011.png")
plot(EN11)
EN12 <- raster("EN_0012.png")
plot(EN12)
EN13 <- raster("EN_0013.png")
plot(EN13)
# In alternativa uso un ciclo "for": guardare in codice su "iol"!

ls()

# Visualizziamo le immagini iniziale e finale per vedere le differenze
cl <- colorRampPalette(c('red','orange','yellow'))(100)
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)
dev.off()

# Graficizzo le differenze
difno2 <- EN13 - EN01
cldif <- colorRampPalette(c('blue','black','yellow'))(100) #
plot(difno2, col=cldif)
dev.off()

# Esercizio: faccio grafico di tutte le immagini con un "par"
par(mfrow=c(4,4))
plot(EN01, col=cl)
plot(EN02, col=cl)
plot(EN03, col=cl)
plot(EN04, col=cl)
plot(EN05, col=cl)
plot(EN06, col=cl)
plot(EN07, col=cl)
plot(EN08, col=cl)
plot(EN09, col=cl)
plot(EN10, col=cl)
plot(EN11, col=cl)
plot(EN12, col=cl)
plot(EN13, col=cl)
# Domani proviamo col "for"

dev.off()

# 06/05
# Cambio la working directory con la nuova cartella
setwd("/Users/enricopriarone/lab/esa_no2")
library(raster)

# creo una lista di pattern, di cui diciamo che sono nominati ".png"
rlist <- list.files(pattern=".png")
rlist

# usiamo funzioni "raster" (non "brick", perché carica l'intero pacchetto layer satellitare: a noi ne serve uno solo)
# e "lapply"
listafinale <- lapply(rlist, raster)
listafinale
# sono 13 RasterLayer

# usiamo funzione "stack" per creare un pacchetto unico di dati: creiamo un'unica immagine
EN <- stack(listafinale)
EN

cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)
# Questo permette di caricare il set completo con tutte le n immagini, senza lavorare su ogni singola

dev.off()

# 12/05
# Grafici e boxplot

library(raster)
setwd("/Users/enricopriarone/lab/esa_no2")

rlist <- list.files(pattern=".png")
rlist

listafinale <- lapply(rlist, raster)
listafinale

EN <- stack(listafinale)
EN

difEN <- EN$EN_0013 - EN$EN_0001
cld <- colorRampPalette(c('blue','white','red'))(100)
plot(difEN, col=cld)

cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)

# Creiamo un boxplot di EN in orizzontale
boxplot(EN, horizontal=T,outline=F,axes=T)

dev.off()
