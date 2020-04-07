# Codice R per analisi di immagini satellitari

# Installo pacchetti che servono o li richiamo
install.packages("RStoolbox") # Verifica
library(raster)
library(RStoolbox) # Verifica

# Richiamo la setwd
setwd("/Users/enricopriarone/lab")

# Uso p224r63_2011_masked.grd
# Associamo il file alla funzione "brick", per importare immagine satellitare con tutte le bande
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# Cerco informazioni sull'immagine attraverso grafici su bande, riflettanze ecc.
plot(p224r63_2011)

# Ci appare stessa immagine in 7 bande, che per LANDSAT sono sempre le stesse:
# 1) blu; 2) verde; 3) rosso; 4) NIR; 5) infr. medio; 6) infr. termico; 7) infr. medio.

# Salvo RData
