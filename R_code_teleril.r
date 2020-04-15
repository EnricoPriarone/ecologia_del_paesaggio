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
# 1) blu;
# 2) verde;
# 3) rosso;
# 4) NIR;
# 5) infr. medio; 
# 6) infr. termico;
# 7) infr. medio.

# Salvo RData

# Richiamo RData
setwd("/Users/enricopriarone/lab")
load(".RData")
ls()
library(raster)

plot(p224r63_2011)

# Cambio colorazione in scala di grigi
cl <- colorRampPalette(c('black','grey','light grey'))(100)
plot(p224r63_2011, col=cl)
names(p224r63_2011) # Visualizza le sette bande che sta usando

# Faccio una palette con la scala del blu
# attach(dataframe) non funziona con i raster
# allora per legare immagine a banda del blu uso simbolo che lega la colonna (banda) al dataset (immagine satellitare): "$"
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
# parte bianca è mascherata: i pixel sono stati eliminati perché non servivano

# Esercizio: plottare banda del NIR con:
# colorRampPalette che varia dal rosso all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# creo multiframe
# uso "par" per usare finestra a blocchi
par(mfrow=c(2,2))

# Inizio col blu, poi verde ecc.
# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)

# green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clg) # è intermedio tra verde e NIR

# red
clr <- colorRampPalette(c('dark red','red','orange'))(100)
plot(p224r63_2011$B3_sre, col=clr) # Sono bassi perché quasi tutto rosso è assorbito per fotosintesi

# NIR
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir) # Valori di NIR sono molto alti -> buona parte è foresta

# Montiamo le bande insieme per fare immagine come la vedrebbe l'occhio umano (con funz. "plotRGB")
# Usiamo le 3 componenti: R=banda del rosso G=verde B=blu (sono disponibili solo 3 bande tutte insieme)
# Chiudiamo prima la finestra plottata
dev.off()
plotRGB(p224r63_2011, r=3, g=2, b=1)

# Stiro un po' i colori per vederlo meglio
# Quello lineare è il più usato
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# Usiamo anche il NIR per distinguere ombre da piante
# Ma sono disponibili solo 3 bande alla volta: occorre eliminarne una
# Elimino la banda blu scalando tutto di uno: r=4, g=3, b=2 )
# È chiamata "432" o "false colours" ("falsi colori")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# Salvo il grafico in PDF
# Per png: png"primografico.pdf")
pdf("primografico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# Faccio multiframe con immagini a colori reali e imm. a colori falsati
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# Esercizio: metto NIR in componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
# Abbiamo fatto in modo che zona boschiva si veda molto bene in verde acceso

# Ora lo metto nel blu
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")
dev.off()

# 15/04
# Richiamo le librerie, la working directory e il file .RData
library(raster)
setwd("/Users/enricopriarone/lab")
load(".RData")
ls()

# Importo file del 1988 con tutte le bande
# "grd" sta per griglia: formato è di righe e colonne
p224r63_1988 <- brick("p224r63_1988_masked.grd")

# Facciamo un plot dell'immagine
plot(p224r63_1988)

# Rifacciamo il lavoro dell'altra volta con l0immagine del 1988
par(mfrow=c(2,2))

# Inizio col blu, poi verde ecc.
# blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre, col=clb)

# green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg) # è intermedio tra verde e NIR

# red
clr <- colorRampPalette(c('dark red','red','orange'))(100)
plot(p224r63_1988$B3_sre, col=clr) # Sono bassi perché quasi tutto rosso è assorbito per fotosintesi

# NIR
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre, col=clnir) # Valori di NIR sono molto alti -> buona parte è foresta

# Faccio plot con le tre bande principali
# B1: blu;
# B2: verde;
# B3: rosso;
# B4: NIR;
# B5: infr. medio; 
# B6: infr. termico;
# B7: infr. medio.
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# Esercizio: fare plot dell'immagine usando il NIR nella componente "r"
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # Già che ci sono sposto tutte le bande di 1: da 4-2-1 a 4-3-2

# Faccio plot delle due immagini (1988 e 2011)
# Devo fare un multipanel: funz. "par", con "mfrow"
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# Confronto: agricolo è molto più sviluppato nel 2011

dev.off()

# Creo un indice per verificare le differenze
# DVI=NIR-RED, e questo si fa pixel per pixel
# Indice spettrale DVI
# dvi_1988 = nir1988-red1988
# uso il $ per collegare le bande al dataset
dvi_1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi_1988)

# Esercizio: calcolare DVI del 2011
dvi_2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi_2011)

# Provo altri colori
cldvi <- colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi_2011, col=cldvi)

# Faccio differenza nel tempo tra i due indici: multitemporal analysis
# Valore è positivo se vegetazione è in buone condizioni nel 2011 rispetto al 1988
# Viceversa, se è negativo ora sta peggio
difdvi <- dvi_2011 - dvi_1988
plot(difdvi)

# Cambio i colori:
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi)

# Faccio par multiframe per vedere le tre immagini
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()

# Provo a cambiare la risoluzione delle immagini
# con pixel più grandi, dunque a risoluzione minore
# La riduco di n volte: se pixel è di 30 m, fact=10 mi dà nuovo pixel di 300 m
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

# Guardo caratteristiche immagini
# imm. originale ha resolution 30x30
# nuova 300x300 e righe sono meno
p224r63_2011
p224r63_2011lr

# Faccio grafico
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# Riduco ancora
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50

par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")
dev.off()

# Faccio dvi a bassa risoluzione
dvi_2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
plot(dvi_2011lr50) # Ma con questo perdo tantissimo e sembra tutto omogeneo

# Faccio lo stesso per il 1988
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi_1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

# Faccio difdvi a bassa risoluzione
difdvilr50 <- dvi_2011lr50 - dvi_1988lr50
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvilr50,col=cldifdvi)

# multiframe
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
# Noto dalla differenza che nella prima immagine posso individuare le micro disomogeneità
