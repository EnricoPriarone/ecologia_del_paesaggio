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
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100) # blue
plot(p224r63_2011$B1_sre, col=clb)

clg <- colorRampPalette(c('dark green','green','light green'))(100) # green
plot(p224r63_2011$B2_sre, col=clg) # è intermedio tra verde e NIR

clr <- colorRampPalette(c('dark red','red','orange'))(100) # red
plot(p224r63_2011$B3_sre, col=clr) # Sono bassi perché quasi tutto rosso è assorbito per fotosintesi

clnir <- colorRampPalette(c('red','orange','yellow'))(100) # NIR
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
