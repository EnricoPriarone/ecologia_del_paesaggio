# R code analisi multitemporale di variazione della land cover
# Copertura del suolo viene da telerilevazioni

setwd("/Users/enricopriarone/lab")
install.packages("Rcmdr")
library(raster)
library(RStoolbox)
library(ggplot2)

# Carichiamo immagine satellitare da cartella "lab" attraverso funzione di raster "brick"
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

# defor1_.1 = NIR
# defor1_.2 = red
# defor1_.3 = green
# Associamo banda rosso a NIR, verde a rosso e blu a verde
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# Andiamo a usare due classi non supervisionate: non spieghiamo al computer la divisione
# Computer raggruppa pixel che sembrano simili tra loro
d1c <- unsuperClass(defor1, nClasses=2)
d1c # Visualizzo i suoi dettagli

# Creo mappa
plot(d1c$map)
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)

# Esempio su significato dollaro
# mappageologica <- geomap(im_sat, nClasees=...)
# plot (mappageologica$lito)
# plot(mappageologica$lineaments)

# Esercizio: classificare con due classi l'immagine satellitare defor2
d2c <- unsuperClass(defor2, nClasses=2)
d2c
plot(d2c$map)
plot(d2c$map, col=cl)

dev.off()

# Faccio nuovo grafico delle due carte ottenute
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

# Visualizzo come sono stati suddivisi i pixel nelle classi
freq(d1c$map)
# aree altre: 35516
# foresta: 305776

totd1 <- 305776 + 35516 # Numero pixel carta
totd1
# 341292

# Calcolo le proporzioni, la percentuale delle frequenze
percent1 <- freq(d1c$map)*100/totd1
percent1

# percentuali:
# aree altre: 10.4
# foreste: 89.6

# Idem per la carta 2
freq(d2c$map)
# aree altre: 164321
# foreste: 178405

totd2 <- 164321 + 178405 # Numero pixel carta
totd2
# 342726

percent2 <- freq(d2c$map)*100/totd2
percent2

# aree altre: 48
# foreste: 52

# Creo dataframe con i dati
cover <- c("Agriculture","Forest")
before <- c(10.4,89.6)
after <- c(48,52)
output <- data.frame(cover,before,after)
View(output)

dev.off()

# 05/05
setwd("/Users/enricopriarone/lab")
load("defor.RData") # Uso questo perché il mio ".RData" mi dà errore: i dati saranno un po' diversi
ls()
library(raster)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) # 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
output

# Creo grafico con percentuale di foresta/agricoltura prima della deforestazione ("before")
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
# agricoltura occupa percentuale molto bassa: ca. 10%; contro il ca. 90% della foresta

# Esercizio: faccio lo stesso per il dopo deforestazione ("after")
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")
# agricoltura è vicina al 50%; foresta lo supera appena

# Usiamo funzione "par"
# Necessita del pacchetto "gridExtra", che installiamo
# Facciamo istogramma delle percentuali associando prima i due grafici a un nome
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# Esercizio: usare "grid.arrange" per graficizzarli
grid.arrange(grafico1, grafico2, nrow=1)

# Manca da cambiare la y! Vedi poi "iol"

dev.off()

# 06/05
# Cambiamo la y
setwd("/Users/enricopriarone/lab")
load(".RData")
ls()
library(raster)
library(ggplot2)
library(gridExtra)

grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

grid.arrange(grafico1, grafico2, nrow = 1)

# "ylim" funziona con "ggplot" e permette di inserire in limite all'ordinata
# Prendere libro "ggplot2. Elegant graphics..." di Whickam!
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

# Esercizio: usa grid.arrange graficizzare
grid.arrange(grafico1, grafico2, nrow = 1)
# Così scala nelle ordinate (y) è la stessa

dev.off()
