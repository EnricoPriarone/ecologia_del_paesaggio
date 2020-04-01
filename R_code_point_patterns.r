# 31/03
# Codice per analisi delle strutture relazionate ai punti nello spazio (point patterns)

# Installo tutti i pacchetti necessari
install.packages("raster")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("spatstat")

# Richiamo file con dati Covid
setwd("/Users/enricopriarone/lab")

# Importo dati tabella
covid <- read.table("covid_agg.csv", head=T)
head(covid)

# Facciamo un plot per visualizzare la distribuzione dei dati
# Dollaro serve a relazionare ogni colonna al proprio dataset
# Altrimenti uso "attach(covid)" e poi "plot(country, cases)"
plot(covid$country, covid$cases)

# Grafico si vede male, allora lo rendiamo verticale
# Provo con lable=0 -> non è cambiato nulla (perché è il default) -> è parallel
plot(covid$country, covid$cases, las=0) # parallel lables

# Con las=1 ho orizzontale ("horizontal")
# Con las=2 labels sono perpendicolari
# Con las=3 sono tutte verticali
plot(covid$country, covid$cases, las=1) # horizontal lables
plot(covid$country, covid$cases, las=2) # perpendicular lables
plot(covid$country, covid$cases, las=3) # vertical lables

# Diminuisco grandezza punti con "cex.axis=0.3"
plot(covid$country, covid$cases, las=3, cex.axis=0.3)

# Faccio plot tramite ggplot
library(ggplot2) # o require(ggplot2)

# Visualizzo dati in ggplot
data(mpg)
head(mpg)

# dichiaro data (mpg)
# diachiaro aes (aesthetics)
# dichiaro tipo di geometria attraverso un'altra micro funzione
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()

# provo a trasformare in grafico per linee
# provo anche con i poligoni
ggplot(mpg, aes(x=displ, y=hwy)) + geom_line()
ggplot(mpg, aes(x=displ, y=hwy)) + geom_polygon()

# Faccio lo stesso con i dati su Covid già caricati
# Prima metto i dati ("covid"), poi le aes (le cerco con "names" o "head" -> scelgo "lon", "lat" e i "cases" per grandezza)
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()

# Richiamo "spatstat"
library(spatstat)

# Uso applicazione point pattern e la collego a dati Covid
# Metto lon e lat e i range di x(-180 ≤ lon ≤ 180) e y(-90 ≤ lat ≤ 90)
attach(covid)
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

# Faccio funzione density, ossia la densità del set spaziale sul Covid
# e ne faccio il grafico
d <- density(covids)
plot(d)

# Inserisco nel grafico punti di origine e i contorni dei Paesi
points(covids)

# Salvo codice R su file come .rdata
q()

# 01/04
# Richiamo la cartella "lab" (dove ho anche .Rdata)
# Ricarico i dati di ieri richiamando .Rdata
setwd("/Users/enricopriarone/lab")
load("point_pattern.RData") # Così ho caricato il file!

# Per visualizzare i file:
ls()

# Carico "spatstat"
library(spatstat)

# Faccio grafico sulla densità
# e cambio un po' le cose
# ad esempio creando una palette di colori, tramite un array (con "c")
# Occhio: in questa funzione usare virgolette singole!
# Metto il numero di livelli di colore che voglio, aggiungendo tra parentesi il numero
plot(d)
cl <- colorRampPalette(c('yellow', 'orange', 'red'))(100) # Funzione importantissima!
plot(d, col=cl)

# Esercizio: plot con colori da verde a blu
cl2 <- colorRampPalette(c('green', 'dark green', 'light blue', 'blue'))(200)
plot(d, col=cl2)

# Aggiungo i dati (punti) del Covid
points(covids)

# Carico i confini dei Paesi attraverso "coastlines" con risoluz. 10 m
# Cartella contiene vari file, in particolare lo shapefile
# Associo il file alla funzione file vettoriali
# Se dà errore è perché non ho caricato la libreria "rgdal": la carico prima di "coastlines"
library(rgdal) # Permette in tutti i software e sistemi operativi di leggere raster e vettori
coastlines <- readOGR("ne_10m_coastline.shp")

# Faccio plot coastlines e lo aggiungo al plot precedente con "add"
plot(coastlines, add=T)

# Più avanti andremo a cartografare anche il numero di casi!

# Esercizio: plot mappa di densità con nuova colorazione e aggiunta coastlines
cl3 <- colorRampPalette(c('light blue', 'green', 'dark green', 'blue'))(400)
plot(d, col=cl3)
plot(coastlines, add=T)
