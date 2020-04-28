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
# e cambio al colore ai punti in vista del grafico finale
points(covids, col="red")

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


# 22/04
# Carico "point_pattern.RData"
setwd("/Users/enricopriarone/lab")
load("point_pattern.RData")
ls()

# Esercizio: cartografare densità dei punti
# Per farlo riprendo comandi della volta scorsa e li aggiusto per i miei obiettivi
library(spatstat)
library(rgdal)
plot(d)
cl <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(d, col=cl)
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# Andiamo a vedere il numero di casi con l'interpolazione,
# che stima i numeri nelle zone in cui non è stato fatto il campionamento
# Con funzione "marks" andiamo a prendere i dati nel point pattern "covids"
# Il dollaro ci serve per non fare l'"attach"
head(covid)
marks(covids) <- covid$cases

# Chiamo "s" la stima dei valori attraverso la funzione di smooth
s <- Smooth(covids)
plot(s)

# Rifare plot di s, cambiando palette e aggiungendo punti e coastlines
cls <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(s, col=cls)
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T, main="Interpolazione")
# Si vede che mettendo il focus sul numero di casi l'Asia a febbraio era la più colpita

# Con "text" vediamo anche valori dei punti
text(covids) # Ma questa volta ci dà l'ID, non il valore dei casi

# Facciamo una carta finale con entrambi i plot
# Usiamo un multiframe
par(mfrow=c(2,1))
# densità:
cl <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(d, col=cl, main="Densità")
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)
# interpolazione:
cls <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(s, col=cls, main="Interpolazione")
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# Chiudo il grafico
dev.off()

# San Marino, caso di studio
# Carico i dati esterni ("Tesi.RData") che ho scaricato da iol
# Serve libreria spatstat
# library(spatstat)
load("Tesi.RData")
ls()
head(Tesi)

# Facciamo un attach della tabella per usarla con il point pattern
attach(Tesi)

# Point pattern: x, y, c(xmin,xmax), c(ymin,ymax)
# Mettiamo anche i limiti per le due coordinate, trattandosi di un'area ristretta
# Uso funzione summary(dataset) per avere il sommario della tabella
summary(Tesi)
# y(lat.) va da 43.91 a 43.94, y(lon.) da 12.42 a 12.46
# Ma noi lasciamo un po' di margine
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9, 43.95))

# Facciamo densità punti e la graficizziamo
dT <- density(Tesippp)
plot(dT)
points(Tesippp)
# Nella parte centrale c'è densità più alta

dev.off()

# 28/04
setwd("/Users/enricopriarone/lab")
load("Tesi.RData")
ls()
# Ottengo i file presenti:
# "dT" è density map di Tesippp
# "Tesi" è un dataset
# "Tesippp" è il point pattern del file "Tesi" (da libreria "Spatstat")

# Associamo i valori che vogliamo stimare
library(spatstat)
plot(dT)
points(Tesippp, col="green")

# Andiamo a stimare la ricchezza specifica
# Con "head" vedo che si trova sotto "Species_richness"
# "marks" va a prendere i valori dalla tabella e li associa ai punti del ppp
head(Tesi)
marks(Tesippp) <- Tesi$Species_richness

# Creo mappa e l'associo a Smooth
interpol <- Smooth(Tesippp)

# Usiamo file su San Marino
# Carico libreria "rgdal"
library(rgdal)
sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T) # Importante! Con "T"/"True" aggiunge nuova mappa a quella precedente
points(Tesippp,col="green")
# Mappa va a sovrapporsi al territorio di San Marino
plot(sanmarino, add=T) # Così i confini di Stato si sovrappongono al plot "interpol"

# Esercizio: plot multiframe di densità e interpolazione
par(mfrow=c(2,1))
plot(dT, main="Densità di punti")
points(Tesippp, col="green")
plot(interpol, main="Stima della ricchezza di specie")
points(Tesippp, col="green")

# Esercizio: inverto la disposizione del grafico
par(mfrow=c(1,2))
plot(dT, main="Densità di punti")
points(Tesippp, col="green")
plot(interpol, main="Stima della ricchezza di specie")
points(Tesippp, col="green")

dev.off()
