# R_code_esame.R
# Riepilogo di tutti i codici scritti finora

# Copernicus data: https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

# 1. R_code_primocod.r   
# 2. R_code_spatial.r   
# 3. R_code_spatial_2.r
# 4. R_code_point_pattern.r  
# 5. R_code_teleril.r   
# 6. R_code_landcover.r   
# 7. R_code_multitemp.r   
# 8. R_code_multitemp_NO2.r   
# 9. R_code_snow.r   
# 10. R_code_patches.r   


#############################################
#############################################



### 1. R_code_primocod.R

# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO
library(sp)
# in alternativa require(sp)

data(meuse)
meuse

# head(meuse) visualizza solo le prime sei righe
head(meuse)
names(meuse)

# dà sommario del dataset:
summary(meuse)

# mostra grafico di correlazione tra variabili in ballo:
pairs(meuse)

# permette di visualizzare solo le variabili di nostro interesse
# occorre fare la tilde "~", che significa "="
# virgola è separatore di argomenti a funzione
pairs(~ cadmium + copper + lead , data = meuse)

# in R si possono richiamare funzioni precedenti: freccia in alto e la premo quante volte mi serve
# lo uso per richiamare "names(meuse)"
# Esercizio: rifaccio pairs mettendo cadmium, copper, lead e zinc
pairs(~ cadmium + copper + lead + zinc , data = meuse)

# permette di ridurre i passaggi (richiamo "names" ecc.) e inserire subito variabili che ci interessano attr. colonne
# in questo caso prendo un subset ("[]") partendo da (",") colonna 3 a colonna 6 (":")
# ci dà grafico uguale a prima
pairs(meuse[,3:6])

# per cambiare colore funzioni: "col="red" " o altri colori
pairs(meuse[,3:6], col="blue")

# per cambiare tipo di caratteri uso "point character"
# ne esistono vari tipi: su "Google immagini" ci sono figure e numeri relativi
pairs(meuse[,3:6], col="blue", pch=20)

# per cambiare dimensione punti uso argomento a funzione "character exageration"
# se cex=1 non cambia nulla, con 10 è 10 volte più grande, con 0.5 diminuisce della metà
pairs(meuse[,3:6], col="blue", pch=20, cex=3)

# do titolo a grafico: "main="
pairs(meuse[,3:6], col="blue", pch=20, cex=3, main="Primo pairs")

# Esercizio: inserire anche "elevation" tra le variabili, che è la settima
pairs(meuse[,3:7], col="blue", pch=20, cex=3, main="Primo pairs")

# source permette di prendere file dall'esterno
# ora facciamo copia-incolla
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(0, 1, 0, 1))
    r1=cor(x,y,use="pairwise.complete.obs")
    r <- abs(cor(x, y,use="pairwise.complete.obs"))


    txt <- format(c(r1, 0.123456789), digits=digits)[1]
    txt <- paste(prefix, txt, sep="")
    if(missing(cex.cor)) cex <- 0.9/strwidth(txt)
    text(0.5, 0.5, txt, cex = cex * r)
}

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"),
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter),
            col = 1, ...)
}

panel.histograms <- function(x, ...)
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}
# "lowess" è smooter locale: uso linea per mostrare relazione tra variabili
# "histograms" fa istogramma variabili

# "lower.panel" è parte inferiore del grafico pairs e decido cosa metterci, ad es. le correlazioni
# "upper.panel" è parte superiore e ci metto lo smoothing, ossia il grafico dei punti con le linee di correlazione
# "diag.panel" è diagonale e ci metto gli istogrammi
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

# Esercizio: in pairs metto lower panel smoothing, diagonal istogrammi e upper correlations
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)

plot(meuse$cadmium, meuse$copper)
# mi dà errore

# uso questa funzione per evitare di usare "$" e di richiamare "meuse"
attach(meuse)
plot(cadmium, copper)

# Cambio il tipo di punto utilizzato, colore e titolo
plot(cadmium, copper, pch=19)
plot(cadmium, copper, pch=19, col="green", main="Primo plot")
# con "yellow" non si vede nulla: bocciato

# voglio cambiare le lables, le etichette
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame")

# argomento che esagera i caratteri delle lables
# e cambio punti
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame", cex.lab=1.5, cex=2)


#############################################
#############################################


### 2. R_code_spatial.R

# R spatial: funzioni spaziali

# richiamo pacchetto "sp"
library(sp)

# dati pacchetto: "meuse"
data(meuse)
head(meuse)

# plot cadmium e lead
# per fare i plot occorre innanzitutto allegare il database (o dataframe)
attach(meuse)
plot(cadmium, lead, col="pink", pch=8, cex=2)

# Esercizio: plot di copper e zinc con simbolo triangolo (17) e colore verde
plot(copper, zinc, col="green", pch=17, cex=2)

# cambio anche le etichette (lables)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# creo un pannello che contenga più grafici:
# funzione "multiframe" (o "multipanel") è IMPORTANTISSIMA!
# "par" ha come argomenti i multiframe relativi a righe e colonne (row)
# "c" permette di inserire il numero di righe e colonne che vogliamo
# inserisco i due grafici che ho fatto
# metto in R le tre righe insieme!
par(mfrow=c(1,2))
plot(cadmium, lead, col="pink", pch=8, cex=2, xlab="cadmio", ylab="piombo", cex.lab=1.5)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# invertiamo i grafici riga-colonna in colonna-riga
# innanzitutto inverto numeri riga e colonna
par(mfrow=c(2,1))
plot(cadmium, lead, col="pink", pch=8, cex=2, xlab="cadmio", ylab="piombo", cex.lab=1.5)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# c'è pacchetto utile, un multiframe automatico: "GGally"
# lo installo e lo richiamo
install.packages("GGally")
library(GGally)

# faccio multipannello con tante variabili
# uso una funzione interna a GGally
# scelgo un subset (un pezzettino), perché altrimenti è troppo grande:
# seleziono solo le prime quattro colonne, relative agli elementi (dalla terza alla sesta)
# "3:6" si può anche scrivere "3-6"
ggpairs(meuse[,3:6])

# ci dà un grafico con distribuzione di frequenza dati
# mette plot variabili: piombo rispetto alle altre ecc.
# probabilmente usa coefficiente di Spearman, ossia coeff. varia da -1 a 1
# 1: correlate in maniera positiva; -1: corr. negativamente; 0: non correlate
# per vedere come funzione è scritta basta digitarla e premere invio

# facciamo parte spaziale (!!)
# per prima cosa dobbiamo dirgli che "meuse" ha coordinate: "x" e "y"
# la funzione usa "=" per fare l'associazione
head(meuse)
coordinates(meuse)=~x+y

# mettiamo funzione "plot" e ottengo distribuzione nello spazio dei nostri dati
plot(meuse)

# metto grafico all'interno di "sp"
# variabili vanno scritte tra virgolette, perché fa link dall'esterno
spplot(meuse, "zinc")

# abbiamo creato il nostro primo grafico spaziale!!! :D
# zona centrale è un fiume (infatti c'è un meandro): fiume è molto inquinato e c'è presenza di zinco
# zinco si disperde velocemente
# ma vicino all'acqua valori sono molto alti


#############################################
#############################################


### 3. R_code_spatial_2.R

# Continuazione R spatial

library(sp)
data(meuse)
head(meuse)

coordinates(meuse)=~x+y

spplot(meuse, "zinc")

# Esercizio: fare "spplot" con il rame
spplot(meuse, "copper")

# lo stesso plottaggio si può fare con "bubble"
# il risultato è più bello!
bubble(meuse, "zinc")

# Esercizio: fare "bubble" del rame in rosso
bubble(meuse, "copper", col="red")

# foraminiferi (Sofia) e carbon capture (Matteo)
# "c" è un array (o vettore)
# al vettore occorre dare un nome, tramite "<-"
# abbiamo creato un oggetto
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# vedo se sono correlati
plot(foram, carbon, col="green", cex=2, pch=19)

# scarico dati e tabelle da iol (è in .csv)
# sono dati dall'esterno sul covid-19
# lo prendo dalla cartella "lab"
# -> percorso/lab (N.B.!!!)
setwd("/Users/enricopriarone/lab")

# leggo la tabella
# occorre spiegare a R che la prima riga contiene i titoli ("header")
# head=T o head=TRUE (sempre in maiuscolo!!!), perché l'header c'è!
# Al contrario dovrei mettere FALSE (o F)
# associo la funzione a "covid" con "<-"
covid <- read.table("covid_agg.csv", head=T)


#############################################
#############################################


### 4. R_code_point_patterns.R

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


#############################################
#############################################


### 5. R_code_teleril.R

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

dev.off()


#############################################
#############################################


### 6. R_code_landcover.R

# R code land cove

setwd("/Users/enricopriarone/lab")
load(".RData")
ls()
library(raster)

# Uso funzione che impila e importa i dati
p224r63_2011 <- brick("p224r63_2011_masked.grd")
install.packages("RStoolbox")
library(RStoolbox)

# Faccio RGB
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# Accorpa i pixel in quattro classi:
# Ottengo un vero e proprio modello
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# Visualizzo le informazioni
p224r63_2011c

# Plotto la mappa
plot(p224r63_2011c$map)

# Stabiliamo noi una legenda
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

# Provo con due classi
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
clclass2 <- colorRampPalette(c('red', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass2)
dev.off()

# In funzione del numero di classi aumenta l'incertezza dell'algoritmo auomatico di classificazione
# riportando potenzialmente classi leggermente differenti
# Con 2 classi l'incertezza è più bassa che con 4


#############################################
#############################################

7. R_code_multitemp.R

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


#############################################
#############################################

8. R_code_multitemp_NO2.R

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
boxplot(EN, horizontal=T,outline=F)

dev.off()


#############################################
#############################################

9. R_code_snow.r

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


#############################################
#############################################


10. R_code_patches.R

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

