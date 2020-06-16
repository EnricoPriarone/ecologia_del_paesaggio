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
# 11. R_code_crop.r
# 12. R_code_sdm.r


###################################################################################################
###################################################################################################
###################################################################################################



### 1. R_code_primocod.R

# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO
library(sp)
# EP: in alternativa require(sp)

data(meuse)
meuse

# EP: head(meuse) visualizza solo le prime sei righe
head(meuse)
names(meuse)

# EP: dà sommario del dataset:
summary(meuse)

# EP: mostra grafico di correlazione tra variabili in ballo:
pairs(meuse)

# EP: permette di visualizzare solo le variabili di nostro interesse
# EP: occorre fare la tilde "~", che significa "="
# EP: virgola è separatore di argomenti a funzione
pairs(~ cadmium + copper + lead , data = meuse)

# EP: in R si possono richiamare funzioni precedenti: freccia in alto e la premo quante volte mi serve
# EP: lo uso per richiamare "names(meuse)"
# Esercizio: rifaccio pairs mettendo cadmium, copper, lead e zinc
pairs(~ cadmium + copper + lead + zinc , data = meuse)

# EP: permette di ridurre i passaggi (richiamo "names" ecc.) e inserire subito variabili che ci interessano attr. colonne
# EP: in questo caso prendo un subset ("[]") partendo da (",") colonna 3 a colonna 6 (":")
# EP: ci dà grafico uguale a prima
pairs(meuse[,3:6])

# EP: per cambiare colore funzioni: "col="red" " o altri colori
pairs(meuse[,3:6], col="blue")

# EP: per cambiare tipo di caratteri uso "point character"
# EP: ne esistono vari tipi: su "Google immagini" ci sono figure e numeri relativi
pairs(meuse[,3:6], col="blue", pch=20)

# EP: per cambiare dimensione punti uso argomento a funzione "character exageration"
# EP: se cex=1 non cambia nulla, con 10 è 10 volte più grande, con 0.5 diminuisce della metà
pairs(meuse[,3:6], col="blue", pch=20, cex=3)

# EP: do titolo a grafico: "main="
pairs(meuse[,3:6], col="blue", pch=20, cex=3, main="Primo pairs")

# Esercizio: inserire anche "elevation" tra le variabili, che è la settima
pairs(meuse[,3:7], col="blue", pch=20, cex=3, main="Primo pairs")

# EP: source permette di prendere file dall'esterno
# EP: ora facciamo copia-incolla
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
# EP: "lowess" è smooter locale: uso linea per mostrare relazione tra variabili
# EP: "histograms" fa istogramma variabili

# EP: "lower.panel" è parte inferiore del grafico pairs e decido cosa metterci, ad es. le correlazioni
# EP: "upper.panel" è parte superiore e ci metto lo smoothing, ossia il grafico dei punti con le linee di correlazione
# EP: "diag.panel" è diagonale e ci metto gli istogrammi
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

# Esercizio: in pairs metto lower panel smoothing, diagonal istogrammi e upper correlations
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)

plot(meuse$cadmium, meuse$copper)
# EP: mi dà errore

# EP: uso questa funzione per evitare di usare "$" e di richiamare "meuse"
attach(meuse)
plot(cadmium, copper)

# EP: Cambio il tipo di punto utilizzato, colore e titolo
plot(cadmium, copper, pch=19)
plot(cadmium, copper, pch=19, col="green", main="Primo plot")
# EP: con "yellow" non si vede nulla: bocciato

# EP: voglio cambiare le lables, le etichette
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame")

# EP: argomento che esagera i caratteri delle lables
# EP: e cambio punti
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame", cex.lab=1.5, cex=2)


###################################################################################################
###################################################################################################
###################################################################################################


### 2. R_code_spatial.R

# R spatial: funzioni spaziali

# EP: richiamo pacchetto "sp"
library(sp)

# EP: dati pacchetto: "meuse"
data(meuse)
head(meuse)

# EP: plot cadmium e lead
# EP: per fare i plot occorre innanzitutto allegare il database (o dataframe)
attach(meuse)
plot(cadmium, lead, col="pink", pch=8, cex=2)

# Esercizio: plot di copper e zinc con simbolo triangolo (17) e colore verde
plot(copper, zinc, col="green", pch=17, cex=2)

# EP: cambio anche le etichette (lables)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# EP: creo un pannello che contenga più grafici:
# EP: funzione "multiframe" (o "multipanel") è IMPORTANTISSIMA!
# EP: "par" ha come argomenti i multiframe relativi a righe e colonne (row)
# EP: "c" permette di inserire il numero di righe e colonne che vogliamo
# EP: inserisco i due grafici che ho fatto
# EP: metto in R le tre righe insieme!
par(mfrow=c(1,2))
plot(cadmium, lead, col="pink", pch=8, cex=2, xlab="cadmio", ylab="piombo", cex.lab=1.5)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# EP: invertiamo i grafici riga-colonna in colonna-riga
# EP: innanzitutto inverto numeri riga e colonna
par(mfrow=c(2,1))
plot(cadmium, lead, col="pink", pch=8, cex=2, xlab="cadmio", ylab="piombo", cex.lab=1.5)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# EP: c'è pacchetto utile, un multiframe automatico: "GGally"
# EP: lo installo e lo richiamo
install.packages("GGally")
library(GGally)

# EP: faccio multipannello con tante variabili
# EP: uso una funzione interna a GGally
# EP: scelgo un subset (un pezzettino), perché altrimenti è troppo grande:
# EP: seleziono solo le prime quattro colonne, relative agli elementi (dalla terza alla sesta)
# EP: "3:6" si può anche scrivere "3-6"
ggpairs(meuse[,3:6])

# EP: ci dà un grafico con distribuzione di frequenza dati
# EP: mette plot variabili: piombo rispetto alle altre ecc.
# EP: probabilmente usa coefficiente di Spearman, ossia coeff. varia da -1 a 1
# EP: 1: correlate in maniera positiva; -1: corr. negativamente; 0: non correlate
# EP: per vedere come funzione è scritta basta digitarla e premere invio

# EP: facciamo parte spaziale (!!)
# EP: per prima cosa dobbiamo dirgli che "meuse" ha coordinate: "x" e "y"
# EP: la funzione usa "=" per fare l'associazione
head(meuse)
coordinates(meuse)=~x+y

# EP: mettiamo funzione "plot" e ottengo distribuzione nello spazio dei nostri dati
plot(meuse)

# EP: metto grafico all'interno di "sp"
# EP: variabili vanno scritte tra virgolette, perché fa link dall'esterno
spplot(meuse, "zinc")

# EP: abbiamo creato il nostro primo grafico spaziale!!! :D
# EP: zona centrale è un fiume (infatti c'è un meandro): fiume è molto inquinato e c'è presenza di zinco
# EP: zinco si disperde velocemente
# EP: ma vicino all'acqua valori sono molto alti


###################################################################################################
###################################################################################################
###################################################################################################


### 3. R_code_spatial_2.R

# Continuazione R spatial

library(sp)
data(meuse)
head(meuse)

coordinates(meuse)=~x+y

spplot(meuse, "zinc")

# Esercizio: fare "spplot" con il rame
spplot(meuse, "copper")

# EP: lo stesso plottaggio si può fare con "bubble"
# EP: il risultato è più bello!
bubble(meuse, "zinc")

# Esercizio: fare "bubble" del rame in rosso
bubble(meuse, "copper", col="red")

# EP: foraminiferi (Sofia) e carbon capture (Matteo)
# EP: "c" è un array (o vettore)
# EP: al vettore occorre dare un nome, tramite "<-"
# EP: abbiamo creato un oggetto
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# EP: vedo se sono correlati
plot(foram, carbon, col="green", cex=2, pch=19)

# EP: scarico dati e tabelle da iol (è in .csv)
# EP: sono dati dall'esterno sul covid-19
# EP: lo prendo dalla cartella "lab"
# EP: -> percorso/lab (N.B.!!!)
setwd("/Users/enricopriarone/lab")

# EP: leggo la tabella
# EP: occorre spiegare a R che la prima riga contiene i titoli ("header")
# EP: head=T o head=TRUE (sempre in maiuscolo!!!), perché l'header c'è!
# EP: Al contrario dovrei mettere FALSE (o F)
# EP: associo la funzione a "covid" con "<-"
covid <- read.table("covid_agg.csv", head=T)


###################################################################################################
###################################################################################################
###################################################################################################


### 4. R_code_point_patterns.R

# 31/03
# Codice per analisi delle strutture relazionate ai punti nello spazio (point patterns)

# EP: Installo tutti i pacchetti necessari
install.packages("raster")
install.packages("rgdal")
install.packages("ggplot2")
install.packages("spatstat")

# EP: Richiamo file con dati Covid
setwd("/Users/enricopriarone/lab")

# EP: Importo dati tabella
covid <- read.table("covid_agg.csv", head=T)
head(covid)

# EP: Facciamo un plot per visualizzare la distribuzione dei dati
# EP: Dollaro serve a relazionare ogni colonna al proprio dataset
# EP: Altrimenti uso "attach(covid)" e poi "plot(country, cases)"
plot(covid$country, covid$cases)

# EP: Grafico si vede male, allora lo rendiamo verticale
# EP: Provo con lable=0 -> non è cambiato nulla (perché è il default) -> è parallel
plot(covid$country, covid$cases, las=0) # EP: parallel lables

# EP: Con las=1 ho orizzontale ("horizontal")
# EP: Con las=2 labels sono perpendicolari
# EP: Con las=3 sono tutte verticali
plot(covid$country, covid$cases, las=1) # EP: horizontal lables
plot(covid$country, covid$cases, las=2) # EP: perpendicular lables
plot(covid$country, covid$cases, las=3) # EP: vertical lables

# EP: Diminuisco grandezza punti con "cex.axis=0.3"
plot(covid$country, covid$cases, las=3, cex.axis=0.3)

# EP: Faccio plot tramite ggplot
library(ggplot2) # EP: o require(ggplot2)

# EP: Visualizzo dati in ggplot
data(mpg)
head(mpg)

# EP: dichiaro data (mpg)
# EP: diachiaro aes (aesthetics)
# EP: dichiaro tipo di geometria attraverso un'altra micro funzione
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()

# EP: provo a trasformare in grafico per linee
# EP: provo anche con i poligoni
ggplot(mpg, aes(x=displ, y=hwy)) + geom_line()
ggplot(mpg, aes(x=displ, y=hwy)) + geom_polygon()

# EP: Faccio lo stesso con i dati su Covid già caricati
# EP: Prima metto i dati ("covid"), poi le aes (le cerco con "names" o "head" -> scelgo "lon", "lat" e i "cases" per grandezza)
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()

# EP: Richiamo "spatstat"
library(spatstat)

# EP: Uso applicazione point pattern e la collego a dati Covid
# EP: Metto lon e lat e i range di x(-180 ≤ lon ≤ 180) e y(-90 ≤ lat ≤ 90)
attach(covid)
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

# EP: Faccio funzione density, ossia la densità del set spaziale sul Covid
# EP: e ne faccio il grafico
d <- density(covids)
plot(d)

# EP: Inserisco nel grafico punti di origine e i contorni dei Paesi
points(covids)

# EP: Salvo codice R su file come .rdata
q()


# 01/04
# EP: Richiamo la cartella "lab" (dove ho anche .Rdata)
# EP: Ricarico i dati di ieri richiamando .Rdata
setwd("/Users/enricopriarone/lab")
load("point_pattern.RData") # EP: Così ho caricato il file!

# EP: Per visualizzare i file:
ls()

# EP: Carico "spatstat"
library(spatstat)

# EP: Faccio grafico sulla densità
# EP: e cambio un po' le cose
# EP: ad esempio creando una palette di colori, tramite un array (con "c")
# EP: Occhio: in questa funzione usare virgolette singole!
# EP: Metto il numero di livelli di colore che voglio, aggiungendo tra parentesi il numero
plot(d)
cl <- colorRampPalette(c('yellow', 'orange', 'red'))(100) # EP: Funzione importantissima!
plot(d, col=cl)

# Esercizio: plot con colori da verde a blu
cl2 <- colorRampPalette(c('green', 'dark green', 'light blue', 'blue'))(200)
plot(d, col=cl2)

# EP: Aggiungo i dati (punti) del Covid
# EP: e cambio al colore ai punti in vista del grafico finale
points(covids, col="red")

# EP: Carico i confini dei Paesi attraverso "coastlines" con risoluz. 10 m
# EP: Cartella contiene vari file, in particolare lo shapefile
# EP: Associo il file alla funzione file vettoriali
# EP: Se dà errore è perché non ho caricato la libreria "rgdal": la carico prima di "coastlines"
library(rgdal) # EP: Permette in tutti i software e sistemi operativi di leggere raster e vettori
coastlines <- readOGR("ne_10m_coastline.shp")

# EP: Faccio plot coastlines e lo aggiungo al plot precedente con "add"
plot(coastlines, add=T)

# EP: Più avanti andremo a cartografare anche il numero di casi!

# Esercizio: plot mappa di densità con nuova colorazione e aggiunta coastlines
cl3 <- colorRampPalette(c('light blue', 'green', 'dark green', 'blue'))(400)
plot(d, col=cl3)
plot(coastlines, add=T)


# 22/04
# EP: Carico "point_pattern.RData"
setwd("/Users/enricopriarone/lab")
load("point_pattern.RData")
ls()

# Esercizio: cartografare densità dei punti
# EP: Per farlo riprendo comandi della volta scorsa e li aggiusto per i miei obiettivi
library(spatstat)
library(rgdal)
plot(d)
cl <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(d, col=cl)
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# EP: Andiamo a vedere il numero di casi con l'interpolazione,
# EP: che stima i numeri nelle zone in cui non è stato fatto il campionamento
# EP: Con funzione "marks" andiamo a prendere i dati nel point pattern "covids"
# EP: Il dollaro ci serve per non fare l'"attach"
head(covid)
marks(covids) <- covid$cases

# EP: Chiamo "s" la stima dei valori attraverso la funzione di smooth
s <- Smooth(covids)
plot(s)

# EP: Rifare plot di s, cambiando palette e aggiungendo punti e coastlines
cls <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(s, col=cls)
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T, main="Interpolazione")
# EP: Si vede che mettendo il focus sul numero di casi l'Asia a febbraio era la più colpita

# EP: Con "text" vediamo anche valori dei punti
text(covids) # EP: Ma questa volta ci dà l'ID, non il valore dei casi

# EP: Facciamo una carta finale con entrambi i plot
# EP: Usiamo un multiframe
par(mfrow=c(2,1))
# EP: densità:
cl <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(d, col=cl, main="Densità")
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)
# EP: interpolazione:
cls <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(s, col=cls, main="Interpolazione")
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T)

# EP: Chiudo il grafico
dev.off()

# San Marino, caso di studio
# EP: Carico i dati esterni ("Tesi.RData") che ho scaricato da iol
# EP: Serve libreria spatstat
# EP: library(spatstat)
load("Tesi.RData")
ls()
head(Tesi)

# EP: Facciamo un attach della tabella per usarla con il point pattern
attach(Tesi)

# EP: Point pattern: x, y, c(xmin,xmax), c(ymin,ymax)
# EP: Mettiamo anche i limiti per le due coordinate, trattandosi di un'area ristretta
# EP: Uso funzione summary(dataset) per avere il sommario della tabella
summary(Tesi)
# EP: y(lat.) va da 43.91 a 43.94, y(lon.) da 12.42 a 12.46
# EP: Ma noi lasciamo un po' di margine
Tesippp <- ppp(Longitude, Latitude, c(12.41, 12.47), c(43.9, 43.95))

# EP: Facciamo densità punti e la graficizziamo
dT <- density(Tesippp)
plot(dT)
points(Tesippp)
# EP: Nella parte centrale c'è densità più alta

dev.off()

# 28/04
setwd("/Users/enricopriarone/lab")
load("Tesi.RData")
ls()
# EP: Ottengo i file presenti:
# EP: "dT" è density map di Tesippp
# EP: "Tesi" è un dataset
# EP: "Tesippp" è il point pattern del file "Tesi" (da libreria "Spatstat")

# EP: Associamo i valori che vogliamo stimare
library(spatstat)
plot(dT)
points(Tesippp, col="green")

# EP: Andiamo a stimare la ricchezza specifica
# EP: Con "head" vedo che si trova sotto "Species_richness"
# EP: "marks" va a prendere i valori dalla tabella e li associa ai punti del ppp
head(Tesi)
marks(Tesippp) <- Tesi$Species_richness

# EP: Creo mappa e l'associo a Smooth
interpol <- Smooth(Tesippp)

# EP: Usiamo file su San Marino
# EP: Carico libreria "rgdal"
library(rgdal)
sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T) # EP: Importante! Con "T"/"True" aggiunge nuova mappa a quella precedente
points(Tesippp,col="green")
# EP: Mappa va a sovrapporsi al territorio di San Marino
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


###################################################################################################
###################################################################################################
###################################################################################################


### 5. R_code_teleril.R

# Codice R per analisi di immagini satellitari

# EP: Installo pacchetti che servono o li richiamo
install.packages("RStoolbox") # EP: Verifica
library(raster)
library(RStoolbox) # EP: Verifica

# EP: Richiamo la setwd
setwd("/Users/enricopriarone/lab")

# EP: Uso p224r63_2011_masked.grd
# EP: Associamo il file alla funzione "brick", per importare immagine satellitare con tutte le bande
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# EP: Cerco informazioni sull'immagine attraverso grafici su bande, riflettanze ecc.
plot(p224r63_2011)

# EP: Ci appare stessa immagine in 7 bande, che per LANDSAT sono sempre le stesse:
# EP: 1) blu;
# EP: 2) verde;
# EP: 3) rosso;
# EP: 4) NIR;
# EP: 5) infr. medio; 
# EP: 6) infr. termico;
# EP: 7) infr. medio.

# EP: Salvo RData

# EP: Richiamo RData
setwd("/Users/enricopriarone/lab")
load(".RData")
ls()
library(raster)

plot(p224r63_2011)

# EP: Cambio colorazione in scala di grigi
cl <- colorRampPalette(c('black','grey','light grey'))(100)
plot(p224r63_2011, col=cl)
names(p224r63_2011) # EP: Visualizza le sette bande che sta usando

# EP: Faccio una palette con la scala del blu
# EP: attach(dataframe) non funziona con i raster
# EP: allora per legare immagine a banda del blu uso simbolo che lega la colonna (banda) al dataset (immagine satellitare): "$"
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
# EP: parte bianca è mascherata: i pixel sono stati eliminati perché non servivano

# Esercizio: plottare banda del NIR con:
# EP: colorRampPalette che varia dal rosso all'arancione, al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# EP: creo multiframe
# EP: uso "par" per usare finestra a blocchi
par(mfrow=c(2,2))

# EP: Inizio col blu, poi verde ecc.
# EP: blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)

# EP: green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_2011$B2_sre, col=clg) # EP: è intermedio tra verde e NIR

# EP: red
clr <- colorRampPalette(c('dark red','red','orange'))(100)
plot(p224r63_2011$B3_sre, col=clr) # EP: Sono bassi perché quasi tutto rosso è assorbito per fotosintesi

# EP: NIR
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir) # EP: Valori di NIR sono molto alti -> buona parte è foresta

# EP: Montiamo le bande insieme per fare immagine come la vedrebbe l'occhio umano (con funz. "plotRGB")
# EP: Usiamo le 3 componenti: R=banda del rosso G=verde B=blu (sono disponibili solo 3 bande tutte insieme)
# EP: Chiudiamo prima la finestra plottata
dev.off()
plotRGB(p224r63_2011, r=3, g=2, b=1)

# EP: Stiro un po' i colori per vederlo meglio
# EP: Quello lineare è il più usato
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# EP: Usiamo anche il NIR per distinguere ombre da piante
# EP: Ma sono disponibili solo 3 bande alla volta: occorre eliminarne una
# EP: Elimino la banda blu scalando tutto di uno: r=4, g=3, b=2 )
# EP: È chiamata "432" o "false colours" ("falsi colori")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# EP: Salvo il grafico in PDF
# EP: Per png: png"primografico.pdf")
pdf("primografico.pdf")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# EP: Faccio multiframe con immagini a colori reali e imm. a colori falsati
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
dev.off()

# Esercizio: metto NIR in componente green
plotRGB(p224r63_2011, r=3, g=4, b=2, stretch="Lin")
# EP: Abbiamo fatto in modo che zona boschiva si veda molto bene in verde acceso

# EP: Ora lo metto nel blu
plotRGB(p224r63_2011, r=3, g=2, b=4, stretch="Lin")
dev.off()

# 15/04
# EP: Richiamo le librerie, la working directory e il file .RData
library(raster)
setwd("/Users/enricopriarone/lab")
load(".RData")
ls()

# EP: Importo file del 1988 con tutte le bande
# EP: "grd" sta per griglia: formato è di righe e colonne
p224r63_1988 <- brick("p224r63_1988_masked.grd")

# EP: Facciamo un plot dell'immagine
plot(p224r63_1988)

# EP: Rifacciamo il lavoro dell'altra volta con l0immagine del 1988
par(mfrow=c(2,2))

# EP: Inizio col blu, poi verde ecc.
# EP: blue
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_1988$B1_sre, col=clb)

# EP: green
clg <- colorRampPalette(c('dark green','green','light green'))(100)
plot(p224r63_1988$B2_sre, col=clg) # EP: è intermedio tra verde e NIR

# EP: red
clr <- colorRampPalette(c('dark red','red','orange'))(100)
plot(p224r63_1988$B3_sre, col=clr) # EP: Sono bassi perché quasi tutto rosso è assorbito per fotosintesi

# EP: NIR
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_1988$B4_sre, col=clnir) # EP: Valori di NIR sono molto alti -> buona parte è foresta

# EP: Faccio plot con le tre bande principali
# EP: B1: blu;
# EP: B2: verde;
# EP: B3: rosso;
# EP: B4: NIR;
# EP: B5: infr. medio; 
# EP: B6: infr. termico;
# EP: B7: infr. medio.
plotRGB(p224r63_1988, r=3, g=2, b=1, stretch="Lin")

# Esercizio: fare plot dell'immagine usando il NIR nella componente "r"
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # EP: Già che ci sono sposto tutte le bande di 1: da 4-2-1 a 4-3-2

# EP: Faccio plot delle due immagini (1988 e 2011)
# EP: Devo fare un multipanel: funz. "par", con "mfrow"
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# EP: Confronto: agricolo è molto più sviluppato nel 2011

dev.off()

# EP: Creo un indice per verificare le differenze
# EP: DVI=NIR-RED, e questo si fa pixel per pixel
# EP: Indice spettrale DVI
# EP: dvi_1988 = nir1988-red1988
# EP: uso il $ per collegare le bande al dataset
dvi_1988 <- p224r63_1988$B4_sre - p224r63_1988$B3_sre
plot(dvi_1988)

# Esercizio: calcolare DVI del 2011
dvi_2011 <- p224r63_2011$B4_sre - p224r63_2011$B3_sre
plot(dvi_2011)

# EP: Provo altri colori
cldvi <- colorRampPalette(c('light blue','light green','green'))(100)
plot(dvi_2011, col=cldvi)

# EP: Faccio differenza nel tempo tra i due indici: multitemporal analysis
# EP: Valore è positivo se vegetazione è in buone condizioni nel 2011 rispetto al 1988
# EP: Viceversa, se è negativo ora sta peggio
difdvi <- dvi_2011 - dvi_1988
plot(difdvi)

# EP: Cambio i colori:
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvi, col=cldifdvi)

# EP: Faccio par multiframe per vedere le tre immagini
par(mfrow=c(3,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plot(difdvi, col=cldifdvi)
dev.off()

# EP: Provo a cambiare la risoluzione delle immagini
# EP: con pixel più grandi, dunque a risoluzione minore
# EP: La riduco di n volte: se pixel è di 30 m, fact=10 mi dà nuovo pixel di 300 m
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

# EP: Guardo caratteristiche immagini
# EP: imm. originale ha resolution 30x30
# EP: nuova 300x300 e righe sono meno
p224r63_2011
p224r63_2011lr

# EP: Faccio grafico
par(mfrow=c(2,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")

# EP: Riduco ancora
p224r63_2011lr50 <- aggregate(p224r63_2011, fact=50)
p224r63_2011lr50

par(mfrow=c(3,1))
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011lr50, r=4, g=3, b=2, stretch="Lin")
dev.off()

# EP: Faccio dvi a bassa risoluzione
dvi_2011lr50 <- p224r63_2011lr50$B4_sre - p224r63_2011lr50$B3_sre
plot(dvi_2011lr50) # Ma con questo perdo tantissimo e sembra tutto omogeneo

# EP: Faccio lo stesso per il 1988
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi_1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

# EP: Faccio difdvi a bassa risoluzione
difdvilr50 <- dvi_2011lr50 - dvi_1988lr50
cldifdvi <- colorRampPalette(c('red','white','blue'))(100)
plot(difdvilr50,col=cldifdvi)

# EP: multiframe
par(mfrow=c(2,1))
plot(difdvi, col=cldifdvi)
plot(difdvilr50, col=cldifdvi)
# EP: Noto dalla differenza che nella prima immagine posso individuare le micro disomogeneità

dev.off()


###################################################################################################
###################################################################################################
###################################################################################################


### 6. R_code_landcover.R

# R code land cove

setwd("/Users/enricopriarone/lab")
load(".RData")
ls()
library(raster)

# EP: Uso funzione che impila e importa i dati
p224r63_2011 <- brick("p224r63_2011_masked.grd")
install.packages("RStoolbox")
library(RStoolbox)

# EP: Faccio RGB
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# EP: Accorpa i pixel in quattro classi:
# EP: Ottengo un vero e proprio modello
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=4)

# EP: Visualizzo le informazioni
p224r63_2011c

# EP: Plotto la mappa
plot(p224r63_2011c$map)

# EP: Stabiliamo noi una legenda
clclass <- colorRampPalette(c('red', 'green', 'blue', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass)

# EP: Provo con due classi
p224r63_2011c <- unsuperClass(p224r63_2011, nClasses=2)
plot(p224r63_2011c$map)
clclass2 <- colorRampPalette(c('red', 'black'))(100) 
plot(p224r63_2011c$map, col=clclass2)
dev.off()

# EP: In funzione del numero di classi aumenta l'incertezza dell'algoritmo auomatico di classificazione
# EP: riportando potenzialmente classi leggermente differenti
# EP: Con 2 classi l'incertezza è più bassa che con 4


###################################################################################################
###################################################################################################
###################################################################################################

### 7. R_code_multitemp.R

# R code analisi multitemporale di variazione della land cover
# EP: Copertura del suolo viene da telerilevazioni

setwd("/Users/enricopriarone/lab")
install.packages("Rcmdr")
library(raster)
library(RStoolbox)
library(ggplot2)

# EP: Carichiamo immagine satellitare da cartella "lab" attraverso funzione di raster "brick"
defor1 <- brick("defor1_.jpg")
defor2 <- brick("defor2_.jpg")

# EP: defor1_.1 = NIR
# EP: defor1_.2 = red
# EP: defor1_.3 = green
# EP: Associamo banda rosso a NIR, verde a rosso e blu a verde
plotRGB(defor1, r=1, g=2, b=3, stretch="Lin")
plotRGB(defor2, r=1, g=2, b=3, stretch="Lin")

# EP: Andiamo a usare due classi non supervisionate: non spieghiamo al computer la divisione
# EP: Computer raggruppa pixel che sembrano simili tra loro
d1c <- unsuperClass(defor1, nClasses=2)
d1c # EP: Visualizzo i suoi dettagli

# EP: Creo mappa
plot(d1c$map)
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c$map, col=cl)

# EP: Esempio su significato dollaro
# EP: mappageologica <- geomap(im_sat, nClasees=...)
# EP: plot (mappageologica$lito)
# EP: plot(mappageologica$lineaments)

# Esercizio: classificare con due classi l'immagine satellitare defor2
d2c <- unsuperClass(defor2, nClasses=2)
d2c
plot(d2c$map)
plot(d2c$map, col=cl)

dev.off()

# EP: Faccio nuovo grafico delle due carte ottenute
par(mfrow=c(1,2))
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)

# EP: Visualizzo come sono stati suddivisi i pixel nelle classi
freq(d1c$map)
# EP: aree altre: 35516
# EP: foresta: 305776

totd1 <- 305776 + 35516 # EP: Numero pixel carta
totd1
# EP: 341292

# EP: Calcolo le proporzioni, la percentuale delle frequenze
percent1 <- freq(d1c$map)*100/totd1
percent1

# EP: percentuali:
# EP: aree altre: 10.4
# EP: foreste: 89.6

# EP: Idem per la carta 2
freq(d2c$map)
# EP: aree altre: 164321
# EP: foreste: 178405

totd2 <- 164321 + 178405 # EP: Numero pixel carta
totd2
# EP: 342726

percent2 <- freq(d2c$map)*100/totd2
percent2

# EP: aree altre: 48
# EP: foreste: 52

# EP: Creo dataframe con i dati
cover <- c("Agriculture","Forest")
before <- c(10.4,89.6)
after <- c(48,52)
output <- data.frame(cover,before,after)
View(output)

dev.off()

# 05/05
setwd("/Users/enricopriarone/lab")
load("defor.RData") # EP: Uso questo perché il mio ".RData" mi dà errore: i dati saranno un po' diversi
ls()
library(raster)
library(ggplot2)
install.packages("gridExtra")
library(gridExtra)

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100) 
plot(d1c$map, col=cl)
plot(d2c$map, col=cl)
output

# EP: Creo grafico con percentuale di foresta/agricoltura prima della deforestazione ("before")
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
# EP: agricoltura occupa percentuale molto bassa: ca. 10%; contro il ca. 90% della foresta

# Esercizio: faccio lo stesso per il dopo deforestazione ("after")
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")
# EP: agricoltura è vicina al 50%; foresta lo supera appena

# EP: Usiamo funzione "par"
# EP: Necessita del pacchetto "gridExtra", che installiamo
# EP: Facciamo istogramma delle percentuali associando prima i due grafici a un nome
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# Esercizio: usare "grid.arrange" per graficizzarli
grid.arrange(grafico1, grafico2, nrow=1)

# EP: Manca da cambiare la y! Vedi poi "iol"

dev.off()

# 06/05
# EP: Cambiamo la y
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

# EP: "ylim" funziona con "ggplot" e permette di inserire in limite all'ordinata
# EP: Prendere libro "ggplot2. Elegant graphics..." di Whickam!
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

# Esercizio: usa grid.arrange graficizzare
grid.arrange(grafico1, grafico2, nrow = 1)
# EP: Così scala nelle ordinate (y) è la stessa

dev.off()


###################################################################################################
###################################################################################################
###################################################################################################


### 8. R_code_multitemp_NO2.R

# EP: Codice analisi dati NO2 da Sentinel-2 ESA (Copernicus), da gennaio a marzo 2020

setwd("/Users/enricopriarone/lab")
library(raster)

# EP: Visualizzo immagini scaricate
EN01 <- raster("EN_0001.png") # EP: Lo "0" prima è importante perché R non li riconosce come numeri, ma come stringhe:
plot(EN01)                    # EP: altrimenti la successiva sarebbe stata "EN10" e non "EN2"

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
# EP: In alternativa uso un ciclo "for": guardare in codice su "iol"!

ls()

# EP: Visualizziamo le immagini iniziale e finale per vedere le differenze
cl <- colorRampPalette(c('red','orange','yellow'))(100)
par(mfrow=c(1,2))
plot(EN01, col=cl)
plot(EN13, col=cl)
dev.off()

# EP: Graficizzo le differenze
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
# EP: Domani proviamo col "for"

dev.off()

# 06/05
# EP: Cambio la working directory con la nuova cartella
setwd("/Users/enricopriarone/lab/esa_no2")
library(raster)

# EP: creo una lista di pattern, di cui diciamo che sono nominati ".png"
rlist <- list.files(pattern=".png")
rlist

# EP: usiamo funzioni "raster" (non "brick", perché carica l'intero pacchetto layer satellitare: a noi ne serve uno solo)
# EP: e "lapply"
listafinale <- lapply(rlist, raster)
listafinale
# EP: sono 13 RasterLayer

# EP: usiamo funzione "stack" per creare un pacchetto unico di dati: creiamo un'unica immagine
EN <- stack(listafinale)
EN

cl <- colorRampPalette(c('red','orange','yellow'))(100)
plot(EN, col=cl)
# EP: Questo permette di caricare il set completo con tutte le n immagini, senza lavorare su ogni singola

dev.off()

# 12/05
# EP: Grafici e boxplot

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

# EP: Creiamo un boxplot di EN in orizzontale
boxplot(EN, horizontal=T,outline=F)

dev.off()


###################################################################################################
###################################################################################################
###################################################################################################


### 9. R_code_snow.r

# Analisi copertura nevosa con dati Copernicus
# https://land.copernicus.vgt.vito.be/PDF/portal/Application.html

setwd("/Users/enricopriarone/lab")
install.packages("ncdf4")
library(ncdf4)
library(raster)

# EP: Importiamo file .nc
# EP: Esercizio: poi facciamo grafico
snowmay <- raster("c_gls_SCE500_202005180000_CEURO_MODIS_V1.0.1.nc")
cl <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snowmay, col=cl)

# EP: Dobbiamo settare una nuova working directory
setwd("/Users/enricopriarone/lab/snow")

# Esercizio: importiamo l'intera serie di file
rlist = list.files(pattern=".tif", full.names=T)

# EP: Salvo raster in una lista
# EP: con «lappy»
list_rast = lapply(rlist, raster)
snow.multitemp <- stack(list_rast)
plot(snow.multitemp, col=cl)

# EP: Lavoriamo su dati del 2000 e del 2020
par(mfrow=c(1,2))
plot(snow.multitemp$snow2000r, col=cl, zlim=c(0,250)) # EP: «zlim» serve per avere la stessa scala sulle ordinate
plot(snow.multitemp$snow2020r, col=cl, zlim=c(0,250))
dev.off()

# EP: Facciamo la differenza per poi graficizzarla
difsnow = snow.multitemp$snow2020r - snow.multitemp$snow2000r
cldiff <- colorRampPalette(c('blue', 'white', 'red'))(100)
plot(difsnow, col=cldiff)

# EP: Facciamo una previsione
# EP: Carichiamo un'analisi dall'esterno attraverso funzione «source»
source("prediction.r")

# EP: Uso dato finale perché il precedente è troppo pesante
predicted.snow.2025.norm <- raster("predicted.snow.2025.norm.tif")
plot(predicted.snow.2025.norm, col=cl)

dev.off()


#############################################
#############################################


### 10. R_code_patches.R

# EP: Lavoriamo sui patch del paesaggio
# EP: Dopo la variazione dell'area, studiamo la variazione delle patch!

setwd("/Users/enricopriarone/lab")
library(raster)
library(ggplot2)

# EP: Occorre installare «igraph» perché non l'ha scaricato direttamente con libreria «raster»
install.packages("igraph")
library(igraph)

# EP: Carico le immagini
d1c <- raster("d1c.tif")
d2c <- raster("d2c.tif")

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c,col=cl)
plot(d2c,col=cl)
# EP: foresta: classe 2
# EP: agricoltura: classe 1

# EP: Eliminiamo tutti i valori di agricoltura, inserendoli come valori nulli («not assigned»)
# EP: Usiamo funzione «reclassify»
d1c.for <- reclassify(d1c, cbind(1,NA)) # «d1c.for» di solito è nome da modello; in alternativa «d1c_for»

par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d1c,col=cl)
plot(d1c.for, col=cl)

d1c.for
# EP: unico valore è «2»

d2c.for <- reclassify(d2c, cbind(1,NA))
par(mfrow=c(1,2))
cl <- colorRampPalette(c('black','green'))(100)
plot(d2c,col=cl)
plot(d2c.for, col=cl)

# EP: Graficizzo le due nuove carte
par(mfrow=c(1,2))
plot(d1c)
plot(d2c)

# EP: Creiamo i vari patch
# EP: facendo un clump
d1c.for.patches <- clump(d1c.for)
d2c.for.patches <- clump(d2c.for)

# EP: Salvo dati verso l'esterno, ossia su «lab» (creo nuovo file)
writeRaster(d1c.for.patches, "d1c.for.patches.tif")
writeRaster(d2c.for.patches, "d2c.for.patches.tif")

# Esercizio: graficizzare le due mappe una accanto all'altra
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)
par(mfrow=c(1,2))
plot(d1c.for.patches, col=clp)
plot(d2c.for.patches, col=clp)

# EP: Definiamo quante patch sono state create
d1c.for.patches # Ha 301 patches
d2c.for.patches # Ha 1212 patches

# EP: Creiamo un frame
time <- c("1. Before deforestation","2. After deforestation")
npatches <- c(301,1212)
output <- data.frame(time,npatches)
attach(output)
output

# EP: Grafico finale
ggplot(output, aes(x=time, y=npatches, color="red")) + geom_bar(stat="identity", fill="white")
# EP: Si vede chiaramente l'aumento delle patch dopo la deforestazione!

dev.off()


###################################################################################################
###################################################################################################
###################################################################################################


### 11. R_code_crop.r
# Simulazione esame

setwd("/Users/enricopriarone/lab/snow")
library(raster)

# Esercizio: caricare tutte le immagini «snow»

# EP: creo una lista di pattern «snow2», perché è presente anche .tif previsionale
rlist <- list.files(pattern="snow2")
rlist

# EP: uso funzioni "raster" e "lapply"
listasnow <- lapply(rlist, raster)
listasnow
# EP: 6 RasterLayer

# EP: uso funzione "stack" per creare un pacchetto unico di dati: creiamo un'unica immagine
snow.multitemp <- stack(listasnow)
snow.multitemp

clb <- colorRampPalette(c('darkblue','blue','light blue'))(100)
plot(snow.multitemp, col=clb)

# EP: Facciamo zoom: due soluzioni
# EP: Plottiamo un'unica immagine
plot(snow.multitemp$snow2010r, col=clb)

# EP: a) usiamo coordinate
extension <- c(6, 20, 35, 50)
zoom(snow.multitemp$snow2010r, ext=extension)

# EP: b) tramite disegno
plot(snow.multitemp$snow2010r, col=clb)

# EP: Ora facciamo un vero ritaglio, un crop
# EP: Lo facciamo definendo le coordinate
extension <- c(6, 20, 35, 50)
snow2010r.italy <- crop(snow.multitemp$snow2010r, extension)
plot(snow2010r.italy, col=clb)

# Esercizio: applichiamo la funzione «crop» su tutte le immagini, ossia su più livelli
snow.multitemp.italy <- crop(snow.multitemp, extension)
snow.multitemp.italy  # EP: Da qui prendo valori max e min per scala
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))

# EP: Facciamo analisi sullo stack tagliato su Italia
# EP: Facciamo boxplot (ma volendo anche regressione, previsione con «prediction» ecc.)
boxplot(snow.multitemp.italy, horizontal=T,outline=F)

# EP: Vedere file caricato  su iol («prediction.r»?) e nel caso contattare il prof

dev.off()


###################################################################################################
###################################################################################################
###################################################################################################


### 12. R_code_sdm.r

install.packages("sdm")

library(sdm)
library(raster)
library(rgdal)

# EP: specie
file <- system.file("external/species.shp", package="sdm") 
species <- shapefile(file)

species

species$Occurrence

plot(species)

plot(species[species$Occurrence == 1,],col='blue',pch=16)
points(species[species$Occurrence == 0,],col='red',pch=16)

# EP: modello
path <- system.file("external", package="sdm") 

# EP: predizione [?]
lst <- list.files(path=path,pattern='asc$',full.names = T)
lst

preds <- stack(lst) 
preds

cl <- colorRampPalette(c('blue','orange','red','yellow')) (100)
plot(preds, col=cl)

plot(preds$elevation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$temperature, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$precipitation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

plot(preds$vegetation, col=cl)
points(species[species$Occurrence == 1,], pch=16)

# EP: modello

d <- sdmData(train=species, predictors=preds)
d

m1 <- sdm(Occurrence ~ elevation + precipitation + temperature + vegetation, data=d, methods='glm') 
p1 <- predict(m1, newdata=preds)

plot(p1, col=cl)
points(species[species$Occurrence == 1,], pch=16)

dev.off()


###################################################################################################
###################################################################################################
###################################################################################################


### Codice esame
### R_code_ .r

# «The Soil Water Index quantifies the moisture condition at various depths in the soil.
# It is mainly driven by the precipitation via the process of infiltration.
# Soil moisture is a very heterogeneous variable and varies on small scales with soil properties and drainage patterns.
# Satellite measurements integrate over relative large-scale areas, with the presence of vegetation adding complexity to the interpretation.
# The soil moisture, up to 5cm soil depth, is recognized as an Essential Climate Variable (ECV) by the Global Climate Observing System (GCOS).
# The Soil Water Index is provided:
# over Europe, with 1km resolution, based on Surface Soil Moisture from Sentinel-1 C-band SAR (SSM product) and EUMETSAT H SAF Metop ASCAT surface soil moisture.
# at global scale, with 0.1 degree or 12.5km resolution, based on the same Metop ASCAT soil moisture observations».

# «Main application domains: 
# Soil moisture is an important variable in land-atmosphere feedbacks at weather and climate time scales because of its major effect on the partitioning of
# incoming radiation (available energy) into latent and sensible heat and on the allocation of precipitation into runoff, subsurface flow, and infiltration.
# Soil moisture is intimately involved in the feedback between climate and vegetation, since local climate and vegetation both influence soil moisture through evapotranspiration,
# while soil moisture and climate determine the type of vegetation in a region.
# Changes in soil moisture therefore have a serious impact on agricultural productivity, forestry and ecosystem health.
# Then, SWI is used in soil-vegetation-atmosphere transfer schemes to improve the accuracy of general circulation models or to improve the understanding of the feedback between climate and vegetation.
# Soil moisture estimates can also assist gas flux estimates in permafrost regions» (<https://land.copernicus.eu/global/products/swi>).

