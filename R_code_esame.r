# R_code_esame.R
# Riepilogo di tutti i codici scritti finora

# Copernicus data: <https://land.copernicus.vgt.vito.be/PDF/portal/Application.html>

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
# Esame: R_code_Borneo.r


###################################################################################################
###################################################################################################
###################################################################################################



### 1. R_code_primocod.R

# PRIMO CODICE R ECOLOGIA DEL PAESAGGIO
library(sp)
# EP: in alternativa require(sp)

# EP: Funzione utile a visualizzare i data set; in questo caso il data set «meuse»
data(meuse)
meuse # EP: relativa a presenza in superficie di quattro metalli pesanti nei pressi del fiume Meuse (Mosa)

# EP: head(meuse) visualizza solo le prime sei righe
# EP: names(meuse) visualizza solo i nomi
head(meuse)
names(meuse)

# EP: dà sommario (informazioni ulteriori) del dataset:
summary(meuse)

# EP: mostra grafico di correlazione tra variabili in ballo (dalla matrice di correlazione):
pairs(meuse)

# EP: permette di visualizzare solo le variabili di nostro interesse
# EP: occorre fare la tilde «~», che significa «=»
# EP: virgola è separatore di argomenti a funzione
# EP: con «data» seleziono il data set che mi interessa
pairs(~ cadmium + copper + lead , data = meuse)

# EP: in R si possono richiamare funzioni precedenti: freccia in alto e la premo quante volte mi serve
# EP: lo uso per richiamare «names(meuse)»
# Esercizio: rifaccio pairs mettendo cadmium, copper, lead e zinc
pairs(~ cadmium + copper + lead + zinc , data = meuse)

# EP: permette di ridurre i passaggi (richiamo «names» ecc.) e inserire subito variabili che ci interessano attr. colonne
# EP: in questo caso prendo un subset («[]») partendo da («,») colonna 3 a colonna 6 («:»)
# EP: ci dà grafico uguale a prima
pairs(meuse[,3:6])

# EP: per cambiare colore funzioni: «col="red"» o altri colori
pairs(meuse[,3:6], col="blue")

# EP: per cambiare tipo di caratteri uso «point character» («pch»)
# EP: ne esistono vari tipi: su 'Google immagini' ci sono figure e numeri relativi
pairs(meuse[,3:6], col="blue", pch=20)

# EP: per cambiare dimensione punti uso argomento a funzione «character exageration» («cex»)
# EP: se cex=1 non cambia nulla, con 10 è 10 volte più grande, con 0.5 diminuisce della metà
pairs(meuse[,3:6], col="blue", pch=20, cex=3)

# EP: do titolo a grafico: «main=»
pairs(meuse[,3:6], col="blue", pch=20, cex=3, main="Primo pairs")

# Esercizio: inserire anche «elevation» tra le variabili, che è la settima
pairs(meuse[,3:7], col="blue", pch=20, cex=3, main="Primo pairs")

# EP: source permette di prendere file dall'esterno
# EP: ora facciamo copia-incolla
panel.correlations <- function(x, y, digits=1, prefix="", cex.cor) # EP: «correlations» ci dà le correlazioni
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

panel.smoothing <- function (x, y, col = par("col"), bg = NA, pch = par("pch"), # EP: «smoothing» ci dà linea di correlazione
    cex = 1, col.smooth = "red", span = 2/3, iter = 3, ...)
{
    points(x, y, pch = pch, col = col, bg = bg, cex = cex)
    ok <- is.finite(x) & is.finite(y)
    if (any(ok))
        lines(stats::lowess(x[ok], y[ok], f = span, iter = iter), # EP: «lowess» è smooter locale: uso linea per mostrare relazione tra variabili
            col = 1, ...)
}

panel.histograms <- function(x, ...) # EP: «histograms» fa istogramma delle variabili
{
    usr <- par("usr"); on.exit(par(usr))
    par(usr = c(usr[1:2], 0, 1.5) )
    h <- hist(x, plot = FALSE)
    breaks <- h$breaks; nB <- length(breaks)
    y <- h$counts; y <- y/max(y)
    rect(breaks[-nB], 0, breaks[-1], y, col="white", ...)
}

# EP: «lower.panel» è parte inferiore del grafico pairs e decido cosa metterci, ad es. le correlazioni
# EP: «upper.panel» è parte superiore e ci metto lo smoothing, ossia il grafico dei punti con le linee di correlazione
# EP: «diag.panel» è diagonale e ci metto gli istogrammi
pairs(meuse[,3:6], lower.panel = panel.correlations, upper.panel = panel.smoothing, diag.panel = panel.histograms)

# Esercizio: in pairs metto lower panel smoothing, diagonal istogrammi e upper correlations
pairs(meuse[,3:6], lower.panel = panel.smoothing, upper.panel = panel.correlations, diag.panel = panel.histograms)

plot(meuse$cadmium, meuse$copper)
# EP: mi dà errore

# EP: uso questa funzione per evitare di usare «$» e di richiamare «meuse»
# EP: serve per costruire grafico solo con variabili cadmium e copper
attach(meuse)
plot(cadmium, copper)

# EP: Cambio il tipo di punto utilizzato, colore e titolo
plot(cadmium, copper, pch=19)
plot(cadmium, copper, pch=19, col="green", main="Primo plot")
# EP: con «yellow» non si vede nulla: bocciato

# EP: voglio cambiare le labels, le etichette
# EP: «xlab» è variabile 1, «ylab» è la var. 2
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame")

# EP: argomento che esagera i caratteri delle labels (es. «cex.lab=1.5»)
# EP: e cambio punti (con «cex»!)
plot(cadmium, copper, pch=19, col="green", main="Primo plot", xlab="cadmio", ylab="rame", cex.lab=1.5, cex=2)


###################################################################################################
###################################################################################################
###################################################################################################


### 2. R_code_spatial.R

# R spatial: funzioni spaziali

# EP: richiamo pacchetto «sp»
library(sp)

# EP: pacchetto utile, un multiframe automatico: «GGally»
# EP: lo installo e lo richiamo
install.packages("GGally")
library(GGally)

# EP: dati pacchetto: «meuse»
data(meuse)
head(meuse)

# EP: plot cadmium e lead
# EP: per fare i plot occorre innanzitutto allegare il database (o dataframe): uso funzione «attach»
attach(meuse)
plot(cadmium, lead, col="pink", pch=8, cex=2)
# funzione «plot» serve per rappresentazione grafica

# Esercizio: plot di copper e zinc con simbolo triangolo (17) e colore verde
plot(copper, zinc, col="green", pch=17, cex=2)

# EP: cambio anche le etichette (labels)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# EP: creo un pannello che contenga più grafici:
# EP: funzione per multiframe (o multipanel) è IMPORTANTISSIMA!
# EP: è la funzione «par», che ha come argomenti i multiframe relativi a righe e colonne (row)
# EP: «c» permette di inserire il numero di righe e colonne che vogliamo
# EP: inserisco i due grafici che ho fatto
# EP: metto in R le tre righe insieme!
par(mfrow=c(1,2)) # EP: par(mfrow=c(numero righe, numero colonne))
plot(cadmium, lead, col="pink", pch=8, cex=2, xlab="cadmio", ylab="piombo", cex.lab=1.5)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# EP: invertiamo i grafici riga-colonna in colonna-riga
# EP: innanzitutto inverto numeri riga e colonna
par(mfrow=c(2,1))
plot(cadmium, lead, col="pink", pch=8, cex=2, xlab="cadmio", ylab="piombo", cex.lab=1.5)
plot(copper, zinc, col="green", pch=17, cex=2, xlab="rame", ylab="zinco", cex.lab=1.5)

# EP: c'è pacchetto utile, un multiframe automatico: «GGally» (!!!)
# EP: lo installo e lo richiamo
# EP: install.packages("GGally")
# EP: library(GGally)
# EP: [vedi inizio codice]

# EP: faccio multipannello con tante variabili
# EP: uso una funzione interna a «GGally»
# EP: scelgo un subset (un pezzettino), perché altrimenti è troppo grande:
# EP: seleziono solo le prime quattro colonne, relative agli elementi (dalla terza alla sesta)
# EP: «3:6» si può anche scrivere «3-6»
ggpairs(meuse[,3:6])
# EP: # EP: funzione «ggpairs» costruisce matrice di grafici da un data set

# EP: ci dà un grafico con distribuzione di frequenza dati
# EP: mette plot variabili: piombo rispetto alle altre ecc.
# EP: probabilmente usa coefficiente di Spearman, ossia coeff. varia da -1 a 1
# EP: 1: correlate in maniera positiva; -1: corr. negativamente; 0: non correlate
# EP: per vedere come funzione è scritta basta digitarla e premere invio:
# EP: ggpairs

# EP: facciamo parte spaziale (!!!)
# EP: per prima cosa dobbiamo dirgli che «meuse» ha coordinate: «x» e «y»
# EP: la funzione usa «=» per fare l'associazione
head(meuse)
coordinates(meuse)=~x+y

# EP: mettio funzione «plot» e ottengo distribuzione nello spazio dei nostri dati
plot(meuse)

# EP: metto grafico all'interno di «sp»
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

# Esercizio: fare «spplot» con il rame
spplot(meuse, "copper")

# EP: lo stesso plottaggio si può fare con «bubble» --> bubble plot
# EP: il risultato è più bello!
bubble(meuse, "zinc")

# Esercizio: fare «bubble» del rame in rosso
bubble(meuse, "copper", col="red")

# EP: foraminiferi (Sofia) e carbon capture (Matteo)
# EP: «c» è un array (o vettore)
# EP: al vettore occorre dare un nome, tramite «<-» (assegnazione)
# EP: abbiamo creato un oggetto (!!!)
foram <- c(10, 20, 35, 55, 67, 80)
carbon <- c(5, 15, 30, 70, 85, 99)

# EP: vedo se sono correlati
# EP : per farlo li graficizzo
plot(foram, carbon, col="green", cex=2, pch=19)

# EP: scarico dati e tabelle da iol (è in .csv)
# EP: sono dati dall'esterno su Covid-19: devo importarli
# EP: lo prendo dalla cartella «lab»
# EP: -> percorso/lab (N.B.!!!)
setwd("/Users/enricopriarone/lab") # EP: set della working directory

# EP: leggo la tabella
# EP: occorre spiegare a R che la prima riga contiene i titoli («header») delle variabili
# EP: «head=T» o «head=TRUE» (sempre in maiuscolo!!!), perché l'header c'è!
# EP: al contrario dovrei mettere «FALSE» (o «F»)
# EP: associo la funzione a «covid» con «<-»
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

library(ggplot2) # EP: o require(ggplot2)
library(spatstat)
library(rgdal)

# EP: Faccio set della wd, per poi richiamare file con dati Covid
setwd("/Users/enricopriarone/lab")

# EP: Importo dati tabella
covid <- read.table("covid_agg.csv", head=T)
head(covid)

# EP: Facciamo un plot per visualizzare la distribuzione dei dati
# EP: Dollaro («$») serve a relazionare ogni colonna al proprio dataset
# EP: Altrimenti uso «attach(covid)» e poi «plot(country, cases)»
plot(covid$country, covid$cases)

# EP: Grafico si vede male, allora lo rendiamo verticale
# EP: Provo con labels uguale a 0 («las=0») -> non è cambiato nulla (perché è il default) -> è parallel!
plot(covid$country, covid$cases, las=0) # EP: parallel labels

# EP: Con las=1 ho orizzontale («horizontal»)
# EP: Con las=2 labels sono perpendicolari
# EP: Con las=3 sono tutte verticali
plot(covid$country, covid$cases, las=1) # EP: horizontal labels
plot(covid$country, covid$cases, las=2) # EP: perpendicular labels
plot(covid$country, covid$cases, las=3) # EP: vertical labels

# EP: Diminuisco grandezza delle etichette ad es. con «cex.axis=0.3»
plot(covid$country, covid$cases, las=3, cex.axis=0.3)

# EP: Faccio plot tramite «ggplot»
# library(ggplot2) o require(ggplot2) [vedi inizio codice]

# EP: Visualizzo dati in ggplot
# EP: uso il data set «mpg», contenuto nel pacchetto «ggplot2»
# EP: «mpg» contiene i dati relativi al risparmio di carburante tra 1999 e 2008 per 38 modelli di automobili popolari
data(mpg)
head(mpg)

# Funzione «ggplot» serve a creare grafici con il pacchetto «ggplot2»
# EP: dichiaro data set («mpg»)
# EP: dichiaro «aes()» (aesthetics), ossia l'estetica con cui vengono assegnate le variabili agli assi («x=» e «y=»)
# EP: dichiaro tipo di geometria attraverso un'altra micro funzione («geom_»), che in questo caso specifica che vogliamo usare dei punti
ggplot(mpg, aes(x=displ, y=hwy)) + geom_point()

# EP: provo a trasformare in grafico per linee (cambio geometria)
# EP: provo anche con i poligoni
ggplot(mpg, aes(x=displ, y=hwy)) + geom_line()
ggplot(mpg, aes(x=displ, y=hwy)) + geom_polygon()

# EP: Faccio lo stesso con i dati su Covid-19 già caricati
# EP: Prima metto i dati («covid»), poi le aes (le cerco con «names» o «head» -> scelgo «lon», «lat» e i «cases» per grandezza)
ggplot(covid, aes(x=lon, y=lat, size=cases)) + geom_point()

# EP: Richiamo «spatstat» e creo un data set apposito
# library(spatstat) [vedi inizio codice]

# EP: Uso applicazione point pattern («ppp») e la collego a dati Covid-19
# EP: Metto lon e lat e i range di x(-180 ≤ lon ≤ 180) e y(-90 ≤ lat ≤ 90)
attach(covid)
covids <- ppp(lon, lat, c(-180, 180), c(-90, 90))

# EP: Faccio funzione «density», ossia la densità del set spaziale sul Covid-19
# EP: e ne faccio il grafico, una mappa di densità
d <- density(covids)
plot(d)

# EP: Inserisco nel grafico punti di origine e i contorni dei Paesi
points(covids)

# EP: Salvo codice R su file come .rdata
# EP: q() -->  non necessario in Mac:
# EP: è sufficiente chiudere R e selezionare «Salva»


# 01/04
# EP: Richiamo la cartella «lab» (dove ho anche .Rdata)
# EP: Ricarico i dati di ieri richiamando .Rdata
setwd("/Users/enricopriarone/lab")
load("point_pattern.RData") # EP: Così ho caricato il file!

# EP: Per visualizzare i file:
ls()

# EP: Carico «spatstat» e «rgdal»
library(spatstat)
library(rgdal) # EP: Permette in tutti i software e sistemi operativi di leggere raster e vettori

# EP: Faccio grafico sulla densità
# EP: e cambio un po' le cose
# EP: ad esempio creando una palette di colori, tramite un array (con «c()»)
# EP: Occhio: in questa funzione usare virgolette singole!
# EP: Metto il numero di livelli di colore che voglio, aggiungendo tra parentesi il numero
plot(d)
cl <- colorRampPalette(c('yellow', 'orange', 'red'))(100) # EP: Funzione importantissima!
plot(d, col=cl)

# Esercizio: plot con colori da verde a blu
cl2 <- colorRampPalette(c('green', 'dark green', 'light blue', 'blue'))(200)
plot(d, col=cl2)

# EP: Aggiungo i dati (punti) del Covid-19, richiamandoli
# EP: e cambio al colore ai punti in vista del grafico finale
points(covids, col="red")

# EP: Carico i confini dei Paesi attraverso «coastlines» con risoluz. 10 m
# EP: Cartella contiene vari file, in particolare lo shapefile
# EP: Associo il file alla funzione file vettoriali
# EP: Se dà errore è perché non ho caricato la libreria «rgdal»: la carico prima di «coastlines»
# EP: library(rgdal) --> permette in tutti i software e sistemi operativi di leggere raster e vettori [vedi inizio codice]
coastlines <- readOGR("ne_10m_coastline.shp")

# EP: Faccio plot coastlines e lo aggiungo al plot precedente con «add=»
plot(coastlines, add=T)

# EP: Più avanti andremo a cartografare anche il numero di casi!

# Esercizio: plot mappa di densità con nuova colorazione e aggiunta coastlines
cl3 <- colorRampPalette(c('light blue', 'green', 'dark green', 'blue'))(400)
plot(d, col=cl3)
plot(coastlines, add=T)


# 22/04
# EP: Carico «point_pattern.RData»
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
# EP: Con funzione «marks» andiamo a prendere i dati nel point pattern «covids»
# EP: Il dollaro ci serve per non fare l'«attach»
head(covid)
marks(covids) <- covid$cases

# EP: Chiamo «s» la stima dei valori attraverso la funzione di smooth («Smooth»)
s <- Smooth(covids)
plot(s)

# EP: Rifare plot di «s», cambiando palette e aggiungendo punti e coastlines
cls <- colorRampPalette(c('cyan', 'purple', 'red'))(100)
plot(s, col=cls)
points(covids, col="grey")
coastlines <- readOGR("ne_10m_coastline.shp")
plot(coastlines, add=T, main="Interpolazione")
# EP: Si vede che mettendo il focus sul numero di casi l'Asia a febbraio era la più colpita

# EP: Con «text» vediamo anche valori dei punti sulla mappa
text(covids) # EP: Ma questa volta ci dà l'ID, non il valore dei casi

# EP: Facciamo una carta finale con entrambi i plot
# EP: Usiamo un multiframe («mfrow»)
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

# San Marino, studio di caso
# EP: Carico i dati esterni («Tesi.RData») che ho scaricato da iol
# EP: Serve libreria «spatstat»
# EP: library(spatstat)
load("Tesi.RData")
ls()
head(Tesi)

# EP: Facciamo un attach della tabella per usarla con il point pattern
attach(Tesi)

# EP: Point pattern: x, y, c(xmin,xmax), c(ymin,ymax)    (N.B.!!!)
# EP: Mettiamo anche i limiti per le due coordinate, trattandosi di un'area ristretta
# EP: Uso funzione «summary(dataset)» per avere il sommario della tabella
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
# EP: «dT» è density map di Tesippp
# EP: «Tesi» è un dataset
# EP: «Tesippp» è il point pattern del file "Tesi" (da libreria «spatstat»)

# EP: Richiamo librerie e poi associo i valori che vogliamo stimare
library(rgdal) 
library(spatstat)
plot(dT)
points(Tesippp, col="green")

# EP: Andiamo a stimare la ricchezza specifica
# EP: Con «head» vedo che si trova sotto «Species_richness»
# EP: «marks()» va a prendere i valori dalla tabella e li associa ai punti del ppp
head(Tesi)
marks(Tesippp) <- Tesi$Species_richness

# EP: Creo mappa e l'associo a «Smooth»
interpol <- Smooth(Tesippp)

# EP: Usiamo file su San Marino
# EP: Carico libreria «rgdal»
# EP: library(rgdal) [vedi inizio codice]
sanmarino <- readOGR("San_Marino.shp")
plot(sanmarino)
plot(interpol, add=T) # EP: Importante! Con «T»/«True» aggiunge nuova mappa a quella precedente
points(Tesippp,col="green")
# EP: Mappa va a sovrapporsi al territorio di San Marino
plot(sanmarino, add=T) # EP: Così i confini di Stato si sovrappongono al plot «interpol»

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

# Codice R per analisi di immagini satellitari (telerilevamento)

# EP: Installo pacchetti che servono o li richiamo
install.packages("RStoolbox") # EP: Verifica [?]
library(raster)
library(RStoolbox) # EP: Verifica [?]

# EP: Richiamo la setwd
setwd("/Users/enricopriarone/lab")

# EP: Uso «p224r63_2011_masked.grd»
# EP: Associamo il file alla funzione «brick()», per importare immagine satellitare con tutte le bande
p224r63_2011 <- brick("p224r63_2011_masked.grd")

# EP: Cerco informazioni sull'immagine attraverso grafici su bande, riflettanze ecc.
plot(p224r63_2011)

# EP: Ci appare stessa immagine in 7 bande, che per LANDSAT sono sempre le stesse:
# EP: 1) blu;
# EP: 2) verde;
# EP: 3) rosso;
# EP: 4) NIR (near infrared);
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
# EP: «attach(dataframe)» non funziona con i raster
# EP: allora per legare immagine a banda del blu uso simbolo che lega la colonna (banda) al dataset (immagine satellitare): «$»
clb <- colorRampPalette(c('dark blue','blue','light blue'))(100)
plot(p224r63_2011$B1_sre, col=clb)
# EP: parte bianca è mascherata: i pixel sono stati eliminati perché non servivano

# Esercizio: plottare banda del NIR con colorRampPalette che varia dal rosso all'arancione al giallo
clnir <- colorRampPalette(c('red','orange','yellow'))(100)
plot(p224r63_2011$B4_sre, col=clnir)

# EP: creo multiframe
# EP: uso «par» per usare finestra a blocchi
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

# EP: Montiamo le bande insieme per fare immagine come la vedrebbe l'occhio umano (con funz. «plotRGB»)
# EP: Usiamo le 3 componenti: R=banda del rosso, G=verde, B=blu (sono disponibili solo 3 bande tutte insieme)
# EP: Chiudiamo prima la finestra plottata
dev.off()
plotRGB(p224r63_2011, r=3, g=2, b=1)

# EP: Stiro un po' i colori per vederlo meglio
# EP: Quello lineare è il più usato
plotRGB(p224r63_2011, r=3, g=2, b=1, stretch="Lin")

# EP: Usiamo anche il NIR per distinguere ombre da piante
# EP: Ma sono disponibili solo 3 bande alla volta: occorre eliminarne una
# EP: Elimino la banda blu scalando tutto di uno: r=4, g=3, b=2 )
# EP: È chiamata «432» o «false colours» («falsi colori»)
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")

# EP: Salvo il grafico in PDF
# EP: Per png: «png("primografico.pdf")»
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
# EP: «grd» sta per griglia: formato è di righe e colonne
p224r63_1988 <- brick("p224r63_1988_masked.grd")

# EP: Facciamo un plot dell'immagine
plot(p224r63_1988)

# EP: Rifacciamo il lavoro dell'altra volta con l'immagine del 1988
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

# Esercizio: fare plot dell'immagine usando il NIR nella componente «r»
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin") # EP: Già che ci sono sposto tutte le bande di 1: da 4-2-1 a 4-3-2

# EP: Faccio plot delle due immagini (1988 e 2011)
# EP: Devo fare un multipanel: funz. «par», con «mfrow»
par(mfrow=c(2,1))
plotRGB(p224r63_1988, r=4, g=3, b=2, stretch="Lin")
plotRGB(p224r63_2011, r=4, g=3, b=2, stretch="Lin")
# EP: Confronto: agricolo è molto più sviluppato nel 2011

dev.off()

# EP: Creo un indice per verificare le differenze
# EP: DVI=NIR-RED, e questo si fa pixel per pixel
# EP: Indice spettrale DVI
# EP: dvi_1988 = nir1988-red1988
# EP: uso il «$» per collegare le bande al dataset
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
# EP: La riduco di n volte: se pixel è di 30 m, «fact=10» mi dà nuovo pixel di 300 m
p224r63_2011lr <- aggregate(p224r63_2011, fact=10)

# EP: Guardo caratteristiche immagini
# EP: imm. originale ha «resolution» 30x30
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
plot(dvi_2011lr50) # EP: Ma con questo perdo tantissimo e sembra tutto omogeneo

# EP: Faccio lo stesso per il 1988
p224r63_1988lr50 <- aggregate(p224r63_1988, fact=50)
dvi_1988lr50 <- p224r63_1988lr50$B4_sre - p224r63_1988lr50$B3_sre

# EP: Faccio «difdvi» a bassa risoluzione
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

# R code land cover

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
# EP: ottengo un vero e proprio modello
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

# EP: Carichiamo immagine satellitare da cartella «lab» attraverso funzione di raster «brick»
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
load("defor.RData") # EP: Uso questo perché il mio «.RData» mi dà errore: i dati saranno un po' diversi
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

# EP: Creo grafico con percentuale di foresta/agricoltura prima della deforestazione («before»)
ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")
# EP: agricoltura occupa percentuale molto bassa: ca. 10%; contro il ca. 90% della foresta

# Esercizio: faccio lo stesso per il dopo deforestazione («after»)
ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")
# EP: agricoltura è vicina al 50%; foresta lo supera appena

# EP: Usiamo funzione «par»
# EP: Necessita del pacchetto «gridExtra», che installiamo [vedi inizio codice 05/05]
# EP: Facciamo istogramma delle percentuali associando prima i due grafici a un nome
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) +
geom_bar(stat="identity", fill="white")

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) +
geom_bar(stat="identity", fill="white")

# Esercizio: usare «grid.arrange» per graficizzarli
grid.arrange(grafico1, grafico2, nrow=1)

# EP: Manca da cambiare la y! Vedi poi «iol»

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

# EP: «ylim» funziona con «ggplot» e permette di inserire in limite all'ordinata
# EP: Prendere libro «ggplot2. Elegant graphics...» di Whickam!
grafico1 <- ggplot(output, aes(x=cover, y=before, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

grafico2 <- ggplot(output, aes(x=cover, y=after, color=cover)) + 
geom_bar(stat="identity", fill="white") +
ylim(0, 100)

# Esercizio: usa «grid.arrange» graficizzare
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
EN01 <- raster("EN_0001.png") # EP: Lo «0» prima è importante perché R non li riconosce come numeri, ma come stringhe:
plot(EN01)                    # EP: altrimenti la successiva sarebbe stata «EN10» e non «EN2»

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
# EP: In alternativa uso un ciclo «for»: guardare in codice su «iol»!

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

# Esercizio: faccio grafico di tutte le immagini con un «par»
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
# EP: Domani proviamo col «for»

dev.off()

# 06/05
# EP: Cambio la working directory con la nuova cartella
setwd("/Users/enricopriarone/lab/esa_no2")
library(raster)

# EP: creo una lista di pattern, di cui diciamo che sono nominati «.png»
rlist <- list.files(pattern=".png")
rlist

# EP: usiamo funzioni «raster» (non «brick», perché carica l'intero pacchetto layer satellitare: a noi ne serve uno solo)
# EP: e «lapply()»
listafinale <- lapply(rlist, raster)
listafinale
# EP: sono 13 RasterLayer

# EP: usiamo funzione «stack» per creare un pacchetto unico di dati: creiamo un'unica immagine
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
# <https://land.copernicus.vgt.vito.be/PDF/portal/Application.html>

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
# EP: con «lapply»
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
d1c.for <- reclassify(d1c, cbind(1,NA)) # EP: «d1c.for» di solito è nome da modello; in alternativa «d1c_for»

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
d1c.for.patches # EP: Ha 301 patches
d2c.for.patches # EP: Ha 1212 patches

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

# EP: uso funzioni «raster» e «lapply»
listasnow <- lapply(rlist, raster)
listasnow
# EP: 6 RasterLayer

# EP: uso funzione «stack» per creare un pacchetto unico di dati: creiamo un'unica immagine
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

# Esercizio: applichiamo la funzione «crop()» su tutte le immagini, ossia su più livelli
snow.multitemp.italy <- crop(snow.multitemp, extension)
snow.multitemp.italy  # EP: Da qui prendo valori max e min per scala
plot(snow.multitemp.italy, col=clb, zlim=c(20,200))

# EP: Facciamo analisi sullo stack tagliato su Italia
# EP: Facciamo boxplot (ma volendo anche regressione, previsione con «prediction» ecc.)
boxplot(snow.multitemp.italy, horizontal=T,outline=F)

# EP: Vedere file caricato su iol («prediction.r»?) e nel caso contattare il prof

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


### Progetto esame
### R_code_Borneo.r

# Dati Copernicus: <https://land.copernicus.vgt.vito.be/PDF/portal/Application.html> (Albergel et Al. 2008 & Wagner et Al. 1999).
# «The Soil Water Index quantifies the moisture condition at various depths in the soil.
# [...] The soil moisture, up to 5cm soil depth, is recognized as an Essential Climate Variable (ECV) by the Global Climate Observing System (GCOS)» (<https://land.copernicus.eu/global/products/swi>).
# Ho preso i dati a scala globale, di risoluzione 12.5km, basate su «EUMETSAT H SAF Metop ASCAT surface soil moisture».

# Dati NASA degli incendi: <https://firms.modaps.eosdis.nasa.gov/download/>.
# Riportano i dati degli incendi e delle anomalie di temperatura attraverso i sensori VIIRS e MODIS.
# Ho usato i dati VIIRS riferiti al satellite S_NPP.
# Risoluzione sensore: 375m (immagine: 250m). Due volte al giorno.


# Ho scaricato i dati realtivi a SWI e incendi in Borneo del 7 novembre degli anni 2015, 2016, 2017, 2018 e 2019.
# Comparerò i dati riferiti al 7 novembre di cinque anni diversi (dal 2015 al 2019) di entrambi i set, per osservare i cambiamenti.
# Proverò a vedere se ci sono eventuali correlazioni tra gli incendi e il cambiamento di SWI.




### Prima parte

# Richiamo le librerie necessarie
library(raster)
library(ggplot2)
library(igraph)
library(ncdf4)
library(spatstat)
library(rgdal)
library(RStoolbox)


# Faccio il set della working directory
setwd("/Users/enricopriarone/lab/esame")


# Per prima cosa carico le immagini Copernicus tutte insieme

# Creo una lista di pattern denominati «.nc»
nclist <- list.files(pattern=".nc")
# nclist           # Così vedo l'elenco delle immagini incluse

# Uso funzioni «raster» e «lapply»
listafinale <- lapply(nclist, raster)
# listafinale      # Vedo i cinque RasterLayer

# Uso funzione «stack» per creare un pacchetto unico di dati: un'unica immagine
swi_12.5km <- stack(listafinale)
# swi_12.5km       # È di classe RasterStack

# Ritaglio le immagini intorno all'estensione del Borneo, inserendo le apposite coordinate
ext <- c(108, 120, -5, 8)
swi_12.5km_bor <- crop(swi_12.5km, ext)
swi_12.5km_bor     # Richiamando il set vedo che valori variano tra 0 e 99.5: li metto come limiti della scala con «zlim»

# Graficizzo il set di immagini relative al SWI in Borneo
cla <- colorRampPalette(c('brown','yellow','dark blue'))(400)
plot(swi_12.5km_bor, col=cla, main="Soil Water Index in Borneo (2015-2019)", zlim=c(0,100), las=1)

dev.off()

# Importo uno .shp contenente i confini di Stato e lo ritaglio nella zona che mi serve
admin <- readOGR("ne_10m_admin_0_countries.shp")
admin.bor <- crop(admin, ext)



### Seconda parte

# Carico le immagini Copernicus singolarmente perché con «lapply()» non riesco a lavorare sulle singole immagini
swi2015r <- raster("c_gls_SWI_201511071200_GLOBE_ASCAT_V3.1.1.nc")
swi2016r <- raster("c_gls_SWI_201611071200_GLOBE_ASCAT_V3.1.1.nc")
swi2017r <- raster("c_gls_SWI_201711071200_GLOBE_ASCAT_V3.1.1.nc")
swi2018r <- raster("c_gls_SWI_201811071200_GLOBE_ASCAT_V3.1.1.nc")
swi2019r <- raster("c_gls_SWI_201911071200_GLOBE_ASCAT_V3.1.1.nc")


# Ritaglio tutte le immagini imponendo come estensione quella del Borneo  e faccio plot di verifica
# extension <- c(-18.3, 9.4, 2.5, 15.5)
# cla <- colorRampPalette(c('brown','yellow','dark blue'))(400)
swi2015r.bor <- crop(swi2015r, ext)
# plot(swi2015r.bor, col=cla, las=1, main="SWI Borneo 2015", xlab="long", ylab="lat")
# plot(admin.bor, add=T)
swi2016r.bor <- crop(swi2016r, ext)
# plot(swi2016r.bor, col=cla, las=1, main="SWI Borneo 2016", xlab="long", ylab="lat")
# plot(admin.bor, add=T)
swi2017r.bor <- crop(swi2017r, ext)
# plot(swi2017r.bor, col=cla, las=1, main="SWI Borneo 2017", xlab="long", ylab="lat")
# plot(admin.bor, add=T)
swi2018r.bor <- crop(swi2018r, ext)
# plot(swi2018r.bor, col=cla, las=1, main="SWI Borneo 2018", xlab="long", ylab="lat")
# plot(admin.bor, add=T)
swi2019r.bor <- crop(swi2019r, ext)
# plot(swi2019r.bor, col=cla, las=1, main="SWI Borneo 2019", xlab="long", ylab="lat")
# plot(admin.bor, add=T)
# dev.off()


# Lavoro sulle immagini del 2015 e del 2019 confrontandole

# Cerco valori max e min delle immagini Copernicus del 2015 e del 2019
swi2015r.bor # 0 --> 95
swi2019r.bor # 0 --> 83.5

# Graficizzo le due immagini e via via faccio confronti tra i vari anni
# cla <- colorRampPalette(c('brown','yellow','dark blue'))(400)
par(mfrow=c(1,2))
plot(swi2015r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2015-2019 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2019r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)

# Faccio lo stesso tra un'annata e l'altra: 2015-'16, 2016-'17, 2017-'17, 2018-'19
# A sinistra ho sempre l'immagine più vecchia
par(mfrow=c(1,2))
plot(swi2015r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2015-2016 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2016r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)

par(mfrow=c(1,2))
plot(swi2016r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2016-2017 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2017r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)

par(mfrow=c(1,2))
plot(swi2017r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2017-2018 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2018r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)

par(mfrow=c(1,2))
plot(swi2018r.bor, zlim=c(0,100), col=cla, las=1, main="Confronto SWI 2018-2019 Borneo", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(swi2019r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)


dev.off()



### Terza parte

# Carico dati .shp degli incendi del 7 novembre degli anni dal 2015 al 2019 e faccio plot di verifica
fire2015r <- readOGR("fire_archive_V1_134134.shp")
# plot(fire2015r)
fire2016r <- readOGR("fire_archive_V1_134135.shp")
# plot(fire2016r)
fire2017r <- readOGR("fire_archive_V1_134136.shp")
# plot(fire2017r)
fire2018r <- readOGR("fire_archive_V1_134137.shp")
# plot(fire2018r)
fire2019r <- readOGR("fire_archive_V1_134138.shp")
# plot(fire2019r)
# dev.off()

# Graficizzo incendi e SWI di ogni anno tutti insieme
par(mfrow=c(1,5))
plot(swi2015r.bor, zlim=c(0,100), col=cla, las=1, xlab="long",ylab="lat") # «main» tolto perché non viene visualizzato
plot(fire2015r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2016r.bor, zlim=c(0,100), col=cla, las=1, xlab="long",ylab="lat")
plot(fire2016r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2017r.bor, zlim=c(0,100), col=cla, las=1, xlab="long",ylab="lat")
plot(fire2017r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2018r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(fire2018r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

plot(swi2019r.bor, zlim=c(0,100), col=cla, las=1, xlab="long", ylab="lat")
plot(fire2019r, add=T, pch=21, col="red")
plot(admin.bor, add=T)


dev.off()


# Faccio la differenza tra pixel immagine 2016 e pixel immagine 2015
# e provo a vedere se corrisponde
dif_swi_1615 <- swi2016r.bor - swi2015r.bor
dif_swi_1615 # Range: -60, 97
# Se differenza dà risultato negativo significa che SWI è diminuito
# Se differenza dà risultato positivo significa che si è verificato un aumento

# Analizzo differenze di SWI di ogni anno confrontandole con gli incendi
# Dall'alto al basso trovo sempre i grafici di: differenza pixel, incendi anno più vecchio, incendi anno più recente
clr <- colorRampPalette(c('green', 'yellow', 'red')) (400)
par(mfrow=c(3,1))
plot(dif_swi_1615, col=clr, las=1, main="Differenza SWI Borneo 2016-2015 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2015r, pch=21, col="blue", las=1)
plot(admin.bor, add=T)
plot(fire2016r, pch=21, col="red", las=1)
plot(admin.bor, add=T)

# Per analizzarle, faccio tutte le differenze tra i vari anni
dif_swi_1716 <- swi2017r.bor - swi2016r.bor
dif_swi_1716 # Range: -65, 51.5
par(mfrow=c(3,1))
plot(dif_swi_1716, col=clr, las=1, main="Differenza SWI Borneo 2017-2016 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2016r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2017r, pch=21, col="red")
plot(admin.bor, add=T)

dif_swi_1817 <- swi2018r.bor - swi2017r.bor
dif_swi_1817 # Range: -55.5, 46.5
par(mfrow=c(3,1))
plot(dif_swi_1817, col=clr, las=1, main="Differenza SWI Borneo 2018-2017 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2017r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2018r, pch=21, col="red")
plot(admin.bor, add=T)

dif_swi_1918 <- swi2019r.bor - swi2018r.bor
dif_swi_1918 # Range: -61.5, 29
par(mfrow=c(3,1))
plot(dif_swi_1918, col=clr, las=1, main="Differenza SWI Borneo 2019-2018 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2018r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2019r, pch=21, col="red")
plot(admin.bor, add=T)


# Faccio differenza tra immagine 2019 e immagine 2015 e la graficizzo in scala di grigi
# Dall'alto al basso ho immagine 2015, immagine 2019 e differenza in scala di grigi
dif_swi_1915 <- swi2019r.bor - swi2015r.bor
dif_swi_1915 # Range: -76, 57.5
# cla <- colorRampPalette(c('brown','yellow','dark blue'))(400)
clgg <- colorRampPalette(c('black','grey','red'))(100)
par(mfrow=c(3,1))
plot(swi2019r.bor, zlim=c(0,100), las=1, main="Differenza SWI 2019-2015", col=cla, xlab="long", ylab="lat")
plot(swi2015r.bor, zlim=c(0,100), col=cla, xlab="long", ylab="lat", las=1)
plot(dif_swi_1915, col=clgg, xlab="long", ylab="lat", las=1)
# Dove è nero c'è stata una forte diminuzione, mentre dove è rosso un aumento

# Graficizzo scala di grigi con incendi per un confronto
# In altro ho immagine differenza e poi incendi 2015 e incendi 2019
par(mfrow=c(3,1))
plot(dif_swi_1915, col=clgg, las=1, main="Differenza SWI Borneo 2019-2015 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(fire2019r, pch=21, col="blue")
plot(admin.bor, add=T)
plot(fire2015r, pch=21, col="red")
plot(admin.bor, add=T)

dev.off()



### Quarta parte

# Uso funzione «unsuperClass()» per riclassificare immagini in 2 classi

cldy <- colorRampPalette(c('dark green', 'yellow'))(100)  # legenda

# Lavoro sulle immagini del 2017 e del 2019 (anni con valori SWI e incendi molto diversi)
borneo2017_rec <- unsuperClass(swi2017r.bor, nClasses=2)
borneo2019_rec <- unsuperClass(swi2019r.bor, nClasses=2)
borneo2017_rec # Ha valori da 1 a 2: divisione fatta!
borneo2019_rec # Ha valori da 1 a 2: divisione fatta!

# Graficizzo le classi create per vedere a che valori corrispondono e le metto a confronto con immagini Copernicus originali
par(mfrow=c(2,2))
plot(borneo2017_rec$map, col=cldy, las= 1, xlab="long", ylab="lat", main="Classi SWI create (2) 2017 e 2019")
plot(swi2017r.bor, las=1, xlab="long", ylab="lat")

plot(borneo2019_rec$map, col=cldy, las= 1, xlab="long", ylab="lat")
plot(swi2019r.bor, las=1, xlab="long", ylab="lat")
# Classe 1: SWI basso
# Classe 2: SWI alto

dev.off()

# Graficizzo solo le due immagini con le classi
par(mfrow=c(1,2))
plot(borneo2017_rec$map, col=cldy, las= 1, main="Valori SWI Borneo 2017 e 2019 in 2 classi:", xlab="long", ylab="lat")
plot(borneo2019_rec$map, col=cldy, las=1, xlab="long", ylab="lat", main="1) Alto; 2) Basso")

# Uso funzione «reclassify()» per eliminare la prima classe, corrispondente ai valori alti
# Voglio lavorare solo su valori bassi, così da poterli confrontare con gli incendi
borneo2017_for <- reclassify(borneo2017_rec$map, cbind(1, NA))
borneo2019_for <- reclassify(borneo2019_rec$map, cbind(1, NA))

# Uso funzione «clump()» per suddividere quello che ho ottenuto in patches
borneo2017_for.patches <- clump(borneo2017_for)
borneo2019_for.patches <- clump(borneo2019_for)
borneo2017_for.patches
borneo2019_for.patches

# patches:
# 2015: 21
# 2019: 24

# Creo due immagini .tif con le nuove suddivisioni
writeRaster(borneo2017_for.patches, "borneo2017_for.patches.tif")
writeRaster(borneo2019_for.patches, "borneo2019_for.patches.tif")

dev.off()

# Graficizzo le immagini divise in patches ottenute
clp <- colorRampPalette(c('dark blue','blue','green','orange','yellow','red'))(100)

par(mfrow=c(1,2))
plot(borneo2017_for.patches,col=clp, las=1, main="Patches SWI Borneo 2017-2019", xlab="long", ylab="lat")
plot(borneo2019_for.patches,col=clp, las=1, xlab="long", ylab="lat")
dev.off()

# Le graficizzo confrontandole con gli incendi del 2017 e del 2019
par(mfrow=c(2,2))
plot(borneo2017_for.patches,col=clp, las=1, main="Patches basso SWI Borneo 2017-2019 con incendi", xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(borneo2019_for.patches,col=clp, las=1, xlab="long", ylab="lat")
plot(admin.bor, add=T)
plot(borneo2017_for.patches,col=clp, las=1, xlab="long", ylab="lat")
plot(fire2017r, add=T, pch=21, col="red")
plot(admin.bor, add=T)
plot(borneo2019_for.patches,col=clp, las=1, xlab="long", ylab="lat")
plot(fire2019r, add=T, pch=21, col="red")
plot(admin.bor, add=T)

dev.off()

# Creo un grafico con «ggplot()» che rappresenta la suddivisione in patches nel 2017 e nel 2019
time <- c("Basso SWI 2017","Basso SWI 2019")
npatches <- c(21,24)

doc <- data.frame(time,npatches)
attach(doc)

ggplot(doc, aes(x=time, y=npatches, color="red"), main="trrr") + geom_bar(stat="identity", fill="white")

dev.off()
