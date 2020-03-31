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
