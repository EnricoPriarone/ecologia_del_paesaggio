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

