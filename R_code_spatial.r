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
