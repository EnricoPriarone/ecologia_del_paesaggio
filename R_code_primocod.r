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

