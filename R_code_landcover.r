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
