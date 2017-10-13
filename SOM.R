library(class)
library(MASS)
library(kohonen)

data = as.matrix(read.table("http://allousame.free.fr/m2mlds/donnees/Aiguillage.txt", header=F, sep =","))
y = data[,553]
x = data[,-553]

SOM.AIG = som(x, grid = somgrid(10,10, "rectangular"), rlen=200, alpha=c(0.05,0.01))
SOM.AIG$codes # coordonnees des centres (espace d’entrée)
SOM.AIG$distances # distances entres neuronnes adjacents
SOM.AIG$changes # erreur entre les donnees et les centres

plot(SOM.AIG,type = "changes")
colour1 = tricolor(SOM.AIG$grid)
plot(SOM.AIG, type="mapping", bgcol = rgb(colour1))
plot(SOM.AIG, type="mapping", labels=x[,5], pchs=2)
plot(SOM.AIG,type="quality")  # quqlité de représentation
plot(SOM.AIG,type="counts") # quqlité de représentation
plot(SOM.AIG,type="codes")

# Modification de la palette de couleurs (bleu -> rouge)
coolBlueHotRed = function(n, alpha = 1)
{
  rainbow(n, end=4/6, alpha=alpha)[n:1]
}

# U-matrix (matrice de voisinage)
plot(SOM.AIG, type="dist.neighbours",palette.name=coolBlueHotRed)

# Component planes
par(mfrow=c(2,5))
for (i in 1:10)
{
  plot(SOM.AIG, type="property", palette.name=coolBlueHotRed, property=SOM.AIG$codes[[1]][,i], main=colnames(SOM.AIG$codes[[1]])[i])
}

