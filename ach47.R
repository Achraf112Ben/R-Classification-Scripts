data = read.table("http://allousame.free.fr/m2mlds/donnees/Aiguillage.txt", header=F, sep =",")
X=data[,-553]
vrais_classes=data[,553]

# Matrice des distances
D <- dist(X, method = "euclidean")

# Mise en oeuvre de l’algorithme
H <- hclust(D, method="ward.D2")

# Représentation graphique (dendrogramme)
plot(H)

# Coupe de l’arbre pour trouver la meilleure partition en K=2 classes
classes <- cutree(H, k=3)

# Graphique dans le premier plan principal (ACP) avec classes colorées
#ACP <- princomp(data)
#plot(ACP$scores[,1],ACP$scores[,2],col=classes,pch=classes)
matplot(t(X), type="l", lty=1, col=classes)
