data = read.table("http://allousame.free.fr/m2mlds/donnees/Aiguillage.txt", header=F, sep =",")
X=data[,-553]
vrais_classes=data[,553]
# Lancer des k-means pour K=2 classes
KM <- kmeans(X, 4)
# Résultats
summary (KM)
# Centres des classes
KM$centers
# Classe de chaque objet
KM$cluster
# Inertie intra-classe
KM$tot.withinss

p = KM$cluster==((vrais_classes+3)%%4)
p = as.integer(p)
p = sum(p)/length(p)
p

matplot(t(X), type="l", lty=1, col=KM$cluster)
