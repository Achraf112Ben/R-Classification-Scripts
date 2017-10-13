data = read.table("http://allousame.free.fr/m2mlds/donnees/Velib.txt", header=T, sep =" ")
location = data[,2:3]
plot(location)

taux = data[,-(1:3)]
matplot(t(), type="l", lty=1)

# Coupe de l’arbre pour trouver la meilleure partition en K=2 classes
#classes <- cutree(H, k=3)

# Graphique dans le premier plan principal (ACP) avec classes colorées
ACP <- princomp(taux)
plot(cumsum(ACP$sdev^2/sum(ACP$sdev^2)))
min(which(cumsum(ACP$sdev^2/sum(ACP$sdev^2))>=0.95))

scr = ACP$scores[,1:37]

# Matrice des distances
D <- dist(scr, method = "euclidean")

# Mise en oeuvre de l’algorithme
H <- hclust(D, method="ward.D2")

# Représentation graphique (dendrogramme)
plot(H)

# Coupe de l’arbre pour trouver la meilleure partition en K=2 classes
classes <- cutree(H, k=6)

plot(location, col=classes)

plot(rev(H$height), type = "l", xlim=c(0,20) )

data_longitude = location[,1]
data_latitude = location[,2]

position=c(2.20, 48.8, 2.48, 48.9)
mymap=get_map(location=position, source="google", maptype="roadmap")
gm=ggmap(mymap)
gm+geom_point(aes(longitude,latitude), data = data.frame(longitude=data_longitude,latitude=data_latitude),alpha = .5, color=classes, size = 4)
