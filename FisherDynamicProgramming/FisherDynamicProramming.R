#This project is an asingment for my Unsurpervided learning cours at the Paris Descartes University by Pr. Allou Same
#The aim of this project is to implement the Fisher Dynamic Programming Algorithm and then use to cluster several real exemples of datasets.
#link to the paper which explains the assignment: http://allousame.free.fr/m2mlds/tp/projet.pdf 

# Author of this R script: Achraf Benlemkaddem
#######################################################################################################################

# Question 1:

diam <- function(X) {
  n = dim(X)[1]
  p = dim(X)[2]
  D = matrix(0, nrow = n, ncol = n)
  for(a in 1:n) {
    cat("a = ",a,"\n")
    for(b in 1:n) {
      if(a<b){
        if(p==1){
          Xab = X[a:b,]
          muab = mean(Xab)
          D[a,b]=sum((Xab - muab)^2)
        }
        else {
          Xab = X[a:b,]
          muab = colMeans(Xab)
          D[a,b]=sum(rowSums((Xab - muab)^2))
        }
      }
    }
  }
  return(D)
}

clustfisher <- function(X, K) {
  # Step 0: Initialiser les matrices D, M1 et M2, 
  # ainsi que le vecteur t des instants de changement et le vecteur des classes
  n = dim(X)[1]
  M1 = matrix(NA, nrow = n, ncol = K)
  M2 = matrix(NA, nrow = n, ncol = K)
  t = rep(NA,(K-1))
  cluster = rep(NA,(n))
  # step 1: calcul de la matrice triangulaire superieure des diametres
  D = diam(X)
  # step 2: calcul recursif des criteres optimaux
  for(i in 1:n) {
    M1[i,1] = D[1,i]
  }
  for(k in 2:K) {
    for(i in k:n) {
      M1[i,k] = min(M1[(k-1):(i-1),(k-1)]+D[k:i,i])
      M2[i,k] = which.min(M1[(k-1):(i-1),(k-1)]+D[k:i,i])
    }
  }
  # step 3: calcul recursif des instants de changement optimaux
  k = K-1
  m = n 
  while(k>=1) {
    t[k] = M2[m,(k+1)]
    m = t[k] - 1
    k = k - 1 
  }
  # step 5: labels des classes formes a partir des instants de changement
  for(i in 1:(t[1]-1)) {
    cluster[i] = 1
  }
  if(K>2) {
    for(k in 2:(K-1)) {
      for(i in t[k-1]:(t[k]-1)) {
        cluster[i] = k
      }
    }
  }
  for(i in t[K-1]:n) {
    cluster[i] = K
  }
  
  Ck = M1[n,K]
  
  return(list(cluster=cluster,t=t, Ck=Ck))
}

# Question 2: 

x = read.csv("http://allousame.free.fr/m2mlds/donnees/sequencesimu.txt", header = FALSE)

cluster = list()
t = list()
Ck = list()

for(k in 2:10) {
  res=clustfisher(x,k)
  cluster = c(cluster,list(res$cluster))
  t = append(t,list(res$t))
  Ck = append(Ck,res$Ck)
}

Ck = unlist(Ck)
par(mfrow=c(1,1))
plot(2:10, Ck, type = "b", main="Histogramme des valeurs de Ck", xlab = "k", ylab = "Ck")
par(new=T)
plot(2:10, Ck, type = "h", main="Histogramme des valeurs de Ck", xlab = "k", ylab = "Ck")
abline(v=4, lty=2, col="red")
par(new=F)

cluster4=unlist(cluster[3])
t4=unlist(t[3])

plot(cbind(1:dim(x)[1], x), col=cluster4, main="Classification du SequenceSemu par Fisher", xlab="indice", ylab="X")
abline(v=t4, lty=2)

# On compare Fisher a Kmeans et CAH-Wards
par(mfrow=c(2,2))

plot(cbind(1:dim(x)[1], x), col=cluster4, main="Classification du SequenceSemu par Fisher", xlab="indice", ylab="X")
abline(v=t4, lty=2)

# K-means
KM <- kmeans(x, 4)

plot(cbind(1:dim(x)[1], x), col=KM$cluster, main="Classification du SequenceSemu par K-Means", xlab="indice", ylab="X")

# CAH-Wards
D <- dist(x, method = "euclidean")
H <- hclust(D, method="ward.D2")
classes <- cutree(H, k=4)

plot(cbind(1:dim(x)[1], x), col=classes, main="Classification du SequenceSemu par CAH-Wards", xlab="indice", ylab="X")

# Question 3: 

x2 = read.csv("http://allousame.free.fr/m2mlds/donnees/Aiguillage.txt", header = FALSE)
y2 = x2[,553]
x2 = x2[,1:552]
res2 = clustfisher(x2,4)

clust2 = as.integer(array(unlist(res2[1])))
t2 = unlist(res2[2])

par(mfrow=c(1,1))
matplot(t(x2), type="l", col=clust2, main="Classification des Aiguillages par Fisher", xlab = "time", ylab = "Power (Watts)")

# On compare Fisher a Kmeans et CAH-Wards
par(mfrow=c(2,2))

matplot(t(x2), type="l", col=y2, main="Classification originale des Aiguillages", xlab = "time", ylab = "Power (Watts)")
matplot(t(x2), type="l", col=clust2, main="Classification des Aiguillages par Fisher", xlab = "time", ylab = "Power (Watts)")

# K-Means

KM <- kmeans(x2, 4)
matplot(t(x2), type="l", col=KM$cluster, main="Classification des Aiguillages par K-Means", xlab = "time", ylab = "Power (Watts)")

# CAH-Wards
D <- dist(x2, method = "euclidean")
H <- hclust(D, method="ward.D2")
classes <- cutree(H, k=4)

matplot(t(x2), type="l", col=classes, main="Classification des Aiguillages par CAH-Wards", xlab = "time", ylab = "Power (Watts)")
