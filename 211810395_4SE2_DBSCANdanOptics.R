#Install Package terlebih dahulu jika belum tersedia di RStudio
#install.packages("factoextra")
#install.packages("gridExtra")
#install.packages("dbscan")
library(factoextra)
library(gridExtra)
library(dbscan)
library(readxl)

#Data Preprocessing
kustodian1<-read_excel("C:/Users/ACER/Downloads/KUSTODIAN.xlsx")
kustodian2<-kustodian[,2:9]
#Melakukan standardisasi agar skala data seragam
kustodian3<-scale(kustodian2)
head(kustodian2)

#Scan berdasarkan densitas
kNNdistplot(kustodian3, k = 7)
abline(h = 2.75, lty = 2)

set.seed(123)
dbscan<- fpc::dbscan(kustodian3, eps = 2.75, MinPts = 7)

fviz_cluster(dbscan, data = kustodian3, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point", palette = "jco", ggtheme = theme_classic())

kNNdistplot(kustodian3, k = 4)  
abline(h = 2.9, lty = 2)

set.seed(123)
dbscan_<- fpc::dbscan(kustodian3, eps = 2.9, MinPts = 4)

fviz_cluster(dbscan_, data = kustodian3, stand = FALSE,
             ellipse = FALSE, show.clust.cent = FALSE,
             geom = "point", palette = "jco", ggtheme = theme_classic())

set.seed(123)
optics_kus<- optics(kustodian3, eps = 2.75, minPts = 7)

plot(optics_kus)
