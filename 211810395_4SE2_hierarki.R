#Install Package terlebih dahulu jika belum tersedia di RStudio
#install.packages("factoextra")
#install.packages("gridExtra")
library(factoextra)
library(gridExtra)
library(readxl)

#Data Preprocessing
kustodian1<-read_excel("C:/Users/ACER/Downloads/KUSTODIAN.xlsx")
kustodian2<-kustodian[,2:9]
#Melakukan standardisasi agar skala data seragam
kustodian3<-scale(kustodian2)
head(kustodian2)

##mencari jumlah klaster optimal secara complete
fviz_nbclust(kustodian3,FUNcluster = hcut,method = "silhouette",
             hc_method = "complete",hc_metric = "euclidean")
##mencari jumlah klaster optimal secara average
fviz_nbclust(kustodian3,FUNcluster = hcut,method = "silhouette",
             hc_method = "average",hc_metric = "euclidean")
##mencari jumlah klaster optimal secara centroid
fviz_nbclust(kustodian3,FUNcluster = hcut,method = "silhouette",
             hc_method = "centroid",hc_metric = "euclidean")
##mencari jumlah klaster optimal secara ward
fviz_nbclust(kustodian3,FUNcluster = hcut,method = "silhouette",
             hc_method = "ward.D",hc_metric = "euclidean")

#Visualisasi dendogram cluster
linkage <- c("complete","average","centroid","ward.D")
cluster <- lapply(linkage, function(i) hclust(dist(kustodian3,method = 'euclidean'),
                                         method = i))
#dendogram complete, dengan k=2
fviz_dend(cluster[[1]])
#dendogram average, dengan k=2
fviz_dend(cluster[[2]])
#dendogram centroid, dengan k=2
fviz_dend(cluster[[3]])
#dendogram ward, dengan k=4
fviz_dend(cluster[[4]])

#Hierarchical Clustering k=2
hc <- eclust(kustodian3,stand = TRUE,FUNcluster = "hclust",k=2,hc_method = "complete",hc_metric = "euclidean",graph = F)
hc$cluster

#Visualisasi
aggregate(kustodian1,by =list(cluster=hc$cluster),
          FUN = mean)
fviz_cluster(hc)

#Interpretasi Principal Component
pca <- prcomp(kustodian3)
pca$rotation
