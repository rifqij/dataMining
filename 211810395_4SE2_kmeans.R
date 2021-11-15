#Install Package terlebih dahulu jika belum tersedia di RStudio
#install.packages("factoextra")
#install.packages("gridExtra")
library(factoextra)
library(gridExtra)
library(readxl)

#Data Preprocessing
kustodian<-read_excel("C:/Users/ACER/Downloads/KUSTODIAN.xlsx")
kustodian1<-kustodian[,2:9]
#Melakukan standardisasi agar skala data seragam
kustodian2<-scale(kustodian1)
head(kustodian2)

#mencari jumlah klaster optimal 
fviz_nbclust(kustodian2, kmeans, method = "wss") +
  labs(subtitle = "Penentuan K Elbow")
fviz_nbclust(kustodian2, kmeans, method = "silhouette") + 
  labs(subtitle = "Penentuan K Silhouette")
set.seed(123)
fviz_nbclust(kustodian2, kmeans, nstart=25, method = "gap_stat", nboot = 50) + 
  labs(subtitle = "Penentuan K Gap Statistic")

#Klasterisasi dengan k=2
set.seed(123)
km.kus <- kmeans(kustodian2, 2, nstart = 20)
km.kus
aggregate(kustodian, by=list(cluster=km.kus$cluster), mean)

#Visualisasi cluster yang terbentuk
fviz_cluster(km.kus, data = kustodian2, ellipse.type = "euclid", star.plot = TRUE,
             repel = TRUE, ggtheme = theme_minimal())
