library(dplyr)
library(corrplot)
library(psych)
library(stats) 
library(cowplot)
library(dplyr)
library(readr)
library(GGally)
library(factoextra)
library(xtable)
h = 6
w = 6
datos1 <- read.csv("Data/movies.csv")
head(datos)
options(round(4))
datos <- datos1 %>%
  select(budget,country,genre,gross,rating,runtime,score,votes,year)

numeric_data <- datos %>%
  select(budget,gross,runtime,score,votes)
numeric_data <- scale(numeric_data)

# Por rating
nd = aggregate(numeric_data,by=list(rating=datos$rating),mean)
nd
numeric.values = nd[,2:6]
rownames(numeric.values) = nd[,1]
numeric.values
# Matriz de distancias y heatmap
disty <- get_dist(numeric.values,method = "pearson")
fviz_dist(disty)+ ggtitle("Distancia entre Ratings")
ggsave('Output/Images/13heatmaprating.eps', width = w+3, height = h)
#Prueba de numero de clusters
fviz_nbclust(numeric.values, 
             kmeans,verbose =T, 
             k.max = 4,
             method = "wss",nboot = 1)+
  geom_vline(xintercept = 3, linetype = 2)+
  ggtitle("Número óptimo de clusters")




solucionKmeans1 <- eclust(numeric.values,
                          FUNcluster = "kmeans",
                          k = 3, #como lo hicimos previamente
                          graph =T, hc_metric = "pearson")




#Jerarquico
res.hc <- hclust(d = disty,method ="ward.D2")
fviz_dend(res.hc, k = 3, # Cut in four groups
          cex = 1, # label size
          k_colors = c("#2E9FDF", "#00AFBB", "#E7B800", "#FC4E07"),
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE # Add rectangle around groups
) + ggtitle("Dendograma de los grupos")
ggsave('Output/Images/15dendogramarating.eps', width = w+3, height = h)








# Por genero


nd = aggregate(numeric_data,by=list(rating=datos$genre),mean)
nd
numeric.values = nd[,2:6]
rownames(numeric.values) = nd[,1]
numeric.values
# Matriz de distancias y heatmap
disty <- get_dist(numeric.values,method = "pearson")
fviz_dist(disty)+ ggtitle("Distancia entre Géneros")
ggsave('Output/Images/16heatmapgenre.eps', width = w+3, height = h)
#Prueba de numero de clusters
fviz_nbclust(numeric.values, 
             kmeans,verbose =T, 
             k.max = 12,
             method = "wss",nboot = 1)+
  geom_vline(xintercept = 7, linetype = 2)+
  ggtitle("Número óptimo de clusters")


ncong = NbClust(data=numeric.values, distance = NULL, diss =disty, min.nc=2,
                max.nc=10, method = "kmeans", index = "alllong")



solucionKmeans1 <- eclust(numeric.values,
                          FUNcluster = "kmeans",
                          k = 7, #como lo hicimos previamente
                          graph =T, hc_metric = "pearson")
ggsave('Output/Images/17kmeansgenero.eps', width = w+3, height = h)






#Jerarquico
res.hc <- hclust(d = disty,method ="ward.D2")
fviz_dend(res.hc, k = 7, # Cut in four groups
          cex = 1, # label size
          color_labels_by_k = TRUE, # color labels by groups
          rect = TRUE,
          rect_fill = T,
          k_colors = "simpsons",
          rect_border = "simpsons",
          type = "circular"
          # Add rectangle around groups
) + ggtitle("Dendograma de los grupos")
ggsave('Output/Images/18dendogramagenero.eps', width = w+3, height = h)


