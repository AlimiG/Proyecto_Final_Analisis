library(dplyr)
library(corrplot)
library(psych)
library(stats) 
library(cowplot)
library(dplyr)
library(readr)
library(GGally)
library(ca)
library(FactoMineR)
library(factoextra)
setwd("C:/Users/Tus nalgas/Desktop/proyecto")
w = 6
h = 6
datos1=read.csv('Data/movies.csv')

grossby <- cut(datos1$gross/1000000,c(-1,30,80,250,938))
datos1<-cbind(datos1,grossby)
cont.table <- table(datos1$rating,datos1$grossby)
cont.table <- cont.table/sum(cont.table)
rel.ca <- ca(cont.table)
plot.ca(rel.ca,mass=c(T,T),col = c('red','blue'))
res.ca <- CA(cont.table,graph = T)

fviz_ca_biplot(res.ca, repel= T,invisible = c("row.sup","col.sup"))

#Realizamos el mismo análisis por género
cont.table <- table(datos1$genre,datos1$grossby)
cont.table <- cont.table/sum(cont.table)
rel.ca <- ca(cont.table)
plot.ca(rel.ca,mass=c(T,T),col = c('red','blue'))
res.ca <- CA(cont.table,graph = T)

fviz_ca_biplot(res.ca, repel= T,invisible = c("row.sup","col.sup"))

#Análisis por runtime
runnby <- cut(datos1$runtime,c(-1,75,100,125,367))
datos1<- cbind(datos1,runnby)
#Realizamos el mismo análisis por género
cont.table <- table(datos1$runnby,datos1$grossby)
cont.table <- cont.table/sum(cont.table)
rel.ca <- ca(cont.table)
plot.ca(rel.ca,mass=c(T,T),col = c('red','blue'))
res.ca <- CA(cont.table,graph = T)
