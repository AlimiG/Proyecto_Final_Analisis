library(dplyr)
library(corrplot)
library(psych)
library(stats) 
datos=read.csv('Data/movies.csv')
str(datos)

#Elimino los datos que no son influyentes
datos <- datos %>%
  select(budget,country,genre,gross,rating,runtime,score,votes,year)
str(datos)
attach(datos)
#Para las variables categóricas podemos realizar una tabla de frecuencias, es decir, el número de ocurrencias
#de cada categoría de la variable y esto lo realizamos con el comando table
table(datos$country)
table(datos$genre)
table(datos$rating)
table(datos$year)

#Ahora lo tabulamos por según dos variables:
table(datos$country, datos$genre)
table(datos$country, datos$rating)

#Para calcular los estadísticos de las variables cuantitativa utilizaremos el comando summary 
datos_num=datos[,c(1,4,6,7,8,9)]
summary(datos_num)



#Ahora realizamos los histogramas respectivos
hist(datos$budget/1000000,  main="Presupuestos")
hist(datos$gross, main="Dinero recaudado por película")
hist(datos$runtime, main="Duración por película")
hist(datos$score, main="Puntuación de la película")
hist(datos$votes, main="Cantidad de votantes por película")
hist(datos$year, main="Años de lanzamiento por película")

#Group by es uno de los comandos más útiles que tiene el paquete dplyr. Como su nombre lo dice nos permite agrupar variables 
#y hacer cálculos entre los grupos, por ejemplo calcular nuevas variables o estadísticos para los grupos.
datos %>%
  group_by(datos$country) %>%
  summarise(TNprom = mean(datos$gross))

datos %>%
  group_by(datos$country) %>%
  summarise(TNprom = mean(datos$score))

#Ahora vemos la matriz de correlaciones entre las variables numéricas
cor(datos_num)

#Tambien podemos ver la matriz de correlaciones graficamente 
crr = round(cor(datos_num),2)


corrplot(crr,method="number",type="upper")

#Uno de los anális a realizar en estos datos es e análisis de componentes principales

#Lo primero que hacemos es ver la dimensión de los datos con los cuales vamos a trabajar
dim(datos_num)

#Vemos el nombre de las columnas
names(datos_num) 

pairs(datos_num)
#A continuación utilizamos la función prcomp() para obtener las componentes principales:
pca.datos_num=princomp(datos_num,cor=T)
pca.datos_num
summary(pca.datos_num)

plot(pca.datos_num)
screeplot(pca.datos_num,type="lines")

par(mfrow=c(2,2), bg="azure")
barplot(loadings(pca.datos_num)[,1],col="blue2",sub="Primera componente")
barplot(loadings(pca.datos_num)[,2],col="orange",sub="Segunda componente")
barplot(loadings(pca.datos_num)[,3],col="blue2",sub="tercera componente")
barplot(loadings(pca.datos_num)[,4],col="orange",sub="cuarta componente")

barplot(loadings(pca.datos_num)[,4],col="blue2",sub="quinta componente")


