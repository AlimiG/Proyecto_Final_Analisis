library(dplyr)
library(corrplot)
library(psych)
library(stats) 
datos=read.csv('Data/movies.csv')
str(datos)

nombres=datos$name
#Elimino los datos que no son influyentes
datos=datos[,c(-2,-4,-7,-9,-12,-14)]
str(datos)

#Para las variables categ�ricas podemos realizar una tabla de frecuencias, es decir, el n�mero de ocurrencias
#de cada categor�a de la variable y esto lo realizamos con el comando table
table(datos$country)
table(datos$genre)
table(datos$rating)
table(datos$year)

#Ahora lo tabulamos por seg�n dos variables:
table(datos$country, datos$genre)
table(datos$country, datos$rating)

#Para calcular los estad�sticos de las variables cuantitativa utilizaremos el comando summary 
datos_num=datos[,c(1,4,6,7,8,9)]
summary(datos_num)

#Ahora realizamos los histogramas respectivos
hist(datos$budget,  main="Presupuestos")
hist(datos$gross, main="Dinero recaudado por pel�cula")
hist(datos$runtime, main="Duraci�n por pel�cula")
hist(datos$score, main="Puntuaci�n de la pel�cula")
hist(datos$votes, main="Cantidad de votantes por pel�cula")
hist(datos$year, main="A�os de lanzamiento por pel�cula")

#Group by es uno de los comandos m�s �tiles que tiene el paquete dplyr. Como su nombre lo dice nos permite agrupar variables 
#y hacer c�lculos entre los grupos, por ejemplo calcular nuevas variables o estad�sticos para los grupos.
datos %>%
  group_by(datos$country) %>%
  summarise(TNprom = mean(datos$gross))

datos %>%
  group_by(datos$country) %>%
  summarise(TNprom = mean(datos$score))

#Ahora vemos la matriz de correlaciones entre las variables num�ricas
cor(datos_num)

#Tambien podemos ver la matriz de correlaciones graficamente 
crr = round(cor(datos_num),2)


corrplot(crr,method="number",type="upper")

#-------------------An�lisis de componentes principales-------------------------------#

#Uno de los an�lis a realizar en estos datos es e an�lisis de componentes principales

#Lo primero que hacemos es ver la dimensi�n de los datos con los cuales vamos a trabajar
dim(datos_num)

#Vemos el nombre de las columnas
names(datos_num) 

pairs(datos_num)
#A continuaci�n utilizamos la funci�n prcomp() para obtener las componentes principales:
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


#-----------------------Escalamiento multidimensional-----------------------#
#El primer escalamiento multidimensional es para Estados unidos
datosUsa = subset(datos, datos$country == 'USA')
num_usa=datosUsa[,c(1,4,6,7,8)]
dist_usa=dist(num_usa)
dist_usa=scale(dist_usa)
usa.mds <- cmdscale(dist_usa, k=2, eig=T)

plot(usa.mds$points[,1],usa.mds$points[,2], pch='.',xlab=' ', ylab=' ')
text(usa.mds$points[,1],usa.mds$points[,2], datosUsa$genre, pos=4,cex=0.75) 

#El segundo escalmiento mmultidimensional es para los datos que no son estados unidos
datosrest = subset(datos, datos$country != 'USA')
str(datosrest)

datosrest_num=datosrest[,c(1,4,6,7,8)]

dist_rest=dist(datosrest_num)
dist_rest=scale(dist_rest)
rest.mds <- cmdscale(dist_rest, k=2, eig=T)

plot(rest.mds$points[,1],rest.mds$points[,2], pch='.',xlab=' ', ylab=' ')
text(rest.mds$points[,1],rest.mds$points[,2], pos=4,cex=0.75) 

#Ahora un escalamiento multidimensional soslo del a�o 2016
datos2016 = subset(datos, datos$year == '2016')
datos2106_num=datos2016[,c(1,4,6,7,8)]

dist_2016=dist(datos2106_num)
dist_2016=scale(dist_2016)
dist2016.mds <- cmdscale(dist_2016, k=2, eig=T)

plot(dist2016.mds$points[,1],dist2016.mds$points[,2], pch='.',xlab=' ', ylab=' ',col = datos2016$rating)
text(dist2016.mds$points[,1],dist2016.mds$points[,2], pos=4,cex=0.75) 
