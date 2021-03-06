library(dplyr)
library(corrplot)
library(psych)
library(stats) 
library(cowplot)
library(dplyr)
library(readr)
library(GGally)
setwd("C:/Users/Tus nalgas/Desktop/proyecto")
w = 6
h = 6
datos1=read.csv('Data/movies.csv')
rownames(datos1) <- datos$name

datos <- datos1 %>%
  select(budget,country,genre,gross,rating,runtime,score,votes,year)

str(datos)
head(datos)
attach(datos)

# Analizamos los paises
datos %>%
  count(country != "USA")

df <- data.frame(
  Country <- c("USA","Extranjera"),
  num <- c(4652,1855)
)

ggplot( data = df,aes(x = Country, y = num, fill = Country)) +
  geom_bar(stat = "identity")+
  theme(legend.position = "none")+
  ylab("Numero de Producciones")+
  xlab("Procedencia de las producciones")+
  ggtitle("N�mero de producciones dentro y fuera de USA")
  ggsave('Output/Images/1barplotpaises.eps', width = w, height = h)
# Vemos que USA lidera el numero de producciones por mucho frente a las
# Extranjeras
# Analizamos por genero
table(datos$genre)
ggplot(datos, aes(x=genre))+
  geom_bar(show.legend = F)+
  ggtitle("N�mero de producciones por g�nero")+
  xlab("G�nero")+ylab("N�mero de producciones")
ggsave('Output/Images/2barplotgenero.eps', width = w+3, height = h)



#graficamos la ganancia contra el genero 
ggplot(datos,aes(x=genre,y=gross/1000000, color=genre)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="G�nero", y = "Ganancia (M$)")+
  ggtitle("Ganancia de las producciones por g�nero")+
  scale_fill_discrete(name = "Compra", labels = c("No", "Si")) + theme_bw()
ggsave('Output/Images/3gananciasporgenero.eps', width = w+2, height = h+2)





# Matriz de correlacion entre variables numericas
sm <- ggpairs(datos, columns = c(1,4,6,7,8), title = "Correlaci�n entre variables num�ricas",
              mapping=ggplot2::aes(colour = rating), axisLabels = "show")
sm
ggsave('Output/Images/6correlacionmatriz.eps', width = w, height = h)


## Analisis variable rating
table(datos$rating)


datos %>%
  ggplot(aes(x = rating, fill = rating))+
  geom_bar()+ ggtitle("N�mero de producciones por Rating")
ggsave('Output/Images/7produccionesporrating.eps', width = w+2, height = h+2)

ggplot(datos,aes(x=rating,y=gross/1000000, color=rating)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="Rating", y = "Ganancia (M$)")+
  ggtitle("Ganancia de las producciones por Rating")+
  theme_bw()
ggsave('Output/Images/8gananciaporrating.eps', width = w+2, height = h+2)

# Analisis variable runtime
datos %>%
  ggplot(aes(x = runtime)) +
  geom_histogram(binwidth = 8)+
  xlim(50,250) + xlab("Duraci�n") + 
  ylab("N�mero de producciones") +
  ggtitle("N�mero de producciones por Duraci�n")
ggsave('Output/Images/4histduracion.eps', width = w, height = h)

#Analisis variable score
datos %>%
  ggplot(aes(x = score)) +
  geom_bar()+
   xlab("Score") + ylab("N�mero de producciones") +
  ggtitle("N�mero de producciones por Score")
ggsave('Output/Images/5barplotscore.eps', width = w, height = h)




ggplot(datos,aes(x=rating,y=score, color=rating)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="Rating", y = "Score")+
  ggtitle("Score de las producciones por Rating")+
  theme_bw()
ggsave('Output/Images/9scoreporrating.eps', width = w+2, height = h+2)
datos %>%
  ggplot(aes(x = votes/1000)) +
  geom_histogram()+
  xlab("Votos") + ylab("N�mero de Votos")+
  ggtitle("Cantidad de votos")
ggsave('Output/Images/10votos.eps', width = w+2, height = h+2)




ggplot(datos,aes(x=rating,y=budget/1000000, color=rating)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="Rating", y = "Budget")+
  ggtitle("Presupuesto de las producciones por Rating")+
  theme_bw()
ggsave('Output/Images/11budgetporrating.eps', width = w+2, height = h+2)




ggplot(datos,aes(x=genre,y=budget/1000000, color=genre)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="Rating", y = "Budget")+
  ggtitle("Presupuesto de las producciones por Rating")+
  theme_bw()
ggsave('Output/Images/12budgetporgenre.eps', width = w+2, height = h+2)
 


