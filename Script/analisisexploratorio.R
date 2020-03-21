library(dplyr)
library(corrplot)
library(psych)
library(stats) 
library(cowplot)
library(dplyr)
library(readr)
library(GGally)

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
  ggtitle("Número de producciones dentro y fuera de USA")
  ggsave('Output/Images/1barplotpaises.png', width = w, height = h)
# Vemos que USA lidera el numero de producciones por mucho frente a las
# Extranjeras
# Analizamos por genero
table(datos$genre)
ggplot(datos, aes(x=genre))+
  geom_bar(show.legend = F)+
  ggtitle("Número de producciones por género")+
  xlab("Género")+ylab("Número de producciones")
ggsave('Output/Images/2barplotgenero.png', width = w+3, height = h)



#graficamos la ganancia contra el genero 
ggplot(datos,aes(x=genre,y=gross/1000000, color=genre)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="Género", y = "Ganancia (M$)")+
  ggtitle("Ganancia de las producciones por género")+
  scale_fill_discrete(name = "Compra", labels = c("No", "Si")) + theme_bw()
ggsave('Output/Images/3gananciasporgenero.png', width = w+2, height = h+2)





# Matriz de correlacion entre variables numericas
sm <- ggpairs(datos, columns = c(1,4,6,7,8), title = "Correlación entre variables numéricas",
              mapping=ggplot2::aes(colour = rating,alpha = 1.4), axisLabels = "show")
sm
ggsave('Output/Images/6correlacionmatriz.png', width = w, height = h)


## Analisis variable rating
table(datos$rating)


datos %>%
  ggplot(aes(x = rating, fill = rating))+
  geom_bar()+ ggtitle("Número de producciones por Rating")
ggsave('Output/Images/7produccionesporrating.png', width = w+2, height = h+2)

ggplot(datos,aes(x=rating,y=gross/1000000, color=rating)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="Rating", y = "Ganancia (M$)")+
  ggtitle("Ganancia de las producciones por Rating")+
  theme_bw()
ggsave('Output/Images/8gananciaporrating.png', width = w+2, height = h+2)

# Analisis variable runtime
datos %>%
  ggplot(aes(x = runtime)) +
  geom_histogram(binwidth = 8)+
  xlim(50,250) + xlab("Duración") + 
  ylab("Número de producciones") +
  ggtitle("Número de producciones por Duración")
ggsave('Output/Images/4histduracion.png', width = w, height = h)

#Analisis variable score
datos %>%
  ggplot(aes(x = score)) +
  geom_bar()+
   xlab("Score") + ylab("Número de producciones") +
  ggtitle("Número de producciones por Score")
ggsave('Output/Images/5barplotscore.png', width = w, height = h)




ggplot(datos,aes(x=rating,y=score, color=rating)) +
  geom_boxplot(outlier.shape = NA,show.legend = F) + geom_jitter(width=0.1,alpha=1,show.legend = F)+
  labs(x="Rating", y = "Score")+
  ggtitle("Score de las producciones por Rating")+
  theme_bw()
ggsave('Output/Images/9scoreporrating.png', width = w+2, height = h+2)
datos %>%
  ggplot(aes(x = votes/1000)) +
  geom_histogram()+
  xlab("Votos") + ylab("Número de Votos")+
  ggtitle("Cantidad de votos")
ggsave('Output/Images/10votos.png', width = w+2, height = h+2)

