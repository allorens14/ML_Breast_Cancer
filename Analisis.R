library(caret)
library(ggplot2)
library(ggthemes)
library(tidyverse)


#Importamos los datos, provenientes de un csv localizado en la raiz del proyecto
data<-read.csv("data.csv")

#Inspeccionamos el numero de variables y sus estadisticas
head(data)
summary(data)
str(data)


#Quitamos la ultima variable porque esta vacia y la primera porque es el id (no es util)
data <- data[,-33]
data <- data[-1]

#Comprobamos el numero de NAs, al salir 0 no es necesario mas analisis de los NAs
sum(is.na(data))
library(VIM)
aggr(data, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)


#Ahora vamos a comprobar la relación existente entre las variables. Para ello primero realizo tres "correaliation plot".
#Uno por cada tipo de dato (mean,se,worst).
library(psych)
pairs.panels(data[,c(2:11)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE,
             pch=1, lm=TRUE, cex.cor=1, smoother=F, stars = T, main="Cancer Mean")

pairs.panels(data[,c(12:21,1)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE,
             pch=1, lm=TRUE, cex.cor=1, smoother=F, stars = T, main="Cancer Mean")

pairs.panels(data[,c(22:31,1)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE,
             pch=1, lm=TRUE, cex.cor=1, smoother=F, stars = T, main="Cancer Mean")




library(GGally)
ggpairs(data[,c(2:11,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))
