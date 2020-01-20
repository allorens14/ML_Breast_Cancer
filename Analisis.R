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



#Comprobamos el numero de NAs, al salir 0 no es necesario mas analisis de los NAs, dos funciones diferentes
sum(is.na(data))

library(VIM)
aggr(data, prop = FALSE, combined = TRUE, numbers = TRUE, sortVars = TRUE, sortCombs = TRUE)

library(DataExplorer)
plot_missing(data)



#Ahora que ya tenemos una idea general de los datos y su extructura podemos pasar al analisis


##################################################--HISTOGRAMAS--#################################################

#Lo primero es ver las frecuencias de cada una de las variables

#library(DataExplorer)
plot_bar(data[1])

ggplot(data,aes(data$diagnosis,fill=data$diagnosis))+ geom_bar(stat="count") +
  scale_fill_manual(values=c("olivedrab3","brown2"))

plot_histogram(data[,c(2:11)])
plot_histogram(data[,c(12:21)])
plot_histogram(data[,c(22:31)])



####################################################--BOXPLOTS--#################################################


#Para poder visualizar correctamente las diferentes variables es necesario estandarizar los valores, de esta forma
#podran compartir ejes y las diferencias podran ser comparables
library(psycho)
data_est <- data %>% standardize()

data_union1 <- data_est[c(-12:-31)] %>% gather("variable","value",-diagnosis)
data_union2 <- data_est[c(c(-22:-31),c(-2:-11))]  %>% gather("variable","value",-diagnosis)  
data_union3 <- data_est[c(-2:-21)] %>% gather("variable","value",-diagnosis)  

ggplot(data_union1,aes(y=value,fill=diagnosis))+ facet_wrap(~variable, scales = "fixed") + geom_boxplot() 
ggplot(data_union2,aes(y=value,fill=diagnosis))+ facet_wrap(~variable, scales = "fixed") + geom_boxplot() 
ggplot(data_union3,aes(y=value,fill=diagnosis))+ facet_wrap(~variable,scales = "fixed") + geom_boxplot() 



#################################################--CORRELATION-PLOTs--##########################################


#Ahora vamos a comprobar la relación existente entre las variables. Para ello primero realizo tres "correaliation plot".
#Uno por cada tipo de dato (mean,se,worst).
library(psych)
pairs.panels(data[,c(2:11)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE, scale = TRUE,
             pch=1, lm=TRUE, cex.cor=3, smoother=F, stars = T, main="Cancer Mean")

pairs.panels(data[,c(12:21)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE, scale = TRUE,
             pch=1, lm=TRUE, cex.cor=3, smoother=F, stars = T, main="Cancer Mean")

pairs.panels(data[,c(22:31)], method="pearson",
             hist.col = "#1fbbfa", density=TRUE, ellipses=TRUE, show.points = TRUE, scale = TRUE,
             pch=1, lm=TRUE, cex.cor=3, smoother=F, stars = T, main="Cancer Mean")



#Creo que es importante en este caso ver los graficos mostrando tambien la variable objetivo
library(GGally)
ggpairs(data[,c(2:11,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggpairs(data[,c(12:21,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))

ggpairs(data[,c(22:31,1)], aes(color=diagnosis, alpha=0.75), lower=list(continuous="smooth"))+ theme_bw()+
  labs(title="Cancer Mean")+
  theme(plot.title=element_text(face='bold',color='black',hjust=0.5,size=12))




