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

library(DataExplorer)
plot_bar(data[1])

ggplot(data,aes(data$diagnosis,fill=data$diagnosis))+ geom_bar(stat="count") +
  scale_fill_manual(values=c("olivedrab3","brown2"))

plot_histogram(data[,c(2:11)])
plot_histogram(data[,c(12:21)])
plot_histogram(data[,c(22:31)])

#Podemos observar en estos histogramas que todas las variables se asemejan a una distribución normal, no encontramos
#grandes repechos o situaciones anormales. Esto sera importante a la hora de analizar los boxplots.

#Creo que puede ser interesante observar los histogramas diferenciando los valores malignos y benignos.
data_est <- data %>% standardize()

data_union1 <- data_est[c(-12:-31)] %>% gather("variable","value",-diagnosis)
data_union2 <- data_est[c(c(-22:-31),c(-2:-11))]  %>% gather("variable","value",-diagnosis)  
data_union3 <- data_est[c(-2:-21)] %>% gather("variable","value",-diagnosis)

ggplot(data_union1,aes(value,fill=diagnosis))+ geom_density(alpha=0.5)+
  facet_wrap(~variable, scales = "fixed")

ggplot(data_union2,aes(value,fill=diagnosis))+ geom_density(alpha=0.5)+
  facet_wrap(~variable, scales = "fixed")

ggplot(data_union3,aes(value,fill=diagnosis))+ geom_density(alpha=0.5)+
  facet_wrap(~variable, scales = "fixed")

#Se puede observar como por ejemplo la variable "fractal_dimension_mean" no va a aportar mucha información a nuestro
#algoritmo al tener distribuciones practicamente solapadas. No hay variabilidad.


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


#Unas medias igualadas entre maligno y benigno nos indican que esa variable no le va a ser realmente útil a nuestro 
#algoritmo para determinar el output, ya que los valores de esa variable no son determinantes.Esto solamente se 
#puede decir si las variables siguen una distribución normal o se asemeja a ella.



#################################################--CORRELATION-PLOTS--##########################################


#Ahora vamos a comprobar la relación existente entre las variables. Para ello primero realizo tres "correaliation plot".
#Uno por cada tipo de dato (mean,se,worst). Esta division se hace por tres motivos, por logica, ya que los datos
#se dividen asi por su naturaleza de origen y por temas de visualización, intentar ver la correlación de las 
#30 variables en un mismo a la vez resulta complicado.

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


#Otro gráfico de correlación, este es mucho mas claro y simple.(solo coeficientes de correlación)
library(GGally)
ggcorr(data[,c(2:11)],label = TRUE, hjust = 0.82,size=3)
ggcorr(data[,c(12:21)],label = TRUE, hjust = 0.82,size=3)
ggcorr(data[,c(22:31)],label = TRUE, hjust = 0.82,size=3)


#Observando todas estas gráficas se ve claramente que existen multiples variables que estan correlacionadas.
#Todas aquellos pares de variables que tengan un indice de correlación alto, siendo el maximo 1, estan aportando
#la misma información a nuestro algoritmo, por ello no es necesario tener ambas.



#################################################--PCA--####################################################

#U número elevado de variables puede resultar en problemas, tanto de rendimineto como de resultado, asi como
#visualizaciones mas complejas e interpretaciones más complicadas.

#Ahora que sabemos que no todas las variables aportan valor y que pueden ser eliminadas de nuestro dataset vamos a
#ver mediante el el método PCA de que forma podemos simplificar nuestros datos.

#Este método nos permite conocer la importancia de las variables (proporcion de la varianza). Una proporción por
#encima del 85% resultará suficiente para no necesitar más variables.

#En este momento se analizara PCA, simplemente para ver que conclusiones se sacan, pero si se dedice utilizar PCA
#finalmente, se aplicará al dataset mas adelante, cuando se vayan a entrenar los algoritmos.

pca <- prcomp(data[,-1], scale = TRUE,center=TRUE)
summary(pca)
get_pca(pca) 

#Al utilizar PCA vemos que PC1~PC6 tiene una proporcion acumulada de +88%. Con 6 variables se pueden explicar
#el 88,75% de los datos.


# % de importancia por componente para los 15 primeros
library(factoextra)
fviz_eig(pca, addlabels=TRUE, ylim=c(0,60), geom = c("bar", "line"), barfill = "pink", 
         barcolor="grey",linecolor = "red", ncp=15)+
  labs(title = "Cancer All Variances - PCA",
       x = "Principal Components", y = "% of variances")



#Varianza explicada Acumulada
varianza <- pca$sdev^2/sum(pca$sdev^2)
varianza_acumulado <- cumsum(varianza)

varianza_dataframe <- data.frame(varianza_acumulado,pc=factor(1:30))

ggplot(data=varianza_dataframe[,c(1:15)],aes(x=pc,y=varianza_acumulado, group = 1))+
  geom_point()+geom_line()+ geom_label(aes(label = round(varianza_acumulado,2))) 



#Biplot con las dos componentes principales y con los dos grupos target. Se aprecian dos clusters claramente 
#diferenciados
fviz_pca_biplot(pca,col.ind = data$diagnosis, col="black",
                palette = "jco", geom = "point", repel=TRUE,
                legend.title="Diagnosis", addEllipses = TRUE)


   
###################################################--t-SNE--#######################################################

#Ahora voy utilizar el metodo t-SNE (stochastic neighbor embedding), el cual podria mejorar lo obtenido por PCA
#ya que PCA es linear y no es capaz de encontrar otro tipo de relaciones entre variables.


library(Rtsne)

tsne <- Rtsne(data,is_distance = FALSE, dims = 2, perplexity = 30,
              theta = 0.5, max_iter = 500)
head(tsne$Y)

res <- as.data.frame(tsne$Y)

colnames(res) <- c("dim_1", "dim_2")
res$numero <- as.character(data$diagnosis)


ggplot(data = res, aes(x = dim_1, y = dim_2)) +
  geom_point(aes(color = numero),size=3,alpha=0.5) + 
  theme_bw()


