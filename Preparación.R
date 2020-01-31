library(caret)
library(ggplot2)
library(ggthemes)
library(tidyverse)


#Fijamos la semilla para que los resultados sean repetibles
set.seed(543)

#Importamos los datos, provenientes de un csv localizado en la raiz del proyecto
data<-read.csv("data.csv")

#Quitamos la ultima variable porque esta vacia y la primera porque es el id (no es util)
data <- data[,-33]
data <- data[-1]


#Mezclo y divido el dataset
index <- sample(1:NROW(data), 0.7 * NROW(data))   
train <- data[index,]                   ## 398 test data (70%)
test <- data[-index,]                   ## 171 test data (30%)
prop.table(table(test$diagnosis))
prop.table(table(train$diagnosis))


#Creo dos nuevos dataset de train y test para poder utilizar los algoritmos con LDA. Creo mi modelo LDA sobre
#sobre mi dataset de train, despues predigo sobre mi modelo LDA de train y convierto en dataframe, de esta forma
#obtengo mi dataset de train-lda.
library(MASS)
lda <- lda(diagnosis ~., data = train, center = TRUE, scale = TRUE)
train_lda <- predict(lda, train)$x %>% as.data.frame() %>% cbind(diagnosis=train$diagnosis)

#Ahora creo mi dataframe de test-lda. Para ello predigo sobre test con mi modelo lda ya creado, de esta forma evito
#introducir bias.
test_lda <- predict(lda, test)$x %>% as.data.frame() %>% cbind(diagnosis=test$diagnosis)

