###################################################--Rpart--####################################################

#Primero Voy a crear un modelo de tipo CART (árbol simple) con el conjunto total de los datos, despues se probará
#con PCA y LDA

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)


#Creamos el modelo con el set de entrenamiento, como metodo un arbol tipo CART (rpart) y como metrica a mejorar ROC
rpart <- train(diagnosis~., 
                          data = train ,
                          method = "rpart", 
                          tuneLength =10,
                          metric="ROC",
                          trControl = fitControl
  )

rpart

#resultado de los K-Folds
rpart$resample

#Compruebo la dispersión de los valores del ROC para la mejor 'cp' durante la validación (K-folds)
hist(rpart$resample$ROC)

#Pinta la gráfica relativa a como cambia la sensibildiad dependiendo del parametro cp(rpart) o maxdepth(rpart2)
plot(rpart)

#Imprime el arbol en plan bonito
library(rattle)
fancyRpartPlot(rpart$finalModel)

#Predigo los resultados del set de test y compruebo con la confusionMatrix el rendimiento del algoritmo
rpart2 <- predict(rpart,test)

confusionMatrix(rpart2, test$diagnosis, positive = "M")

#Como resultado se ha obtenido un 94.15% de precision,Kappa : 0.8768, Sensitivity : 0.8841 y         
#Specificity : 0.9804

#Es un resultado bueno pero no asombroso


confMRP <- confusionMatrix(rpart2, test$diagnosis, positive = "M")
Accuracy <- round(confMRP$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='Rpart') 
res <- rbind(res,new)

###################################################--PCA--####################################################

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           summaryFunction = twoClassSummary
)


#Creamos el modelo con el set de entrenamiento, como metodo un arbol tipo CART (rpart) y como metrica a mejorar ROC
rpart <- train(diagnosis~., 
               data = train,
               method = "rpart", 
               tuneLength =10,
               metric='ROC',
               preProcess = c('center', 'scale','pca'),
               trControl = fitControl
               )

rpart

#Pinta la gráfica relativa a como cambia la sensibildiad dependiendo del parametro cp(rpart) o maxdepth(rpart2)
plot(rpart)

#Imprime el arbol en plan bonito
library(rattle)
fancyRpartPlot(rpart$finalModel)

#Predigo los resultados del set de test y compruebo con la confusionMatrix el rendimiento del algoritmo
rpart2 <- predict(rpart,test)

confusionMatrix(rpart2, test$diagnosis, positive = "M")

#Como resultado se ha obtenido un 94.15% de precision (lo mismo que sin PCA),Kappa : 0.8779, 
#Sensitivity : 0.9130 (mayor al anterior) y Specificity : 0.9608 (2% menor)

#Es un resultado muy parecido al anterior con todos los datos


confMRP2 <- confusionMatrix(rpart2, test$diagnosis, positive = "M")
Accuracy <- round(confMRP2$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='Rpart-PCA') 
res <- rbind(res,new)

###################################################--LDA-####################################################



#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)


#Creamos el modelo con el set de entrenamiento, como metodo un arbol tipo CART (rpart) y como metrica a mejorar ROC
rpart_lda <- train(diagnosis~., 
               data = train_lda,
               method = "rpart", 
               tuneLength =10,
               metric='ROC',
               trControl = fitControl
)

rpart_lda

#Pinta la gráfica relativa a como cambia la sensibildiad dependiendo del parametro cp(rpart) o maxdepth(rpart2)
plot(rpart_lda)

#Imprime el arbol en plan bonito
library(rattle)
fancyRpartPlot(rpart_lda$finalModel)

#Predigo los resultados del set de test y compruebo con la confusionMatrix el rendimiento del algoritmo
rpart_lda_2 <- predict(rpart_lda,test_lda)

confusionMatrix(rpart_lda_2, test_lda$diagnosis, positive = "M")

#Como resultado se ha obtenido un 95.32% de precision (el mejor de los 3),Kappa : 0.9044, 
#Sensitivity : 0.9583 (la mejor) y Specificity : 0.9495



confMRP3 <- confusionMatrix(rpart_lda_2, test_lda$diagnosis, positive = "M")
Accuracy <- round(confMRP3$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='Rpart-LDA') 
res <- rbind(res,new)

