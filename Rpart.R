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
                          method = "rpart2", 
                          tuneLength =10,
                          metric="ROC",
                          trControl = fitControl
  )

rpart

#resultado de los K-Folds
rpart$resample

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

#Como resultado se ha obtenido un 94.15% de precision,Kappa : 0.8768, Sensitivity : 0.8841 y         
#Specificity : 0.9804

#Es un resultado bueno pero no asombroso




