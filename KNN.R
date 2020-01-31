###################################################--KNN--####################################################
#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)


knn <- train(diagnosis ~ ., 
                data = train, 
                method = "knn",
                metric="ROC",
                preProcess = c("center","scale"),
                tuneLength = 20,
                trControl=fitControl
             )



knn
plot(knn)

#resultado de los K-Folds
knn$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(knn$resample$ROC)

pred_knn <- predict(knn, test)
confusionMatrix(pred_knn, test$diagnosis, positive = "M")

#Accuracy : 0.9825 Sensitivity : 0.9565  Specificity : 1.0000 


confMKNN <- confusionMatrix(pred_knn, test$diagnosis, positive = "M")
Accuracy <- round(confMKNN$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='KNN') 
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



knn_pca <- train(diagnosis ~ ., 
             data = train, 
             method = "knn",
             metric="ROC",
             preProcess = c("center","scale","pca"),
             tuneLength = 20,
             trControl=fitControl
)

knn_pca
plot(knn_pca)
pred_knn_pca <- predict(knn_pca, test)
confusionMatrix(pred_knn_pca, test$diagnosis, positive = "M")

#Accuracy : 0.9708  Sensitivity : 0.9275   Specificity : 1.0000

confMKNN2 <- confusionMatrix(pred_knn_pca, test$diagnosis, positive = "M")
Accuracy <- round(confMKNN2$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='KNN-PCA') 
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
knn_lda <- train(diagnosis~., 
                   data = train_lda,
                   method = "knn", 
                   tuneLength =10,
                   metric='ROC',
                   trControl = fitControl
)


knn_lda
plot(knn_lda)
pred_knn_lda <- predict(knn_lda, test_lda)
confusionMatrix(pred_knn_lda, test$diagnosis, positive = "M")


#Accuracy : 0.9883 Sensitivity : 0.9710 Specificity : 1.0000


confMKNN3 <- confusionMatrix(pred_knn_lda, test$diagnosis, positive = "M")
Accuracy <- round(confMKNN3$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='KNN-LDA') 
res <- rbind(res,new)
