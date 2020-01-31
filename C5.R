###################################################--C5.0--####################################################

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)


C5 <- train(diagnosis ~ ., 
             data = train, 
             method = "C5.0",
             metric="ROC",
             preProcess = c("center","scale"),
             #tuneLength = 5,
             trControl=fitControl
)



C5 
plot(C5)

#resultado de los K-Folds
C5$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(C5$resample$ROC)

pred_C5 <- predict(C5, test)
confusionMatrix(pred_C5, test$diagnosis, positive = "M")

#Accuracy : 0.9766  Sensitivity : 0.9565  Specificity : 0.9902  

confMC5 <- confusionMatrix(pred_knn_lda, test$diagnosis, positive = "M")
Accuracy <- round(confMC5$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='C5') 
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



C5_pca <- train(diagnosis ~ ., 
            data = train, 
            method = "C5.0",
            metric="ROC",
            preProcess = c("center","scale","pca"),
            tuneLength = 5,
            trControl=fitControl
)



C5_pca
plot(C5_pca)

#resultado de los K-Folds
C5$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(C5_pca$resample$ROC)

pred_C5_pca <- predict(C5_pca, test)
confusionMatrix(pred_C5_pca, test$diagnosis, positive = "M")

#Accuracy : 0.9649 Sensitivity : 0.9710  Sensitivity : 0.9710 

confMC52 <- confusionMatrix(pred_knn_lda, test$diagnosis, positive = "M")
Accuracy <- round(confMC52$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='C5-PCA') 
res <- rbind(res,new)

###################################################--LDA-####################################################


#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)


C5_lda <- train(diagnosis ~ ., 
            data = train_lda, 
            method = "C5.0",
            metric="ROC",
            preProcess = c("center","scale"),
            tuneLength = 5,
            trControl=fitControl
)



C5_lda 
plot(C5_lda)

#resultado de los K-Folds
C5_lda$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(C5_lda$resample$ROC)

pred_C5_lda <- predict(C5_lda, test_lda)
confusionMatrix(pred_C5_lda, test_lda$diagnosis, positive = "M")

#Accuracy : 0.9766  Sensitivity : 0.9855   Specificity : 0.9706  

confMC53 <- confusionMatrix(pred_knn_lda, test$diagnosis, positive = "M")
Accuracy <- round(confMC53$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='C5-LDA') 
res <- rbind(res,new)
