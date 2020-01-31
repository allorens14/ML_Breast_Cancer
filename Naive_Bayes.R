###################################################--Naive Bayes--####################################################

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)

grid <- data.frame(fL=c(0,0.5,1.0,1.5), usekernel = TRUE, adjust=c(0,0.5,1.0,1.5))


nb <- train(diagnosis ~ ., 
            data = train, 
            method = "nb",
            metric="ROC",
            preProcess = c("center","scale"),
            #tuneLength = 5,
            tuneGrid=grid,
            trControl=fitControl
)



nb 
#The final values used for the model were fL = 1, usekernel = TRUE and adjust = 1.

plot(nb)

#Importancia de la variables
plot(varImp(nb))

#resultado de los K-Folds
nb$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(nb$resample$ROC)

pred_nb <- predict(nb, test)
confusionMatrix(pred_nb, test$diagnosis, positive = "M")

#Accuracy : 0.9708   Sensitivity : 0.9710  Specificity : 0.9706   Kappa : 0.9394


confMNB <- confusionMatrix(pred_nb, test$diagnosis, positive = "M")
Accuracy <- round(confMNB$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='Naive Bayes') 
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

grid <- data.frame(fL=c(0,0.5,1.0), usekernel = TRUE, adjust=c(0,0.5,1.0))



nb_pca <- train(diagnosis ~ ., 
                data = train, 
                method = "nb",
                metric="ROC",
                preProcess = c("center","scale","pca"),
                tuneGrid=grid,
                #tuneLength = 5,
                trControl=fitControl
)



nb_pca
plot(nb_pca)

#resultado de los K-Folds
nb_pca$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(nb_pca$resample$ROC)

pred_nb_pca <- predict(nb_pca, test)
confusionMatrix(pred_nb_pca, test$diagnosis, positive = "M")

#Accuracy : 0.9357  Sensitivity : 0.8696  Specificity : 0.9804  Kappa : 0.8641


confMNB2 <- confusionMatrix(pred_nb_pca, test$diagnosis, positive = "M")
Accuracy <- round(confMNB2$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='Naive Bayes-PCA') 
res <- rbind(res,new)


###################################################--LDA-####################################################


#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)

grid <- data.frame(fL=c(0,0.5,1.0,1.5), usekernel = TRUE, adjust=c(0,0.5,1.0,1.5))


nb_lda <- train(diagnosis ~ ., 
                data = train_lda, 
                method = "nb",
                metric="ROC",
                preProcess = c("center","scale"),
                tuneLength = 5,
                tuneGrid=grid,
                trControl=fitControl
)



nb_lda 
plot(nb_lda)

#resultado de los K-Folds
nb_lda$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(nb_lda$resample$ROC)

pred_nb_lda <- predict(nb_lda, test_lda)
confusionMatrix(pred_nb_lda, test_lda$diagnosis, positive = "M")


#Accuracy : 0.9825    Sensitivity : 0.9565   Specificity : 1.0000   Kappa : 0.9635

confMNB3 <- confusionMatrix(pred_nb_lda, test_lda$diagnosis, positive = "M")
Accuracy <- round(confMNB3$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='Naive Bayes-LDA') 
res <- rbind(res,new)
