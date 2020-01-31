###################################################--GBM--####################################################
#Los parametros a tunear en este algoritmo son:

#shrinkage:             lo rápido que boostea
#n.minobsinnode:        número de registros mínimos para poder dividir un nodo
#n.trees:               número de árboles que se generan, es lo mismo que iteraciones
#interaction depth:     complejidad de los árboles

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary,
                           search = "random"
                           
)

gbmGrid <-  expand.grid(interaction.depth = c(1:8), 
                        n.trees = (1:10)*50, 
                        shrinkage = 0.1,
                        n.minobsinnode = c(10,20))

gbm <- train(diagnosis ~ ., 
            data = train, 
            method = "gbm",
            metric="ROC",
            preProcess = c("center","scale"),
            #tuneLength = 5,
            #tuneGrid=gbmGrid,
            trControl=fitControl
)



gbm 
#The final values used for the model:n.trees = 250, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10

plot(gbm)

#resultado de los K-Folds
gbm$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(gbm$resample$ROC)

pred_gbm <- predict(gbm, test)
confusionMatrix(pred_gbm, test$diagnosis, positive = "M")

#Accuracy : 0.9825 Sensitivity : 0.9710  Specificity : 0.9902 Kappa : 0.9635


confMGBM <- confusionMatrix(pred_gbm, test$diagnosis, positive = "M")
Accuracy <- round(confMGBM$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='GBM') 
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


confMGBM2<- confusionMatrix(pred_nb_pca, test$diagnosis, positive = "M")
Accuracy <- round(confMGBM2$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='GBM-PCA') 
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

confMGBM3 <- confusionMatrix(pred_nb_lda, test_lda$diagnosis, positive = "M")
Accuracy <- round(confMGBM3$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='GBM-LDA') 
res <- rbind(res,new)

