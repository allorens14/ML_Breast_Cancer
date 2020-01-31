###################################################--SVM--####################################################
#Aqui tenemos dos posibles algoritmos SVM a utilizar, uno lineal y otro con un kernel radial (svmLinear y svmRadial)
#El lineal tiene solo un parametro de ajuste; C (coste)
#SVM con kernel radial tiene dos: C y sigma(rotación)

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary

)

grid <-  expand.grid(C = c(0.05,0.1,0.15,0.2,0.5,0.75,1.0,1.2))
                        

svm <- train(diagnosis ~ ., 
             data = train, 
             method = "svmLinear",
             metric="ROC",
             preProcess = c("center","scale"),
             trControl=fitControl,
             tuneGrid = grid
             #tuneLength=10
             
)


svm 
#The final values used for the model:n.trees = 250, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10

plot(svm)

#resultado de los K-Folds
svm$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(svm$resample$ROC)

pred_svm <- predict(svm, test)
confusionMatrix(pred_svm, test$diagnosis, positive = "M")


#Accuracy : 0.9942 Sensitivity : 1.0000   Specificity : 0.9902 Kappa : 0.9879 

confMSVM <- confusionMatrix(pred_svm, test$diagnosis, positive = "M")
Accuracy <- round(confMSVM$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='SVM') 
res <- rbind(res,new)


###################################################--SVM-Radial--####################################################

#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 5,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           
)

grid <-  expand.grid(C = c(0.05,0.1,0.2,0.6,1.0,1.1,1.2),
                     
                    sigma= c(0,0.01, 0.02, 0.025, 0.04, 0.05, 0.07, 0.09, 0.1, 0.25, 0.5,0.9)
                    )


svm <- train(diagnosis ~ ., 
             data = train, 
             method = "svmRadial",
             metric="ROC",
             preProcess = c("center","scale"),
             trControl=fitControl,
             tuneGrid = grid
             #tuneLength=10
             
)


svm 
#The final values used for the model:n.trees = 250, interaction.depth = 2, shrinkage = 0.1 and n.minobsinnode = 10

plot(svm)

#resultado de los K-Folds
svm$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(svm$resample$ROC)

pred_svm <- predict(svm, test)
confusionMatrix(pred_svm, test$diagnosis, positive = "M")

#Accuracy : 0.9883 Sensitivity : 0.9855  Specificity : 0.9902 Kappa : 0.9757

confMSVMRad <- confusionMatrix(pred_svm, test$diagnosis, positive = "M")
Accuracy <- round(confMSVMRad$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='SVM-Radial') 
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

grid <-  expand.grid(C = c(0.05,0.1,0.15,0.2,0.5,0.75,1.0,1.2))



svm_pca <- train(diagnosis ~ ., 
                data = train, 
                method = "svmLinear",
                metric="ROC",
                preProcess = c("center","scale","pca"),
                tuneGrid=grid,
                trControl=fitControl
)



svm_pca
plot(svm_pca)

#resultado de los K-Folds
svm_pca$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(svm_pca$resample$ROC)

pred_svm_pca <- predict(svm_pca, test)
confusionMatrix(pred_svm_pca, test$diagnosis, positive = "M")

#Accuracy : 0.9708   Sensitivity : 0.9565  Specificity : 0.9804  Kappa : 0.9391

confMSVM2 <- confusionMatrix(pred_svm_pca, test$diagnosis, positive = "M")
Accuracy <- round(confMSVM2$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='SVM-PCA') 
res <- rbind(res,new)


###################################################--LDA-####################################################


#Tuning parameters
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
)

grid <-  expand.grid(C = c(0.05,0.1,0.2,0.6,1.0,1.1,1.2),
                     
                     sigma= c(0,0.01, 0.02, 0.025, 0.04, 0.05, 0.07, 0.09, 0.1, 0.25, 0.5,0.9)
)

svm_lda <- train(diagnosis ~ ., 
                data = train_lda, 
                method = "svmRadial",
                metric="ROC",
                preProcess = c("center","scale"),
                #tuneLength = 10,
                tuneGrid=grid,
                trControl=fitControl
)



svm_lda 
plot(svm_lda)

#resultado de los K-Folds
svm_lda$resample

#Compruebo la dispersión de los valores del ROC para la mejor K durante la validación (K-folds)
hist(svm_lda$resample$ROC)

pred_svm_lda <- predict(svm_lda, test_lda)
confusionMatrix(pred_svm_lda, test_lda$diagnosis, positive = "M")

#Accuracy : 0.9883    Sensitivity : 0.9710   Specificity : 1.0000   Kappa : 0.9756

confMSVM3 <- confusionMatrix(pred_svm_lda, test_lda$diagnosis, positive = "M")
Accuracy <- round(confMSVM3$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='SVM-LDA') 
res <- rbind(res,new)

