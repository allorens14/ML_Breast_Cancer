
###################################################--Random Forest--####################################################

#Primero voy a realizar la predicción con la totalidad de mis datos.


#Creamos los parametros de Control del algoritmo. Se ha elegido un K-fold como método de comprobación, con 10 folds y 
#10 repeticiones
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

#Creamos el modelo, entrenandolo sobre el dataset de train y asignando como trControl nuestros parametros. 
#La métrica que vamos a intentar maximizar es el 'ROC', por lo que lo indicamos en el parametro 'metric'.
#Como preproceso, se va a realizar una estandarización de los datos. Ya que como hemos visto en el analisis,
#las variables tienen rangos muy dispares. Utilizamos el método ranger para probar con los tipos de division posibles.
model_rf <- train(diagnosis~.,
                  train,
                  method="ranger",
                  metric="ROC",
                  #tuneLength=15,
                  #tuneGrid = expand.grid(mtry = c(2, 3, 6)),
                  preProcess = c('center', 'scale'),
                  trControl=fitControl)
model_rf
plot(model_rf)

#Importancia de la variables
plot(varImp(model_rf))

#resultado de los K-Folds
model_rf$resample

#Compruebo la dispersión de los valores del ROC para mtry = 2, splitrule = extratrees and min.node.size = 1.
#durante la validación (K-folds)
hist(model_rf$resample$ROC)

#Vemos que el máximo valor de ROC (99,2%) lo obtiene con los siguientes parametros:
#mtry = 2 (numero de variables a elegir en cada division), 
#splitrule = extratrees (metodo de division)
#and min.node.size = 1 (minimo tamaño de nodo)

#Ahora predecimos el resultado del target con los datos de test
pred_rf <- predict(model_rf, test)


#Creamos una ConfusionMatrix para ver el rendimiento de mi modelo. Compruebo los aciertos de la prediccdión con los 
#valores reales de la variable taget de mi dataset de test. Consideramos como valor positivo el encontrar un tumor.
confusionMatrix(pred_rf, test$diagnosis, positive = "M")

#Obtenemos una precisión de 97,08%, lo cual no esta nada mal. Cabe mencionar que se ha obtenido un 99% de Specificity 
#y un 93.65% de Sensitivity. Posiblemente se querria al revés, aumentar la Sensitivity; ya que preferimos tener un
#falso positivo que un falso negativo. El valor de Kappa es de 93%, lo cual es muy positivo.

#Guardo el valor de la Accuracy de mi modelo en un data frame de resultados para luego poder comparar modelos
confMRF <- confusionMatrix(pred_rf, test$diagnosis, positive = "M")
Accuracy <- round(confMRF$overall[1]*100,3)
res <- as.data.frame(Accuracy) %>% mutate(algoritmo='RandomForest') 


###################################################--PCA--####################################################


#Ahora voy a utilizar para realizar mi modelo los datos transformados por PCa en vez de utilizar la totalidad 
#de ellos. Para ello utilizare caret, ya que a traves del parametro 'preProcess' nos permite realizar cambios sobre
#el dataset antes de entrenar el modelo.

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           #search='grid'
                           )

#Grid <- expand.grid(mtry = (2:17))

model_rf <- train(diagnosis~.,
                  train,
                  method="ranger",
                  metric="ROC",
                  tuneLength=10,
                  #tuneGrid = Grid,
                  preProcess = c('center', 'scale','pca'),
                  trControl=fitControl)
model_rf
plot(model_rf)
pred_rf <- predict(model_rf, test)
confusionMatrix(pred_rf, test$diagnosis, positive = "M")

#Con un threshold de 0.99 o 0.95 el algoritmo no mejora en cuanto a precisión al utilizar PCA, mas concretamente,
#al utilizarlo con ranger (rf da peores resultados) alcanza alrededor de 95. Es un 2% menos preciso

confMRF2 <- confusionMatrix(pred_rf, test$diagnosis, positive = "M")
Accuracy <- round(confMRF2$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='RandomForest-PCA') 
res <- rbind(res,new)


 ###################################################--LDA-####################################################

#Voy a crear ahora el modelo sobre los datasets creados con lda aplicado.

#Misma función de control(metodo de validacion K-folds, 10 folds, 10 repeticiones)
fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           #preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)

#creamos el modelo. Puesto que solo tenemos una variable no tiene sentido probarlo con mtry>1 por lo que fijamos
#el parametro tuneLenght a 1.
model_rf_lda <- train(diagnosis~.,
                  train_lda,
                  method="rf",
                  metric="ROC",
                  tuneLength=1,
                  #tuneGrid = expand.grid(mtry = c(2, 3, 6)),
                  #preProcess = c('center', 'scale'),
                  trControl=fitControl)
model_rf_lda
pred_rf <- predict(model_rf_lda, test_lda)
confusionMatrix(pred_rf, test_lda$diagnosis, positive = "M")

#La precision de este modelo es de 95,91% similar a la opcion anterior
#El valor de Kappa es de 0.9148, siendo tambien bueno.
#Los valores de Sensitivity y Specificity son 0.9420 y 0.9706 superando en Sensitivity al primero pero peor en
#Sensitivity en un ~2%.

#El mejor de los 3 parece ser el primero: RandomForest con ranger y todos los datos.


confMRF3 <- confusionMatrix(pred_rf, test_lda$diagnosis, positive = "M")
Accuracy <- round(confMRF3$overall[1]*100,3)
new <- data.frame(Accuracy) %>% mutate(algoritmo='RandomForest-LDA') 
res <- rbind(res,new)
