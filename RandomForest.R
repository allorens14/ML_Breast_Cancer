
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
                  tuneLength=15,
                  #tuneGrid = Grid,
                  preProcess = c('center', 'scale','pca'),
                  trControl=fitControl)
model_rf
plot(model_rf)
pred_rf <- predict(model_rf, test)
confusionMatrix(pred_rf, test$diagnosis, positive = "M")

#Con un threshold de 0.99 o 0.95 el algoritmo no mejora en cuanto a precisión a utilizar PCA, mas concretamente,
#al utilizarlo con ranger (rf da peores resultados) alcanza alrededor de 95. Es un 2% menos preciso


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

#La precision de este modelo es de 98.25% superior a las dos opciones vistas anteriormente (97% y 95% respectivamente)
#El valor de Kappa es de 0.9635, siendo tambien el mayor de entre el resto de modelos.
#Los valores de Sensitivity y Specificity son 0.9710 y 0.9902 igualando en Specificity al mejor y superando en
#Sensitivity al mejor en un ~3%.

#Este último modelo es sin duda el mejor de los 3: RandomForest con LDA. Enhorabuena Alejandro! ;)


