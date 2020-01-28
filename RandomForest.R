
###################################################--Random Forest--####################################################

#Primero voy a realizar la predicción con la totalidad de mis datos.


#Fijamos la semilla para que los resultados sean repetibles
set.seed(543)


#Mezclo y divido el dataset
index <- sample(1:NROW(data), 0.7 * NROW(data))   
train <- data[index,]                   ## 398 test data (70%)
test <- data[-index,]                   ## 171 test data (30%)
prop.table(table(test$diagnosis))
prop.table(table(train$diagnosis))


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

#Obtenemos una precisión de 97,08%, lo cual no esta nada mal. Cab mencionar que se ha obtenido un 99% de Specificity 
#y un 93.65% de Sensitivity. Posiblemente se querria al revés, aumentar la Sensitivity; ya que preferimos tener un
#falso positivo que un falso negativo. El valor de Kappa es de 93%, lo cual es muy positivo.





#Ahora voy a utilizar para realizar mi modelo los datos transformados por PCa en vez de utilizar la totalidad de ellos.

fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary
                           #search='grid'
                           )

Grid <- expand.grid(mtry = (2:17))

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










train_lda <- lda(diagnosis ~., data = train, center = TRUE, scale = TRUE)
train_lda <- predict(train_lda, data)$x %>% as.data.frame() %>% cbind(diagnosis=train$diagnosis)


test_lda <- lda_df[-data_index,]



fitControl <- trainControl(method = "repeatedcv",
                           number = 10,
                           repeats = 10,
                           #preProcOptions = list(thresh = 0.99), # threshold for pca preprocess
                           classProbs = TRUE,
                           summaryFunction = twoClassSummary)


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
pred_rf <- predict(model_rf, test)
confusionMatrix(pred_rf, test$diagnosis, positive = "M")