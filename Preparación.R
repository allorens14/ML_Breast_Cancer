#Fijamos la semilla para que los resultados sean repetibles
set.seed(543)


#Mezclo y divido el dataset
index <- sample(1:NROW(data), 0.7 * NROW(data))   
train <- data[index,]                   ## 398 test data (70%)
test <- data[-index,]                   ## 171 test data (30%)
prop.table(table(test$diagnosis))
prop.table(table(train$diagnosis))



#Creo dos nuevos dataset de train y test para poder utilizar los algoritmos con LDA
library(MASS)
train_lda <- lda(diagnosis ~., data = train, center = TRUE, scale = TRUE)
train_lda <- predict(train_lda, train)$x %>% as.data.frame() %>% cbind(diagnosis=train$diagnosis)

test_lda <- lda(diagnosis ~., data = test, center = TRUE, scale = TRUE)
test_lda <- predict(test_lda, test)$x %>% as.data.frame() %>% cbind(diagnosis=test$diagnosis)

