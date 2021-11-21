feto_data <- read.table("feto_data", header = T)
is.na(feto_data$tumor_stage.diagnoses)


install.packages("glmnet")
install.packages("leaps")
install.packages("MASS")
install.packages("AUC")
install.packages("plyr")
library("glmnet")
library("MASS")
library("leaps")
library("AUC")
library("caret")
library("plyr")
library("e1071")

feto_data$tumor_stage.diagnoses <- revalue(feto_data$tumor_stage.diagnoses, replace = c("stage i" = "a" ,"stage ii" = "b", "stage iii" = "c", "stage iiia" = "d", "stage iiib" = "e", "stage iiic" = "f", "stage iv" = "g", "stage iva" = "h", "stage ivb" = "i"))

feto_data$stage <- rep(1, nrow(feto_data))
for(i in 1:nrow(feto_data)) {
  if (feto_data[i,]$tumor_stage.diagnoses == "a"){
    feto_data[i,]$stage <- "a"
  }
  else if (feto_data[i,]$tumor_stage.diagnoses == "b") {
    feto_data[i,]$stage <- "b"
  }
  else if (feto_data[i,]$tumor_stage.diagnoses == "c" |feto_data[i,]$tumor_stage.diagnoses == "d" | 
           feto_data[i,]$tumor_stage.diagnoses == "e" |feto_data[i,]$tumor_stage.diagnoses == "f") {
    feto_data[i,]$stage <- "c"
  }
  else if (feto_data[i,]$tumor_stage.diagnoses == "g" |feto_data[i,]$tumor_stage.diagnoses == "h" |
           feto_data[i,]$tumor_stage.diagnoses == "i") {
    feto_data[i,]$stage <- "d"
  }
}
feto_data$stage <- factor(feto_data$stage)
feto_data <- feto_data[-34]

# png("C:/Users/minguk/Desktop/coding/R/Data_preprocessing/a.png")
# featurePlot(x = feto_data[,1:length(feto_data)-1], y = feto_data$tumor_stage.diagnoses, plot = "pairs")
# dev.off() ## 변수들간의 관계 
# 
# feto_data_1 <- dummyVars(tumor_stage.diagnoses~., data = feto_data)
# head(predict(feto_data_1, newdata = feto_data)) ## one hot encoding
# 
# nzv <- nearZeroVar(feto_data, saveMetrics= TRUE)
# 
# set.seed(1)
# i <- sample(1:nrow(feto_data), 0.7*nrow(feto_data))
# total_train <- feto_data[i,]
# test <- feto_data[-i,]
# ii <- sample(1:nrow(total_train), 0.5*nrow(total_train))
# train <- total_train[ii,]
# validation <- total_train[-ii,]
# dim(total_train)
# dim(test)
# 
# preProcValues <- preProcess(total_train, method = c("center", "scale"))
# pre <- preProcess(feto_data, method = c("center", "scale"))
# 
# 
# total_train_Transformed <- predict(preProcValues, total_train)
# test_Transformed <- predict(preProcValues, test)
# data_Transformed <- predict(pre, feto_data)
# 
# total_train_Transformed_1 <- dummyVars(tumor_stage.diagnoses~. , data = total_train_Transformed)
# test_Transformed_1 <- dummyVars(tumor_stage.diagnoses~. , data = test_Transformed)
# data_Transformed_1 <- dummyVars(tumor_stage.diagnoses~. , data = data_Transformed)
# 
# set.seed(1)
# trainIndex <- createDataPartition(data_Transformed$tumor_stage.diagnoses, p = .7, list = FALSE, times = 1)
# data_train <- data_Transformed[trainIndex,]
# data_test <- data_Transformed[-trainIndex,]
# 
# fitControl <- trainControl(method = "repeatedcv", number = 4, classProbs = T)
# NB_model <- train(tumor_stage.diagnoses ~., data = data_train, method = 'naive_bayes', trControl = fitControl, metric = 'Accuracy')
# data_train$tumor_stage.diagnoses





feto_data_1 <- dummyVars(stage~., data = feto_data)
head(predict(feto_data_1, newdata = feto_data)) ## one hot encoding

nzv <- nearZeroVar(feto_data, saveMetrics= TRUE)

# set.seed(1)
# i <- sample(1:nrow(feto_data), 0.7*nrow(feto_data))
# total_train <- feto_data[i,]
# test <- feto_data[-i,]
# ii <- sample(1:nrow(total_train), 0.5*nrow(total_train))
# train <- total_train[ii,]
# validation <- total_train[-ii,]
# dim(total_train)
# dim(test)

# preProcValues <- preProcess(total_train, method = c("center", "scale"))
pre <- preProcess(feto_data, method = c("center", "scale"))


# total_train_Transformed <- predict(preProcValues, total_train)
# test_Transformed <- predict(preProcValues, test)
data_Transformed <- predict(pre, feto_data)

# total_train_Transformed_1 <- dummyVars(stage~. , data = total_train_Transformed)
# test_Transformed_1 <- dummyVars(stage~. , data = test_Transformed)
data_Transformed_1 <- dummyVars(stage~. , data = data_Transformed)

set.seed(1)
trainIndex <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
data_train <- data_Transformed[trainIndex,]
data_test <- data_Transformed[-trainIndex,]


fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, classProbs = T)

NB_model <- train(factor(stage) ~ . , data = data_train, method = 'naive_bayes', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(NB_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

KNN_model <- train(factor(stage) ~., data = data_train, method = 'knn', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(KNN_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

NN_model <- train(factor(stage) ~., data = data_train, method = 'nnet', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(NN_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

SVM_model <- train(factor(stage) ~., data = data_train, method = 'svmRadial', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(SVM_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

RF_model <- train(factor(stage) ~., data = data_train, method = 'rf', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(RF_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

importance <- varImp(RF_model, scale=FALSE)
summary(importance)

importance <- varImp(SVM_model, scale=FALSE)
summary(importance)

importance <- varImp(NB_model, scale=FALSE)
summary(importance)
NB_model$dots


##-----------------------------------------------

nofeto_data <- read.table("nofeto_data", header = T)

nofeto_data$tumor_stage.diagnoses <- revalue(nofeto_data$tumor_stage.diagnoses, replace = c("stage i" = "a" ,"stage ii" = "b", "stage iii" = "c", "stage iiia" = "d", "stage iiib" = "e", "stage iiic" = "f", "stage iv" = "g", "stage iva" = "h", "stage ivb" = "i"))

nofeto_data$stage <- rep(1, nrow(nofeto_data))
for(i in 1:nrow(nofeto_data)) {
  if (nofeto_data[i,]$tumor_stage.diagnoses == "a"){
    nofeto_data[i,]$stage <- "a"
  }
  else if (nofeto_data[i,]$tumor_stage.diagnoses == "b") {
    nofeto_data[i,]$stage <- "b"
  }
  else if (nofeto_data[i,]$tumor_stage.diagnoses == "c" |nofeto_data[i,]$tumor_stage.diagnoses == "d" | 
           nofeto_data[i,]$tumor_stage.diagnoses == "e" |nofeto_data[i,]$tumor_stage.diagnoses == "f") {
    nofeto_data[i,]$stage <- "c"
  }
  else if (nofeto_data[i,]$tumor_stage.diagnoses == "g" |nofeto_data[i,]$tumor_stage.diagnoses == "h" |
           nofeto_data[i,]$tumor_stage.diagnoses == "i") {
    nofeto_data[i,]$stage <- "d"
  }
}
nofeto_data$stage <- factor(nofeto_data$stage)
nofeto_data <- nofeto_data[-32]

pre <- preProcess(nofeto_data, method = c("center", "scale"))
data_Transformed <- predict(pre, nofeto_data)
data_Transformed_1 <- dummyVars(stage~. , data = data_Transformed)

set.seed(1)
trainIndex <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
data_train <- data_Transformed[trainIndex,]
data_test <- data_Transformed[-trainIndex,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, classProbs = T)
NB_model <- train(factor(stage) ~., data = data_train, method = 'naive_bayes', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(NB_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

KNN_model <- train(factor(stage) ~., data = data_train, method = 'knn', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(KNN_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

NN_model <- train(factor(stage) ~., data = data_train, method = 'nnet', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(NN_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

SVM_model <- train(factor(stage) ~., data = data_train, method = 'svmRadial', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(SVM_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)




##-----------------------------------------------

feto_data1 <- read.table("feto_data1", header = T)

feto_data1$tumor_stage.diagnoses <- revalue(feto_data1$tumor_stage.diagnoses, replace = c("stage i" = "a" ,"stage ii" = "b", "stage iii" = "c", "stage iiia" = "d", "stage iiib" = "e", "stage iiic" = "f", "stage iv" = "g", "stage iva" = "h", "stage ivb" = "i"))

feto_data1$stage <- rep(1, nrow(feto_data1))
for(i in 1:nrow(feto_data1)) {
  if (feto_data1[i,]$tumor_stage.diagnoses == "a"){
    feto_data1[i,]$stage <- "a"
  }
  else if (feto_data1[i,]$tumor_stage.diagnoses == "b") {
    feto_data1[i,]$stage <- "b"
  }
  else if (feto_data1[i,]$tumor_stage.diagnoses == "c" |feto_data1[i,]$tumor_stage.diagnoses == "d" | 
           feto_data1[i,]$tumor_stage.diagnoses == "e" |feto_data1[i,]$tumor_stage.diagnoses == "f") {
    feto_data1[i,]$stage <- "c"
  }
  else if (feto_data1[i,]$tumor_stage.diagnoses == "g" |feto_data1[i,]$tumor_stage.diagnoses == "h" |
           feto_data1[i,]$tumor_stage.diagnoses == "i") {
    feto_data1[i,]$stage <- "d"
  }
}
feto_data1$stage <- factor(feto_data1$stage)
feto_data1 <- feto_data1[-34]

pre1 <- preProcess(feto_data1, method = c("center", "scale"))
data_Transformed1 <- predict(pre1, feto_data1)
data_Transformed_11 <- dummyVars(stage~. , data = data_Transformed1)

set.seed(1)
trainIndex <- createDataPartition(data_Transformed1$stage, p = .7, list = FALSE, times = 1)
data_train <- data_Transformed1[trainIndex,]
data_test <- data_Transformed1[-trainIndex,]

fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 5, classProbs = T)
NB_model <- train(factor(stage) ~., data = data_train, method = 'naive_bayes', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(NB_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

KNN_model <- train(factor(stage) ~., data = data_train, method = 'knn', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(KNN_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

NN_model <- train(factor(stage) ~., data = data_train, method = 'nnet', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(NN_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)

SVM_model <- train(factor(stage) ~., data = data_train, method = 'svmRadial', trControl = fitControl, metric = 'Accuracy')
Predict_result <- predict(SVM_model, newdata = data_test)
confusionMatrix(data = Predict_result, data_test$stage)
