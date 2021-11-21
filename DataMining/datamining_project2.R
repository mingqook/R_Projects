library("glmnet")
library("MASS")
library("leaps")
library("AUC")
library("caret")
library("plyr")
library("e1071")
library("ROCR")
library("randomForest")
library("class")

feto_data <- read.table("feto_data", header = T)

feto_data$tumor_stage.diagnoses <- revalue(feto_data$tumor_stage.diagnoses, replace = c("stage i" = "a" ,"stage ii" = "b", "stage iii" = "c", "stage iiia" = "d", "stage iiib" = "e", "stage iiic" = "f", "stage iv" = "g", "stage iva" = "h", "stage ivb" = "i"))

feto_data$stage <- rep(1, nrow(feto_data))
for(i in 1:nrow(feto_data)) {
  if (feto_data[i,]$tumor_stage.diagnoses == "a"|feto_data[i,]$tumor_stage.diagnoses == "b"){
    feto_data[i,]$stage <- "a"
  }
  else {
    feto_data[i,]$stage <- "d"
  }
}
feto_data$stage <- factor(feto_data$stage)
feto_data <- feto_data[-34]

pre <- preProcess(feto_data, method = c("center", "scale"))
data_Transformed <- predict(pre, feto_data)
set.seed(1)
trainIndex <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
data_train <- data_Transformed[trainIndex,]
data_test <- data_Transformed[-trainIndex,]



# feto_data_1 <- dummyVars(stage~., data = data_Transformed)
# feto_data_2 <- predict(feto_data_1, newdata = data_Transformed) ## one hot encoding
# feto_data_2 <- cbind(feto_data_2, feto_data$stage)
# colnames(feto_data_2)[ncol(feto_data_2)] <- "stage"
# set.seed(1)
# trainIndex1 <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
# data_train1 <- feto_data_2[trainIndex1,]
# data_test1 <- feto_data_2[-trainIndex1,]

# kn <- knn(train = data_train1, cl = data_train1[,ncol(data_train1)], test = data_test1) 
# t1 <- confusionMatrix(factor(kn), data = factor(data_test1[,ncol(data_test1)]))
# t1$overall[1]

# floor(sqrt(dim(data_train)[1]))
# 
# KNN_model <- knn(data_train, data_test, data_train$stage, k = 18)

# NB_model <- naiveBayes(data_train[,15:16], data_train$stage)
# NB_predict <- predict(NB_model, data_test)
# x <- confusionMatrix(NB_predict, data = data_test$stage)
# x$overall[1]

RF_model <- randomForest(stage ~ ., data = data_train)
RF_predict <- predict(RF_model, data_test)
# confusionMatrix(RF_predict, data = data_test$stage)


data_train1 <- cbind(data_train[,order(-importance(RF_model))],data_train$stage)
colnames(data_train1)[34] <- "stage"
# NB_model1 <- naiveBayes(data_train1[,15:16], data_train1$stage)
# NB_predict1 <- predict(NB_model1, data_test)
# t <- confusionMatrix(NB_predict1, data = data_test$stage)
# t$overall[1] ##accuracy 추출





################# naive bayes

##### 5-fold cv로 변수개수 정하기
h <- list()
for(n in 1 : 5) {
  c <- cut(seq(1:nrow(data_train1)), breaks = 5, labels = F)
  data_train1_train <- data_train1[c != n,]
  data_train1_vali <- data_train1[c == n,]
  m <- list() ## 변수명
  j <- c() ## max accuracy 저장
  k <- 2
  repeat {
    j[1] <- 0
    if (k == 2) {
      q <- c() ## max accuracy를 구하기 위해 모든 accuracy 저장
      w <- c() ## accuracy의 index저장
      for(i in 1 : ncol(combn(ncol(data_train1_train)-1,2))){
        x <- combn(ncol(data_train1_train)-1,2)
        x1 <- x[,i]
        NB <- naiveBayes(data_train1_train[,x1], data_train1_train$stage)
        NB_p <- predict(NB, data_train1_vali)
        y <- confusionMatrix(NB_p, data = data_train1_vali$stage)
        q[i] <- y$overall[1]
        w <- which(q == max(q))
      }
      j[k] <- max(q)
      x <- combn(ncol(data_train1_train)-1,2)
      x2 <- x[,w[1]]
      m[[k-1]] <- colnames(data_train1_train[x2])
      data_train2 <- data_train1_train[,-x2]
      if(j[k] < j[k-1]) break
      k = k + 1
    }
    else {
      q <- c()
      w <- c()
      for(i in 1 : ncol(combn(ncol(data_train2)-1,1))){
        x <- combn(ncol(data_train2)-1,1)
        x1 <- x[,i]
        x3 <- unlist(m)
        x1 <- c(colnames(data_train2)[x1],x3)
        NB <- naiveBayes(data_train1_train[,x1], data_train1_train$stage)
        NB_p <- predict(NB, data_train1_vali)
        y <- confusionMatrix(NB_p, data = data_train1_vali$stage)
        q[i] <- y$overall[1]
        w <- which(q == max(q))
      }
      j[k] <- max(q)
      x <- combn(ncol(data_train2)-1,1)
      x2 <- x[,w[1]]
      m[[k-1]] <- colnames(data_train2[x2])
      data_train2 <- data_train2[,-x2]
      if(j[k] < j[k-1]) break
      k = k + 1
    }
  }
  h[[n]] <- j
}

h ## 변수 6개 썼을 때 최대라고 생각할 수 있다.


#### 5-fold로 정한 변수개수를 통해서 적합

q <- c()
m <- list() ## 변수명
j <- c() ## max accuracy 저장
w <- c()
k <- 2
repeat {
  j[1] <- 0
  if (k == 2) {
    q <- c() ## max accuracy를 구하기 위해 모든 accuracy 저장
    w <- c() ## accuracy의 index저장
    for(i in 1 : ncol(combn(ncol(data_train1)-1,2))){
      x <- combn(ncol(data_train1)-1,2)
      x1 <- x[,i]
      NB <- naiveBayes(data_train1[,x1], data_train1$stage)
      NB_p <- predict(NB, data_test)
      y <- confusionMatrix(NB_p, data = data_test$stage)
      q[i] <- y$overall[1]
      w <- which(q == max(q))
    }
    j[k] <- max(q)
    x <- combn(ncol(data_train1)-1,2)
    x2 <- x[,w[1]]
    m[[k-1]] <- colnames(data_train1[x2])
    data_train2 <- data_train1[,-x2]
    if(j[k] < j[k-1]) break
    k = k + 1
  }
  else {
    q <- c()
    w <- c()
    for(i in 1 : ncol(combn(ncol(data_train2)-1,1))){
      x <- combn(ncol(data_train2)-1,1)
      x1 <- x[,i]
      x3 <- unlist(m)
      x1 <- c(colnames(data_train2)[x1],x3)
      NB <- naiveBayes(data_train1[,x1], data_train1$stage)
      NB_p <- predict(NB, data_test)
      y <- confusionMatrix(NB_p, data = data_test$stage)
      q[i] <- y$overall[1]
      w <- which(q == max(q))
    }
    j[k] <- max(q)
    x <- combn(ncol(data_train2)-1,1)
    x2 <- x[,w[1]]
    m[[k-1]] <- colnames(data_train2[x2])
    data_train2 <- data_train2[,-x2]
    if(k > 7) break
    k = k + 1
  }
}
j
m1 <- unlist(m)
m1[1:7]


NB1 <- naiveBayes(data_train1[,m1], data_train1$stage)
NB_p1 <- predict(NB1, data_test)
y <- confusionMatrix(NB_p1, data = data_test$stage)
y

################### knn

feto_data <- read.table("feto_data", header = T)

feto_data$tumor_stage.diagnoses <- revalue(feto_data$tumor_stage.diagnoses, replace = c("stage i" = "a" ,"stage ii" = "b", "stage iii" = "c", "stage iiia" = "d", "stage iiib" = "e", "stage iiic" = "f", "stage iv" = "g", "stage iva" = "h", "stage ivb" = "i"))

feto_data$stage <- rep(1, nrow(feto_data))
for(i in 1:nrow(feto_data)) {
  if (feto_data[i,]$tumor_stage.diagnoses == "a"|feto_data[i,]$tumor_stage.diagnoses == "b"){
    feto_data[i,]$stage <- "a"
  }
  else {
    feto_data[i,]$stage <- "d"
  }
}
feto_data$stage <- factor(feto_data$stage)
feto_data <- feto_data[-34]

pre <- preProcess(feto_data, method = c("center", "scale"))
data_Transformed <- predict(pre, feto_data)
# set.seed(1)
# trainIndex <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
# data_train <- data_Transformed[trainIndex,]
# data_test <- data_Transformed[-trainIndex,]
# 
# 
# class(data_Transformed$stage)

feto_data_1 <- dummyVars(~., data = data_Transformed)
feto_data_2 <- predict(feto_data_1, newdata = data_Transformed) ## one hot encoding
feto_data_2 <- feto_data_2[,-55:-58]
feto_data_2 <- cbind(feto_data_2, feto_data$stage)
colnames(feto_data_2)[ncol(feto_data_2)] <- "stage"
set.seed(1)
trainIndex1 <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
data_train1 <- feto_data_2[trainIndex1,]
data_test1 <- feto_data_2[-trainIndex1,]




kn <- knn(train = data_train1, cl = data_train1[,ncol(data_train1)], test = data_test1) 
t1 <- confusionMatrix(factor(kn), data = factor(data_test1[,ncol(data_test1)]))
t1$overall[1]



##### 5-fold cv로 변수개수 정하기
h <- list()
for(n in 1 : 5) {
  set.seed(1)
  trainIndex1 <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
  k_train <- data_Transformed[trainIndex1,]
  c <- cut(seq(1:nrow(k_train)), breaks = 5, labels = F)
  k_train_train <- data_train1[c != n,]
  k_train_vali <- data_train1[c == n,]
  m <- list() ## 변수명
  j <- c() ## max accuracy 저장
  k <- 2
  repeat {
    j[1] <- 0
    if (k == 2) {
      q <- c() ## max accuracy를 구하기 위해 모든 accuracy 저장
      w <- c() ## accuracy의 index저장
      for(i in 1 : ncol(combn(ncol(data_Transformed)-1,2))){
        set.seed(1)
        trainIndex1 <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
        k_train <- data_Transformed[trainIndex1,]
        k_test <- data_Transformed[-trainIndex1,]
        x <- combn(ncol(data_Transformed)-1,2)
        x1 <- x[,i]
        k_data_train_train <- k_train_train[,x1]
        k_data_train_train <- cbind(k_data_train_train, k_train_train$stage)
        colnames(k_data_train_train)[ncol(k_data_train_train)] <- "stage"
        data_train_k_train <- dummyVars(~., data = k_data_train_train)
        data_train_k_train <- predict(data_train_k_train, newdata = k_data_train_train)
        data_train_k_train <- data_train_k_train[,-(ncol(data_train_k_train)-3):-ncol(data_train_k_train)]
        data_train_k_train <- cbind(data_train_k_train, k_train_train$stage)
        colnames(data_train_k_train)[ncol(data_train_k_train)] <- "stage"
        k_data_test_vali <- k_train_vali[,x1]
        k_data_test_vali <- cbind(k_data_test_vali, k_train_vali$stage)
        colnames(k_data_test_vali)[ncol(k_data_test_vali)] <- "stage"
        data_test_k_vali <- dummyVars(~., data = k_data_test_vali)
        data_test_k_vali <- predict(data_test_k_vali, newdata = k_data_test_vali)
        data_test_k_vali <- data_test_k_vali[,-(ncol(data_test_k_vali)-3):-ncol(data_test_k_vali)]
        data_test_k_vali <- cbind(data_test_k_vali, k_train_vali$stage)
        colnames(data_test_k_vali)[ncol(data_test_k_vali)] <- "stage"
        kn <- knn(train = data_train_k_train, cl = data_train_k_train[,ncol(data_train_k_train)], test = data_test_k_vali) 
        t1 <- confusionMatrix(factor(kn), data = factor(data_test_k_vali[,ncol(data_test_k_vali)]))
        q[i] <- t1$overall[1]
        w <- which(q == max(q))
      }
      j[k] <- max(q)
      x <- combn(ncol(data_Transformed)-1,2)
      x2 <- x[,w[1]]
      m[[k-1]] <- colnames(data_Transformed[x2])
      data_Transformed2 <- data_Transformed[,-x2]
      if(j[k] < j[k-1]) break
      k = k + 1
    }
    else {
      q <- c()
      w <- c()
      for(i in 1 : ncol(combn(ncol(data_Transformed2)-1,1))){
        set.seed(1)
        trainIndex1 <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
        k_train <- data_Transformed[trainIndex1,]
        k_test <- data_Transformed[-trainIndex1,]
        x <- combn(ncol(data_Transformed2)-1,1)
        x1 <- x[,i]
        x3 <- unlist(m)
        x1 <- c(colnames(data_Transformed2)[x1],x3)
        k_data_train_train <- k_train_train[,x1]
        k_data_train_train <- cbind(k_data_train_train, k_train_train$stage)
        colnames(k_data_train_train)[ncol(k_data_train_train)] <- "stage"
        data_train_k_train <- dummyVars(~., data = k_data_train_train)
        data_train_k_train <- predict(data_train_k_train, newdata = k_data_train_train)
        data_train_k_train <- data_train_k_train[,-(ncol(data_train_k_train)-3):-ncol(data_train_k_train)]
        data_train_k_train <- cbind(data_train_k_train, k_train_train$stage)
        colnames(data_train_k_train)[ncol(data_train_k_train)] <- "stage"
        k_data_test_vali <- k_train_vali[,x1]
        k_data_test_vali <- cbind(k_data_test_vali, k_train_vali$stage)
        colnames(k_data_test_vali)[ncol(k_data_test_vali)] <- "stage"
        data_test_k_vali <- dummyVars(~., data = k_data_test_vali)
        data_test_k_vali <- predict(data_test_k_vali, newdata = k_data_test_vali)
        data_test_k_vali <- data_test_k_vali[,-(ncol(data_test_k_vali)-3):-ncol(data_test_k_vali)]
        data_test_k_vali <- cbind(data_test_k_vali, k_train_vali$stage)
        colnames(data_test_k_vali)[ncol(data_test_k_vali)] <- "stage"
        kn <- knn(train = data_train_k_train, cl = data_train_k_train[,ncol(data_train_k_vali)], test = data_test_k_vali) 
        t1 <- confusionMatrix(factor(kn), data = factor(data_test_k_vali[,ncol(data_test_k_vali)]))
        q[i] <- t1$overall[1]
        w <- which(q == max(q))
      }
      j[k] <- max(q)
      x <- combn(ncol(data_Transformed2)-1,1)
      x2 <- x[,w[1]]
      m[[k-1]] <- colnames(data_Transformed2[x2])
      data_Transformed2 <- data_Transformed2[,-x2]
      if(k > 8) break
      k = k + 1
    }
  }
  h[[n]] <- j
}

h ## 변수 7개 썼을 때 최대라고 생각할 수 있다.


#### 5-fold로 정한 변수개수를 통해서 적합
q <- c()
m <- list() ## 변수명
j <- c() ## max accuracy 저장
w <- c()
k <- 2
repeat {
  j[1] <- 0
  if (k == 2) {
    q <- c() ## max accuracy를 구하기 위해 모든 accuracy 저장
    w <- c() ## accuracy의 index저장
    for(i in 1 : ncol(combn(ncol(data_Transformed)-1,2))){
      set.seed(1)
      trainIndex1 <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
      k_train <- data_Transformed[trainIndex1,]
      k_test <- data_Transformed[-trainIndex1,]
      x <- combn(ncol(data_Transformed)-1,2)
      x1 <- x[,i]
      k_data_train <- k_train[,x1]
      k_data_train <- cbind(k_data_train, k_train$stage)
      colnames(k_data_train)[ncol(k_data_train)] <- "stage"
      data_train_k <- dummyVars(~., data = k_data_train)
      data_train_k <- predict(data_train_k, newdata = k_data_train)
      data_train_k <- data_train_k[,-(ncol(data_train_k)-3):-ncol(data_train_k)]
      data_train_k <- cbind(data_train_k, k_train$stage)
      colnames(data_train_k)[ncol(data_train_k)] <- "stage"
      k_data_test <- k_test[,x1]
      k_data_test <- cbind(k_data_test, k_test$stage)
      colnames(k_data_test)[ncol(k_data_test)] <- "stage"
      data_test_k <- dummyVars(~., data = k_data_test)
      data_test_k <- predict(data_test_k, newdata = k_data_test)
      data_test_k <- data_test_k[,-(ncol(data_test_k)-3):-ncol(data_test_k)]
      data_test_k <- cbind(data_test_k, k_test$stage)
      colnames(data_test_k)[ncol(data_test_k)] <- "stage"
      kn <- knn(train = data_train_k, cl = data_train_k[,ncol(data_train_k)], test = data_test_k) 
      t1 <- confusionMatrix(factor(kn), data = factor(data_test_k[,ncol(data_test_k)]))
      q[i] <- t1$overall[1]
      w <- which(q == max(q))
    }
    j[k] <- max(q)
    x <- combn(ncol(data_Transformed)-1,2)
    x2 <- x[,w[1]]
    m[[k-1]] <- colnames(data_Transformed[x2])
    data_Transformed2 <- data_Transformed[,-x2]
    if(j[k] < j[k-1]) break
    k = k + 1
  }
  else {
    q <- c()
    w <- c()
    for(i in 1 : ncol(combn(ncol(data_Transformed2)-1,1))){
      set.seed(1)
      trainIndex1 <- createDataPartition(data_Transformed$stage, p = .7, list = FALSE, times = 1)
      k_train <- data_Transformed[trainIndex1,]
      k_test <- data_Transformed[-trainIndex1,]
      x <- combn(ncol(data_Transformed2)-1,1)
      x1 <- x[,i]
      x3 <- unlist(m)
      x1 <- c(colnames(data_Transformed2)[x1],x3)
      k_data_train <- k_train[,x1]
      k_data_train <- cbind(k_data_train, k_train$stage)
      colnames(k_data_train)[ncol(k_data_train)] <- "stage"
      data_train_k <- dummyVars(~., data = k_data_train)
      data_train_k <- predict(data_train_k, newdata = k_data_train)
      data_train_k <- data_train_k[,-(ncol(data_train_k)-3):-ncol(data_train_k)]
      data_train_k <- cbind(data_train_k, k_train$stage)
      colnames(data_train_k)[ncol(data_train_k)] <- "stage"
      k_data_test <- k_test[,x1]
      k_data_test <- cbind(k_data_test, k_test$stage)
      colnames(k_data_test)[ncol(k_data_test)] <- "stage"
      data_test_k <- dummyVars(~., data = k_data_test)
      data_test_k <- predict(data_test_k, newdata = k_data_test)
      data_test_k <- data_test_k[,-(ncol(data_test_k)-3):-ncol(data_test_k)]
      data_test_k <- cbind(data_test_k, k_test$stage)
      colnames(data_test_k)[ncol(data_test_k)] <- "stage"
      kn <- knn(train = data_train_k, cl = data_train_k[,ncol(data_train_k)], test = data_test_k) 
      t1 <- confusionMatrix(factor(kn), data = factor(data_test_k[,ncol(data_test_k)]))
      q[i] <- t1$overall[1]
      w <- which(q == max(q))
    }
    j[k] <- max(q)
    x <- combn(ncol(data_Transformed2)-1,1)
    x2 <- x[,w[1]]
    m[[k-1]] <- colnames(data_Transformed2[x2])
    data_Transformed2 <- data_Transformed2[,-x2]
    if(k > 8) break
    k = k + 1
  }
}
m1 <- unlist(m)
m1






























# q <- c()
# m <- list() ## 변수명
# j <- c() ## max accuracy 저장
# w <- c()
# k <- 2
# repeat {
#   j[1] <- 0
#   if (k == 2) {
#     q <- c() ## max accuracy를 구하기 위해 모든 accuracy 저장
#     w <- c() ## accuracy의 index저장
#     for(i in 1 : ncol(combn(ncol(data_train1)-1,2))){
#       x <- combn(ncol(data_train1)-1,2)
#       x1 <- x[,i]
#       NB <- naiveBayes(data_train1[,x1], data_train1$stage)
#       NB_p <- predict(NB, data_test)
#       y <- confusionMatrix(NB_p, data = data_test$stage)
#       q[i] <- y$overall[1]
#       w <- which(q == max(q))
#     }
#     j[k] <- max(q)
#     x <- combn(ncol(data_train1)-1,2)
#     x2 <- x[,w[1]]
#     m[[k-1]] <- colnames(data_train1[x2])
#     data_train2 <- data_train1[,-x2]
#     if(j[k] < j[k-1]) break
#     k = k + 1
#   }
#   else {
#     q <- c()
#     w <- c()
#     for(i in 1 : ncol(combn(ncol(data_train2)-1,1))){
#       x <- combn(ncol(data_train2)-1,1)
#       x1 <- x[,i]
#       x3 <- unlist(m)
#       x1 <- c(colnames(data_train2)[x1],x3)
#       NB <- naiveBayes(data_train1[,x1], data_train1$stage)
#       NB_p <- predict(NB, data_test)
#       y <- confusionMatrix(NB_p, data = data_test$stage)
#       q[i] <- y$overall[1]
#       w <- which(q == max(q))
#     }
#     j[k] <- max(q)
#     x <- combn(ncol(data_train1)-1,1)
#     x2 <- x[,w[1]]
#     m[[k-1]] <- colnames(data_train2[x2])
#     data_train2 <- data_train2[,-x2]
#     if(j[k] < j[k-1]) break
#     k = k + 1
#   }
# }




colnames(data_train1)

dim(data_train2)

data_train1[,c(2,25)]

data_train1[,colnames(data_train)[c(54,55)]]

combn(ncol(data_test)-1,2)[,55]
dim(combn(ncol(data_test),2))

NB <- naiveBayes(data_train[,combn(33,2)[,33]], data_train$stage)
NB_p <- predict(NB, data_test)
y <- confusionMatrix(NB_p, data = data_test$stage)
y$overall[1]

re <- naiveBayes(data_train[,as.character(k$Attribute[i])],data_train$stage)
re1 <- predict(re, data_test)
a <- prediction(as.numeric(re1), as.numeric(data_test$stage))
a <- performance(a,"auc")
k[i,"AUC"] <- a@y.values[[1]]

k <- data.frame(Attribute = c(colnames(data_train[1:33])), a = NA)
for (i in 1:nrow(k)){
  NB_model <- naiveBayes(data_train[,as.character(k$Attribute[i])], data_train$stage)
  NB_predict <- predict(NB_model, data_test)
  t <- confusionMatrix(NB_predict, data = data_test$stage)
  k[i,"a"] <- t$overall[1]
}
k



b <- prediction(as.numeric(NB_predict1), as.numeric(data_test$stage))
b1 <- performance(b,"auc")
b1 <- b1@y.values[[1]]
b1 ## auc는 2진분류에서만

a <- prediction(as.numeric(NB_predict), as.numeric(data_test$stage))
a1 <- performance(a,"auc")
a1 <- a1@y.values[[1]]
a1 ## auc는 2진분류에서만
# a2 <- performance(a, measure = "tpr", x.measure = "fpr")
# plot(a2)
# str(a2)


# install.packages("multiROC")
# library("multiROC")
# cal_confus(as.factor(data_test$stage), as.factor(NB_model))
# ?cla_confus

install.packages("pROC")
library("pROC")
multiclass.roc(as.numeric(data_test$stage), as.numeric(NB_predict))


# k <- data.frame(Attribute = c(colnames(data_train[1:33])),AUC = NA)
# for (i in 1:nrow(k)){
#   re <- naiveBayes(data_train[,as.character(k$Attribute[i])],data_train$stage)
#   re1 <- predict(re, data_test)
#   a <- prediction(as.numeric(re1), as.numeric(data_test$stage))
#   a <- performance(a,"auc")
#   k[i,"AUC"] <- a@y.values[[1]]
# }

install.packages("AUCRF")
library("AUCRF")
fit <- AUCRF(stage ~., data = data_train)
