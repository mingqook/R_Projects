feto_data2 <- read.table("feto_data_2", header = T)

feto_data2$tumor_stage.diagnoses <- revalue(feto_data2$tumor_stage.diagnoses, replace = c("stage i" = "a" ,"stage ii" = "b", "stage iii" = "c", "stage iiia" = "d", "stage iiib" = "e", "stage iiic" = "f", "stage iv" = "g", "stage iva" = "h", "stage ivb" = "i", "not reported" = "no"))

feto_data2$stage <- rep(1, nrow(feto_data2))
for(i in 1:nrow(feto_data2)) {
  if (feto_data2[i,]$tumor_stage.diagnoses == "a"){
    feto_data2[i,]$stage <- "a"
  }
  else if (feto_data2[i,]$tumor_stage.diagnoses == "no") {
    feto_data2[i,]$stage <- "b"
  }
  else{
    feto_data2[i,]$stage <- "c"
  }
}
feto_data2$stage <- factor(feto_data2$stage)
feto_data2 <- feto_data2[-34]

pre <- preProcess(feto_data2, method = c("center", "scale"))
data_Transformed <- predict(pre, feto_data2)

feto_data_1 <- dummyVars(~., data = data_Transformed)
feto_data_2 <- predict(feto_data_1, newdata = data_Transformed) ## one hot encoding
feto_data_2 <- feto_data_2[,-55:-58]
feto_data_2 <- cbind(feto_data_2, feto_data2$stage)
colnames(feto_data_2)[ncol(feto_data_2)] <- "stage"
set.seed(1)
trainIndex1 <- createDataPartition(data_Transformed$stage, p = 2/3, list = FALSE, times = 1)
data_train1 <- feto_data_2[trainIndex1,]
data_test1 <- feto_data_2[-trainIndex1,]

h <- list()
for(n in 1:5){
  set.seed(1)
  c <- cut(seq(1:nrow(data_train1)), breaks = 5, labels = F)
  data_train1_train <- data_train1[c != n,]
  data_train1_vali <- data_train1[c == n,]
  j <- c()
  for(i in 1:20){
    kn <- knn(train = data_train1_train, cl = data_train1_train[,ncol(data_train1_train)], test = data_train1_vali, k = i) 
    t1 <- confusionMatrix(factor(kn), data = factor(data_train1_vali[,ncol(data_train1_vali)]))
    j[i] <- t1$overall[1]
  }
  h[[n]] <- j 
}
h

s <- rep(0,20)
for(j in 1:20){
  for(i in 1:5){
    s[j] <- s[j] + h[[i]][j]
  }
}
s <- s[-1]
which.max(s)

##### 17개 일 때 최고

kn1 <- knn(train = data_train1, cl = data_train1[,ncol(data_train1)], test = data_test1, k = 17)
kn_c <- confusionMatrix(factor(kn1), data = factor(data_test1[,ncol(data_test1)]))
kn_c

kn_R <- multiclass.roc(as.numeric(data_test1[,ncol(data_test1)]), as.numeric(kn1))
auc(kn_R)
kn_R1 <- kn_R[['rocs']]
plot.roc(kn_R1[[1]])
sapply(2:length(kn_R1),function(i) lines.roc(kn_R1[[i]],col=i))
