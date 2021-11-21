data3 <- read.csv('pendigit3.txt', sep = ",", header = F)
View(data3)
a <- data3[1,]
a1 <- unlist(c(a[1],a[3],a[5],a[7],a[9],a[11],a[13],a[15]))
a2 <- unlist(c(a[2],a[4],a[6],a[8],a[10],a[12],a[14],a[16]))
plot(a1,a2)
lines(a1,a2)     



data3_1 <- data3[,1:16]
pri_data3 <- princomp(data3_1)
loading <- pri_data3$loadings
lamda <- (pri_data3$sdev)^2
score <- pri_data3$scores
pairs(score)
plot(lamda, main = "scree", xlab = "component", ylab = "lamda")
lines(lamda)

par(mfrow = c(2,2))
for (i in 1:4){
  plot(score[,i], main = paste("scatter plot",i,"th","score") )
}

par(mfrow = c(2,2))
for (i in 5:8){
  plot(score[,i], main = paste("scatter plot",i,"th","score") )
}

par(mfrow = c(2,2))
for (i in 5:8){
  qqnorm(score[,i],main = paste("QQplot",i,"th","score") )
  qqline(score[,i], distribution = qnorm)
  
}

## plot을 봤을 때 정규분포를 따를 것 같다. qqplot을 그렸을 때 직선관계가 나타나므로 모두 정규성 따름


summary(pri_data3)
plot(cumsum(lamda)*100/sum(lamda),main = "cumscree", xlab = "compoent", ylab = "cumulative percent")
lines(cumsum(lamda)*100/sum(lamda))
abline(h = 90)


data3_2 <- apply(data3_1,2,mean)
par(mfrow = c(1,5))
for(i in 1:5){
  x <- data3_2 + (-2 + (i-1)*1) * sqrt(lamda[1]) * loading[,1]
  a1 <- unlist(c(x[1],x[3],x[5],x[7],x[9],x[11],x[13],x[15]))
  a2 <- unlist(c(x[2],x[4],x[6],x[8],x[10],x[12],x[14],x[16]))
  plot(a1,a2)
  lines(a1,a2)
}

par(mfrow = c(1,5))
for(i in 1:5){
  x <- data3_2 + (-2 + (i-1)*1) * sqrt(lamda[2]) * loading[,2]
  a1 <- unlist(c(x[1],x[3],x[5],x[7],x[9],x[11],x[13],x[15]))
  a2 <- unlist(c(x[2],x[4],x[6],x[8],x[10],x[12],x[14],x[16]))
  plot(a1,a2)
  lines(a1,a2)
}

par(mfrow = c(1,5))
for(i in 1:5){
  x <- data3_2 + (-2 + (i-1)*1) * sqrt(lamda[3]) * loading[,3]
  a1 <- unlist(c(x[1],x[3],x[5],x[7],x[9],x[11],x[13],x[15]))
  a2 <- unlist(c(x[2],x[4],x[6],x[8],x[10],x[12],x[14],x[16]))
  plot(a1,a2)
  lines(a1,a2)
}

par(mfrow = c(1,5))
for(i in 1:5){
  x <- data3_2 + (-2 + (i-1)*1) * sqrt(lamda[4]) * loading[,4]
  a1 <- unlist(c(x[1],x[3],x[5],x[7],x[9],x[11],x[13],x[15]))
  a2 <- unlist(c(x[2],x[4],x[6],x[8],x[10],x[12],x[14],x[16]))
  plot(a1,a2)
  lines(a1,a2)
}


par(mfrow = c(1,1))
data8 <- read.csv('pendigit8.txt', sep = ",", header = F)
data <- rbind(data3,data8)                  
data_1 <- data[,1:16]
pri_data <- princomp(data_1)
loading1 <- pri_data$loadings
lamda1 <- (pri_data$sdev)^2
score1 <- pri_data$scores
pairs(score1, col = c(rep("blue",1105),rep("red",1105)))
plot(lamda1, main = "scree", xlab = "component", ylab = "lamda")
lines(lamda1)
plot(cumsum(lamda1)*100/sum(lamda1),main = "cumscree", xlab = "compoent", ylab = "cumulative percent")
lines(cumsum(lamda1)*100/sum(lamda1))
pairs(score1[,1:5], col = c(rep("blue",1105),rep("red",1105)))



