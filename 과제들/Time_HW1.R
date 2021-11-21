######## 7번

x0 <- 0
k0 <- c()
set.seed(1)
for(i in 1:10000){
  k0[i] <- x0
  e <- rnorm(1)
  x0 <- x0 + e
}
k0
plot(k0)
####정상성 없음

x1 <- 0
k1 <- c()
for(j in 1:10000){
  k1[j] <- x1
  x1 <- x1 + j
}
k1
plot(k1)
####정상성 없음

x2 <- 0
k2 <- c()
for(l in 1:10000){
 k2[l] <- x2
 x2 <- x2 + sin(l)
}
k2
plot(k2)
####정상성 있음

######## 8번
library(stringr)
library(zoo)
library(ggplot2)
kospi <- read.csv("ex_ch1_8.csv", stringsAsFactor = FALSE)
kospi1 <- kospi[,1]
kospi2 <- str_replace(kospi[,2],",","")
kospi2 <- as.numeric(kospi2)
kospi3 <- data.frame(kospi1,kospi2)
colnames(kospi3) <- c("year","index")
plot(kospi3, type = "c", col = "red")
View(kospi)

kospi2_5 <- rollmean(kospi2, 5, align = "left")
kospi3_5 <- data.frame(kospi1[1:length(kospi2_5)],kospi2_5)
colnames(kospi3_5) <- c("year","index")
plot(kospi3_5, col = "blue")

kospi2_7 <- rollmean(kospi2, 7, align = "left")
kospi3_7 <- data.frame(kospi1[1:length(kospi2_7)],kospi2_7)
colnames(kospi3_7) <- c("year","index")
plot(kospi3_7, col = "blue")

kospi2_10 <- rollmean(kospi2, 10, align = "left")
kospi3_10 <- data.frame(kospi1[1:length(kospi2_10)],kospi2_10)
colnames(kospi3_10) <- c("year","index")
plot(kospi3_10, col = "blue")

kospi2_15 <- rollmean(kospi2, 15, align = "left")
kospi3_15 <- data.frame(kospi1[1:length(kospi2_15)],kospi2_15)
colnames(kospi3_15) <- c("year","index")
plot(kospi3_15, col = "blue")

kospi2_20 <- rollmean(kospi2, 20, align = "left")
kospi3_20 <- data.frame(kospi1[1:length(kospi2_20)],kospi2_20)
colnames(kospi3_20) <- c("year","index")
plot(kospi3_20, col = "blue")

gg1 <- ggplot(data = kospi3, aes(x = year,y= index, group = 1))
gg + geom_line(color = "grey", lwd = 2) + geom_line(data = kospi3_5,aes(x = year,y= index, group = 1), color = "red") + geom_line(data = kospi3_7,aes(x = year,y= index, group = 1), color = "green") + geom_line(data = kospi3_10,aes(x = year,y= index, group = 1), color = "black") + geom_line(data = kospi3_15,aes(x = year,y= index, group = 1), color = "blue") + geom_line(data = kospi3_20,aes(x = year,y= index, group = 1), color = "yellow", lwd = 2) 



######## 9번
a <- read.csv("ex_ch1_9.csv", stringsAsFactors = FALSE)
View(a)

y <- matrix(a[21:100,2], nc = 1)
z <- matrix(nr = 80, nc = 20)


for(i in 1:20){
  z[,21-i] <- a[i:(79+i),2]
}
View(y)
View(z)
f <- lm(formula = y~z)
f1 <- coef(f)[2:21]
f1

results <- c()
for(i in 1:10){
  results[i] <- sum(f1*a[(99+i):(80+i),2])
}
results

ff <- lm(formula = y~z+0) #### 원점을 지나는 회귀모형
ff1 <- coef(ff)
ff1

results1 <- c()
for(i in 1:10){
  results1[i] <- sum(ff1*a[(99+i):(80+i),2])
}
results1

