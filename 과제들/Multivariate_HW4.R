
library("mvtnorm")
library("caret")
library("GGally")

iris1 <- iris[iris$Species != "setosa",]

iris1_ve <- iris1[iris1$Species == "versicolor",1:4]
iris1_vi <- iris1[iris1$Species == "virginica",1:4]

mean_ve <- sapply(iris1_ve, mean)
mean_vi <- sapply(iris1_vi, mean)

var_ve <- cov(iris1_ve)
var_vi <- cov(iris1_vi)

dmvnorm(iris1[,1:4], mean_ve, var_ve) ## f_ve = f_p
dmvnorm(iris1[,1:4], mean_ve, var_vi) ## f_vi = f_n

pi_ve <- nrow(iris1_ve) / nrow(iris1) ## pi_p
pi_vi <- nrow(iris1_vi) / nrow(iris1) ## pi_n

ve_vi <- 1 ## c(p|n)
vi_ve <- 10 ## c(n|p)

predicted <- c()

for(i in 1:nrow(iris1)) {
  if((dmvnorm(iris1[i,1:4],mean_ve,var_ve))/dmvnorm(iris1[i,1:4],mean_vi,var_vi) > ((ve_vi * pi_vi) / (vi_ve * pi_ve))){
    predicted[i] <- "versicolor"
  }
  else predicted[i] <- "virginica"
}

confusionMatrix(as.factor(predicted),as.factor(iris1$Species))

iris2 <- cbind(iris1[,-5],predicted)
ggscatmat(iris1, columns = 1:4, color = "Species")
ggscatmat(iris2, columns = 1:4, color = "predicted")

