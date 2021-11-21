install.packages("openintro")
install.packages("ggplot2")
library(openintro)
library(ggplot2)
data("starbucks")
View(starbucks)
attach(starbucks) #dataset의 변수명을 직접 사용할 수 있음
#1-1
ggplot(data = starbucks, aes(x=calories, y=carb)) + geom_point() + labs(x = "Calories", y="Carbohydrate")
##선형관계가 있다고 볼 수 있다.

#1-2
summary(lm(carb ~ calories))

#1-3
# intercept 8.94356이지만 p-value 값이 커서 유의하다고 볼 수 없다. 
# 기울기는 0.10603으로 유의하다 

#1-4
#R^2값은 0.4556, adjusted R^2는 0.4484 calories가 carbohydrates를 45%정도 설명한다.

#1-5
res <- lm(carb ~ calories)
res_1 <- fortify(res)
ggplot(res_1, aes(x = .fitted, y=.resid)) + geom_point() + labs(x="fitted",y="residuals") #residual plot 그리기
ggplot(res, aes(x=fitted(res),y=residuals(res))) + geom_point() + labs(x="fitted",y="residuals") #위와 똑같은 그래프 나옴
#0을 기준으로 random하게 나타나므로 선형성을 확인할 수는 있으나 fitted값이 커질수록 분산이 커지는 것이 확인된다
#선형성은 만족하나 등분산성은 장담할 수 없다.

#2-1
absent <- read.csv("absenteeism.csv")
absent_1 <- absent
absent_1$eth <- as.numeric(absent_1$eth) - 1
absent_1$sex <- as.numeric(absent_1$sex) - 1
absent_1$lrn <- as.numeric(absent_1$lrn) - 1
str(absent_1)
View(absent_1)

#2-2
absent_lm <- lm(days ~ eth + sex + lrn, data = absent_1)
summary(absent_lm)

#2-3
fitted(absent_lm)
# y = 18.932 - 9.112eth +3.104sex + 2.154lrn 
# intercept와 eth의 계수는 p value가 매우 작아서 유의하지만 sex와 lrn의 계수는 p value가 유의수준 0.05보다 커서 유의하지 않다.

#2-4
summary(absent_lm)
# adjusted R^2  값은 0.07009로 매우 작다. 즉 모델이 7%정도밖에 설명력을 갖지 못한다.

#2-5
absent_lm_1 <- fortify(absent_lm)
ggplot(absent_lm_1, aes(x = .fitted, y=.resid)) + geom_point() + labs(x="fitted",y="residuals") 
#선형성을 확인할 수 없다. 하지만 random하게 고르게 퍼져 있으므로 등분산성이 있다고 볼 수 있다.(???)

#2-6
install.packages("tidyverse")
library(tidyverse)
newdata <- data_frame(eth = c(1,1,1,0,0), sex = c(0,1,0,1,0), lrn = c(0,0,1,1,0))
predict_day <- predict(absent_lm, newdata = newdata) #newdata와 lm의 변수명이 일치해야 함
newdata_1 <- cbind(newdata,predict_day)
newdata_1

#3 -> 다항함수는 I(x^2) 이용
#3번 다시 제대로 할 것

f <- function(x) { x^2 }
get_sim_data <- function(f, sample_size = 100) { 
  x = runif(n = sample_size, min = 0, max = 1) 
  y = rnorm(n = sample_size, mean = f(x), sd = 0.3) 
  data.frame(x, y) 
}

#poly와 i()의 차이 , fitted()와 $fitted.values의 차이
set.seed(1)
sim_data <- get_sim_data(f)
lm1 <- lm(y ~ 1, data = sim_data)
lm2 <- lm(y ~ x, data = sim_data)
lm3 <- lm(y ~ poly(x, degree = 2), data = sim_data)
lm4 <- lm(y ~ poly(x, degree = 9), data = sim_data)
sim_data_1 <- data.frame(x = sim_data$x, y = sim_data$y, lm1 = lm1$fitted.values, lm2 = lm2$fitted.values, lm3 = lm3$fitted.values, lm4 = lm4$fitted.values)
ggplot(data = sim_data_1) + geom_point(aes(x=x, y=y)) + geom_line(aes(x = x, y = lm1),color = 1) + geom_line(aes(x = x, y = lm2),color=2) + geom_line(aes(x = x, y = lm3),color=3)+ geom_line(aes(x = x, y = lm4),color=4)

n_sims <- 250
n_models <- 4
df <- data.frame(0.90)
colnames(df) <- 'x'
r <- data.frame(NA)
for(sim in 1:n_sims){
  sim_data <- get_sim_data(f)
  lm1 <- lm(y ~ 1, data = sim_data)
  lm2 <- lm(y ~ x, data = sim_data)
  lm3 <- lm(y ~ x + I(x^2), data = sim_data)
  lm4 <- lm(y ~ x + I(x^2) + I(x^3) + I(x^4) + I(x^5) + I(x^6) + I(x^7) + I(x^8) + I(x^9), data = sim_data)
   #함수를 어떻게 선택하면 되지? 인덱스 부여해서 선택할 수 있으면 좋을텐데
  r[sim,1] <- predict(lm1, newdata = df)
  r[sim,2] <- predict(lm2, newdata = df)
  r[sim,3] <- predict(lm3, newdata = df)
  r[sim,4] <- predict(lm4, newdata = df)
}
colnames(r)<- c('0','1','2','9')
boxplot(r, xlab = "degree", ylab = "prediction")
#ggplot 이용해서는 어떻게 그릴 수 있을까?