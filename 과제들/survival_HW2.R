# hw2_5번 S(x)의 추정치를 지수분포가정, 와이불분포가정해서 구한 후 KM 추정치와 비교하라 
# 지수분포와 와이불분포는 parametric -> mle를 통해 모수를 먼저 추정
# 이는 lab3 슬라이드 3쪽부터 11쪽까지 참고

library(survival)
library(KMsurv)

data(drug6mp)
da <- drug6mp

# KM 그리기
mp_time <- c(mp$t1,mp$t2)
mp_event <- c(rep(1,length(mp$t1)),mp$relapse)
surv_x_1 <- Surv(time = mp_time, event = mp_event )
surv_x_1_fit <- survfit(formula = surv_x_1 ~ 1, data = surv_x_1, conf.type = "plain")
plot(surv_x_1_fit)

mp_time <- c(mp$t1,mp$t2)
mp_event <- c(rep(1,length(mp$t1)),mp$relapse)
surv_x_2 <- Surv(time = mp$t1, event = rep(1,length(mp$t1)) )
surv_x_2_fit <- survfit(formula = surv_x_2 ~ 1, data = surv_x_2, conf.type = "plain")
plot(surv_x_2_fit)

mp_time <- c(mp$t1,mp$t2)
mp_event <- c(rep(1,length(mp$t1)),mp$relapse)
surv_x_3 <- Surv(time = mp$t2, event = mp$relapse )
surv_x_3_fit <- survfit(formula = surv_x_3 ~ 1, data = surv_x_3, conf.type = "plain")
plot(surv_x_3_fit)

# 지수분포의 MLE를 통해 그리기
lamda_hat_1 <- sum(mp_event)/sum(mp_time) # 지수분포의 mle
curve(exp(-lamda_hat_1*x), from = 0, to = max(mp_time) + 1, col = "blue", add = T)

lamda_hat_2 <- sum(sum(rep(1,length(mp$t1))))/sum(mp$t1) # 지수분포의 mle
curve(exp(-lamda_hat_2*x), from = 0, to = max(mp$t1) + 1, col = "green", add = T)

lamda_hat_3 <- sum(mp$relapse)/sum(mp$t2) # 지수분포의 mle
curve(exp(-lamda_hat_3*x), from = 0, to = max(mp$t2) + 1, col = "red", add = T)



# 와이불의 MLE를 통해 그리기
alpha_1 <- 1.141 # 대입으로 직접 찾음, ln_1식을 먼저 구하고, lamda_1 식을 찾은 후 a에 일일이 값을 대입
lamda_1 <- sum(mp_event) / sum((mp_time)^alpha_1) # 직접 손으로 전개
ln_1 <- log(alpha_1*lamda_1)*sum(mp_event) + (alpha_1 - 1)*sum(log(mp_time)*mp_event) - lamda_1 * sum((mp_time)^alpha_1)



# 직접 손으로 전개, 최대 가 되야함
curve(exp(-lamda_1*(x^alpha_1)), from = 0, to = max(mp_time) + 1, col = "red", add = T)
