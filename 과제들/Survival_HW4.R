library("survival")
library("KMsurv")

data("larynx")
data <- larynx
surv_data <- Surv(time = data$time, event = data$delta)
data_reg_log <- survreg(surv_data ~ factor(stage) + age, dist = "loglogistic", data = data)
summary(data_reg_log)

mu_hat <- data_reg_log$coefficients[1]
sigma_hat <- data_reg_log$scale
lamda_hat <- exp(-mu_hat/sigma_hat)
alpha_hat <- 1/sigma_hat

c(lamda_hat,alpha_hat)

beta_hat <- -data_reg_log$coefficients[2:length(data_reg_log$coefficients)] / sigma_hat
beta_hat

exp(beta_hat[1])  ## stage1에 비해 stage2 일 때 odds비의 증가분 (odss 비 = s/1-s) ## 강의록 p.80 ## proportional odds model
exp(beta_hat[2])  ## stage1에 비해 stage3 일 때 odds비의 증가분 (odss 비 = s/1-s)
exp(beta_hat[3])  ## stage1에 비해 stage4 일 때 odds비의 증가분 (odss 비 = s/1-s) 
