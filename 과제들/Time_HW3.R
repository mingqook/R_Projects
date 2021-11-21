library("forecast")

#######3.6
set.seed(200423)

data_6 <- arima.sim(model = list(ar = c(0.5), ma = c(2)), n = 100)
fit_6 <- arima(data_6, order = c(1,0,1), include.mean = T)
fit_6$coef
pre_6 <- forecast(fit_6, h=10)
pre_6
plot(pre_6)

######3.7
set.seed(200423)

data_n_7 <- arima.sim(model = list(ar = c(0.7)), n = 100)
mle_n_7 <- Arima(data_n_7, order = c(1,0,0), method = "ML")
lse_n_7 <- Arima(data_n_7, order = c(1,0,0), method = "CSS")
mle_n_7
mle_n_7$coef
mle_n_7$var.coef
lse_n_7
lse_n_7$coef
lse_n_7$var.coef

data_t_7 <- arima.sim(model = list(ar = c(0.7)), n = 100, rand.gen = function(n,...) rt(n,4))
mle_t_7 <- Arima(data_t_7, order = c(1,0,0), method = "ML")
lse_t_7 <- Arima(data_t_7, order = c(1,0,0), method = "CSS")
mle_t_7
mle_t_7$coef
mle_t_7$var.coef
lse_t_7
lse_t_7$coef
lse_t_7$var.coef

#####3.8
set.seed(200423)

data_8 <- arima.sim(model = list(ar = c(-0.2,0.48)), n = 100, rand.gen = function(n,...) rt(n,4))
fit_8 <- arima(data_8, order = c(2,0,0))
esti <- fit_8$coef
covar <- solve(fit_8$var.coef)
margin <- qchisq(0.95,2)

##### CI = tr((true - esti))%*%covar%*%(true-esti)<=margin


#####3.10
set.seed(200423)

data_10 <- arima.sim(model = list(ar=c(0.7), ma=c(-0.2)), n = 300)
acf_10 <- acf(data_10)
head(acf_10$acf)
plot(acf_10)

#####3.12
set.seed(200423)

data_12 <- arima.sim(model = list(ma=c(0.5)), n = 300)
fit_12 <- Arima(data_12, order = c(0,0,1))
fit_12

pre12 <- forecast(fit_12, h = 30)
pre12
