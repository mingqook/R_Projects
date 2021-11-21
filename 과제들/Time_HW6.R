#####11번

library(fGarch)
library(rugarch)


g_1 <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.4), rseed=1) 
data_1 <- garchSim(spec = g_1, n=1000)
plot(data_1)

g_2 <- garchSpec(model = list(omega=1, alpha=0.01, beta=0.01), rseed=1) 
data_2 <- garchSim(spec = g_2, n=1000)
plot(data_2)

g_3 <- garchSpec(model = list(omega=1, alpha=0.5, beta=0.01), rseed=1) 
data_3 <- garchSim(spec = g_3, n=1000)
plot(data_3)

g_4 <- garchSpec(model = list(omega=1, alpha=0.01, beta=0.5), rseed=1) 
data_4 <- garchSim(spec = g_4, n=1000)
plot(data_4)

g_5 <- garchSpec(model = list(omega=1, alpha=0.6, beta=0.3999999), rseed=1) 
data_5 <- garchSim(spec = g_5, n=1000)
plot(data_5)

##### 자유도 4인 t분포
g_t_1 <- garchSpec(model = list(omega=1, alpha=0.3, beta=0.4, shape = 4),
                   rseed=1, cond.dist="std") 
data_t_1 <- garchSim(spec = g_t_1, n=1000)
plot(data_t_1)

g_t_2 <- garchSpec(model = list(omega=1, alpha=0.01, beta=0.01, shape = 4),
                   rseed=1, cond.dist="std") 
data_t_2 <- garchSim(spec = g_t_2, n=1000)
plot(data_t_2)

g_t_3 <- garchSpec(model = list(omega=1, alpha=0.5, beta=0.01, shape = 4),
                   rseed=1, cond.dist="std") 
data_t_3 <- garchSim(spec = g_t_3, n=1000)
plot(data_t_3)

g_t_4 <- garchSpec(model = list(omega=1, alpha=0.01, beta=0.5, shape = 4),
                   rseed=1, cond.dist="std") 
data_t_4 <- garchSim(spec = g_t_4, n=1000)
plot(data_t_4)

g_t_5 <- garchSpec(model = list(omega=1, alpha=0.6, beta=0.3999999, shape = 4),
                   rseed=1, cond.dist="std") 
data_t_5 <- garchSim(spec = g_t_5, n=1000)
plot(data_t_5)




##### 14번

##### garch(1,1) 적합
data_14 <- read.table("ex_ch6_14.txt", stringsAsFactors = F)
data_14 <- data_14[-1,]
data_14 <- as.numeric(data_14[,2])


fit_14 <- garchFit(formula = ~ garch(1,1), data = data_14)
para <- fit_14@fit$par
para

### change point detection
cov_cal <- function(data, h) {
  n <- length(data)
  m <- mean(data)
  x <- data[1:(n-h)] - m
  y <- data[(h+1):n] - m
  return(sum(x * y/n))
}

var_cal <- function(data, max_h=length(data)^(1/3)) {
  n <- length(data)
  sd2_hat <- cov_cal(data, 0)
  for(i in 1:max_h) {
    sd2_hat <- sd2_hat + 2*(cov_cal(data, i))
  }
  return(sd2_hat)
}

CUSUM_cal <- function(data) {
  ## return : maximum cusum test statistics and change point
  n <- length(data)
  cusum <- abs((cumsum(data) - (1:n)/n*sum(data) ) / ( sqrt(n) * sqrt(var_cal(data))))
  argmax <- which.max(cusum)
  if(max(cusum)>1.358) return(list("CUSUM_statistics" = max(cusum), "change_point"=argmax))
  else return(print("no change"))
}

CUSUM_cal(data_14)

plot(data_14, type = "l")
