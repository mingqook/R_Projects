library(sarima)
library(forecast)


#####5.5
data_5 <- read.csv("ex_ch5_5.txt")
View(data_5)

######(1)
plot(data_5$data, type = "l")
#####  시각 적으로 약 10~15를 주기로 계절성을 가진다
findfrequency(data_5$data)
##### 12의 주기를 가진다고 확인 할 수 있다.

######(2)
plot(diff(data_5$data, lag = 12), type = "l")
##### lag 12으로 차분을 해줬더니 평균값을 0을 가지고 분산이 존재하며 정상성을 만족한다고 볼 수 있다.

#####(3)
data_5_1 <- ts(data_5$data, frequency = 12)
fit_5_1 <- auto.arima(data_5_1)
fit_5_1
