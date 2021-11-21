library(ggplot2)
library(lubridate)

chn10 <- read.csv("chn10.csv", stringsAsFactors = F)
chn25 <- read.csv("chn25.csv", stringsAsFactors = F)
kor10 <- read.csv("kor10.csv", stringsAsFactors = F)
kor25 <- read.csv("kor25.csv", stringsAsFactors = F)


chn10_data <- chn10[,c(2,9)]
chn10_data[,3] <- substr(chn10_data[,1],1,4)
chn10_data[,3] <- paste(chn10_data[,3],"년",sep = "")
chn10_data[,4] <- substr(chn10_data[,1],6,7)
chn10_data[,4] <- paste(chn10_data[,4],"월",sep = "")
chn10_data[,5] <- substr(chn10_data[,1],9,10)
chn10_data[,3] <- as.factor(chn10_data[,3])
chn10_data[,4] <- as.factor(chn10_data[,4])
chn10_data[,5] <- chn10_data[,5]
chn10_data[,1] <- as.Date(chn10_data[,1])
chn10_data[,6] <- as.factor(rep(10))
chn10_data[,7] <- as.factor(rep("중국"))
colnames(chn10_data)[2:7] <- c("농도","년","월","일","종류","국가")

chn25_data <- chn25[,c(2,9)]
chn25_data[,3] <- substr(chn25_data[,1],1,4)
chn25_data[,3] <- paste(chn25_data[,3],"년",sep = "")
chn25_data[,4] <- substr(chn25_data[,1],6,7)
chn25_data[,4] <- paste(chn25_data[,4],"월",sep = "")
chn25_data[,5] <- substr(chn25_data[,1],9,10)
chn25_data[,3] <- as.factor(chn25_data[,3])
chn25_data[,4] <- as.factor(chn25_data[,4])
chn25_data[,5] <- chn25_data[,5]
chn25_data[,1] <- as.Date(chn25_data[,1])
chn25_data[,6] <- as.factor(rep(25))
chn25_data[,7] <- as.factor(rep("중국"))
colnames(chn25_data)[2:7] <- c("농도","년","월","일","종류","국가")

kor10_data <- kor10[,c(2,6)]
kor10_data[,3] <- substr(kor10_data[,1],1,4)
kor10_data[,3] <- paste(kor10_data[,3],"년",sep = "")
kor10_data[,4] <- substr(kor10_data[,1],6,7)
kor10_data[,4] <- paste(kor10_data[,4],"월",sep = "")
kor10_data[,5] <- substr(kor10_data[,1],9,10)
kor10_data[,3] <- as.factor(kor10_data[,3])
kor10_data[,4] <- as.factor(kor10_data[,4])
kor10_data[,5] <- kor10_data[,5]
kor10_data[,1] <- as.Date(kor10_data[,1])
kor10_data[,6] <- as.factor(rep(10))
kor10_data[,7] <- as.factor(rep("한국"))
colnames(kor10_data)[2:7] <- c("농도","년","월","일","종류","국가")

kor25_data <- kor25[,c(2,6)]
kor25_data[,3] <- substr(kor25_data[,1],1,4)
kor25_data[,3] <- paste(kor25_data[,3],"년",sep = "")
kor25_data[,4] <- substr(kor25_data[,1],6,7)
kor25_data[,4] <- paste(kor25_data[,4],"월",sep = "")
kor25_data[,5] <- substr(kor25_data[,1],9,10)
kor25_data[,3] <- as.factor(kor25_data[,3])
kor25_data[,4] <- as.factor(kor25_data[,4])
kor25_data[,5] <- as.factor(kor25_data[,5])
kor25_data[,1] <- kor25_data[,1]
kor25_data[,6] <- as.factor(rep(25))
kor25_data[,7] <- as.factor(rep("한국"))
colnames(kor25_data)[2:7] <- c("농도","년","월","일","종류","국가")

chn_data <- rbind(chn10_data, chn25_data)
kor_data <- rbind(kor10_data, kor25_data)
data_10 <- rbind(chn10_data, kor10_data)
data_25 <- rbind(chn25_data, kor25_data)
data_total <- rbind(chn_data, kor_data)


View(chn_data)
str(chn_data)

summary(chn10_data)
summary(chn25_data)
summary(kor10_data)
summary(kor25_data)



ggplot(data = chn_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_line()  + scale_x_date(breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("미세먼지 종류에따른 중국의 미세먼지량 추이")

ggplot(data = chn_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth() + scale_x_date(breaks="3 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("미세먼지 종류에따른 중국의 미세먼지량 추이 - 계절성 확인")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다.

ggplot(data = chn_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth(method = "loess") + scale_x_date(breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("미세먼지 종류에따른 중국의 미세먼지량 추이 - 증가성 확인")
######시간이 흐름에 따라서 전체적으로 감소하는 경향을 보이고 있다.

ggplot(data = kor_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_line() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("미세먼지 종류에따른 한국의 미세먼지량 추이")

ggplot(data = kor_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth() + scale_x_date(date_breaks="3 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("미세먼지 종류에따른 한국의 미세먼지량 추이 - 계절성 확인")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다.

ggplot(data = kor_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth(method = "loess") + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("미세먼지 종류에따른 한국의 미세먼지량 추이 - 증가성 확인")
######시간이 흐름에 따라서 전체적으로 감소하는 경향을 보이고 있다.

ggplot(data = data_10, aes(x = DATE, y = 농도, color = 국가)) +
  geom_line() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("국가에따른 10미세먼지량 추이")

ggplot(data = data_10, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("국가에따른 10미세먼지량 추이 - 계절성 확인")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다. 중국과 한국이 비슷한 계절성을 보인다

ggplot(data = data_10, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth(method = "loess") + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("국가에따른 10미세먼지량 추이 - 증가성 확인")
##### 중국과 한국 모두 점차 감소하는 경향을 보이고 있다.

ggplot(data = data_25, aes(x = DATE, y = 농도, color = 국가)) +
  geom_line() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("국가에따른 25미세먼지량 추이")

ggplot(data = data_25, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("국가에따른 25미세먼지량 추이 - 계절성 확인")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다. 중국과 한국이 비슷한 계절성을 보인다.

ggplot(data = data_25, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth(method = "loess") + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("국가에따른 25미세먼지량 추이 - 증가성 확인")
#####중국은 감소하다가 2018년 여름을 기준으로 다시 증가하는 경향을 보이지만 한국은 꾸준히 감소하는 경향을 보인다.


ggplot(data = chn_data, aes(x = 일, y = 농도 , group=종류, colour=종류)) +
  geom_line() +scale_x_discrete(breaks = seq(1,30,5)) +
  facet_wrap(~ 월) +
  theme_bw() +
  geom_smooth() + ggtitle("미세먼지 종류에따른 중국의 월별 미세먼지량 추이")

ggplot(data = chn_data, aes(x = 일, y = 농도 , group=종류, colour=종류)) +
  geom_line() +scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~ 월) +
  theme_bw() +
  ggtitle("미세먼지 종류에따른 중국의 월별 미세먼지량 추이")

ggplot(data = chn_data, aes(x = 일, y = 농도 , group=종류, colour=종류)) +
  scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~ 월) +
  theme_bw() +
  geom_smooth() + ggtitle("미세먼지 종류에따른 중국의 월별 미세먼지량 추이")


ggplot(data = chn_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~ 년) +
  theme_bw() +
  ggtitle("미세먼지 종류에따른 중국의 연도별 미세먼지량 추이")

ggplot(data = chn_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
   scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~ 년) +
  theme_bw() +
  geom_smooth() +ggtitle("미세먼지 종류에따른 중국의 연도별 미세먼지량 추이")


ggplot(data = kor_data, aes(x = 일, y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_wrap(~월) +
  theme_bw() +
  geom_smooth() + ggtitle("미세먼지 종류에따른 한국의 월별 미세먼지량 추이")

ggplot(data = kor_data, aes(x = 일, y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~월) +
  theme_bw() +
  ggtitle("미세먼지 종류에따른 한국의 월별 미세먼지량 추이")

ggplot(data = kor_data, aes(x = 일, y = 농도 , group=종류, colour=종류)) +
  scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~월) +
  theme_bw() +
  geom_smooth() + ggtitle("미세먼지 종류에따른 한국의 월별 미세먼지량 추이")


ggplot(data = kor_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_wrap(~년) +
  theme_bw() +
  ggtitle("미세먼지 종류에따른 한국의 연도별 미세먼지량 추이")

ggplot(data = kor_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~년) +
  theme_bw() +
  ggtitle("미세먼지 종류에따른 한국의 연도별 미세먼지량 추이")

ggplot(data = kor_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~년) +
  theme_bw() +
  geom_smooth() +ggtitle("미세먼지 종류에따른 한국의 연도별 미세먼지량 추이")



ggplot(data = chn10_data, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 중국의 미세먼지(10)량 추이")

ggplot(data = chn25_data, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 중국의 미세먼지(25)량 추이")

ggplot(data = kor10_data, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 한국의 미세먼지(10)량 추이")

ggplot(data = kor25_data, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 한국의 미세먼지(25)량 추이")

ggplot(data = data_10, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 한국과 중국의 미세먼지(10)량 추이")

ggplot(data = data_10, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_smooth()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 한국과 중국의 미세먼지(10)량 추이")

ggplot(data = data_25, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 한국과 중국의 미세먼지(25)량 추이")

ggplot(data = data_25, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_smooth()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1)) +ggtitle("연도별 한국과 중국의 미세먼지(25)량 추이")

