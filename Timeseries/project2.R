library(ggplot2)
library(lubridate)

chn10 <- read.csv("chn10_CHINA_daily.csv", stringsAsFactors = F)
chn2.5 <- read.csv("chn25_CHINA_daily.csv", stringsAsFactors = F)
kor10 <- read.csv("kor10_KOREA_daily.csv", stringsAsFactors = F)
kor2.5 <- read.csv("kor25_KOREA_daily.csv", stringsAsFactors = F)

View(chn10)

chn10[,4] <- substr(chn10[,2],1,4)
chn10[,4] <- paste(chn10[,4],"년",sep = "")
chn10[,5] <- substr(chn10[,2],6,7)
chn10[,5] <- paste(chn10[,5],"월",sep = "")
chn10[,6] <- substr(chn10[,2],9,10)
chn10[,4] <- as.factor(chn10[,4])
chn10[,5] <- as.factor(chn10[,5])
chn10[,2] <- as.Date(chn10[,2])
chn10[,7] <- as.factor(rep(10))
chn10[,8] <- as.factor(rep("중국"))
colnames(chn10)[2:8] <- c("DATE","농도","년","월","일","종류","국가")
chn10 <- chn10[-1,]
chn10[,1] <- chn10[,1] - 1

chn2.5[,4] <- substr(chn2.5[,2],1,4)
chn2.5[,4] <- paste(chn2.5[,4],"년",sep = "")
chn2.5[,5] <- substr(chn2.5[,2],6,7)
chn2.5[,5] <- paste(chn2.5[,5],"월",sep = "")
chn2.5[,6] <- substr(chn2.5[,2],9,2.5)
chn2.5[,4] <- as.factor(chn2.5[,4])
chn2.5[,5] <- as.factor(chn2.5[,5])
chn2.5[,2] <- as.Date(chn2.5[,2])
chn2.5[,7] <- as.factor(rep(2.5))
chn2.5[,8] <- as.factor(rep("중국"))
colnames(chn2.5)[2:8] <- c("DATE","농도","년","월","일","종류","국가")

kor10[,4] <- substr(kor10[,2],1,4)
kor10[,4] <- paste(kor10[,4],"년",sep = "")
kor10[,5] <- substr(kor10[,2],6,7)
kor10[,5] <- paste(kor10[,5],"월",sep = "")
kor10[,6] <- substr(kor10[,2],9,10)
kor10[,4] <- as.factor(kor10[,4])
kor10[,5] <- as.factor(kor10[,5])
kor10[,2] <- as.Date(kor10[,2])
kor10[,7] <- as.factor(rep(10))
kor10[,8] <- as.factor(rep("한국"))
colnames(kor10)[2:8] <- c("DATE","농도","년","월","일","종류","국가")

kor2.5[,4] <- substr(kor2.5[,2],1,4)
kor2.5[,4] <- paste(kor2.5[,4],"년",sep = "")
kor2.5[,5] <- substr(kor2.5[,2],6,7)
kor2.5[,5] <- paste(kor2.5[,5],"월",sep = "")
kor2.5[,6] <- substr(kor2.5[,2],9,2.5)
kor2.5[,4] <- as.factor(kor2.5[,4])
kor2.5[,5] <- as.factor(kor2.5[,5])
kor2.5[,2] <- as.Date(kor2.5[,2])
kor2.5[,7] <- as.factor(rep(2.5))
kor2.5[,8] <- as.factor(rep("한국"))
colnames(kor2.5)[2:8] <- c("DATE","농도","년","월","일","종류","국가")

chn_data <- rbind(chn10, chn2.5)
kor_data <- rbind(kor10, kor2.5)
data_10 <- rbind(chn10, kor10)
data_2.5 <- rbind(chn2.5, kor2.5)
data_total <- rbind(chn_data, kor_data)

summary(chn10)
summary(chn2.5)
summary(kor10)
summary(kor2.5)
summary(kor_data)

ggplot(data = chn_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_line()  + scale_x_date(breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("미세먼지 종류에따른 중국의 미세먼지량 추이")

ggplot(data = chn_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth() + scale_x_date(breaks="3 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("미세먼지 종류에따른 중국의 미세먼지량 추이 - gam")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다.

ggplot(data = chn_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth(method = "loess") + scale_x_date(breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("미세먼지 종류에따른 중국의 미세먼지량 추이 - loess")
######시간이 흐름에 따라서 전체적으로 감소하는 경향을 보이고 있다.

ggplot(data = kor_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_line() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("미세먼지 종류에따른 한국의 미세먼지량 추이")

ggplot(data = kor_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth() + scale_x_date(date_breaks="3 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("미세먼지 종류에따른 한국의 미세먼지량 추이 - gam")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다.

ggplot(data = kor_data, aes(x = DATE, y = 농도, color = 종류)) +
  geom_smooth(method = "loess") + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("미세먼지 종류에따른 한국의 미세먼지량 추이 - loess")
######시간이 흐름에 따라서 전체적으로 감소하는 경향을 보이고 있다.

ggplot(data = data_10, aes(x = DATE, y = 농도, color = 국가)) +
  geom_line() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("국가에따른 pm10 미세먼지량 추이")

ggplot(data = data_10, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("국가에따른 pm10 미세먼지량 추이 - gam")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다. 중국과 한국이 비슷한 계절성을 보인다

ggplot(data = data_10, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth(method = "loess") + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("국가에따른 pm10 미세먼지량 추이 - loess")
##### 중국과 한국 모두 점차 감소하는 경향을 보이고 있다.

ggplot(data = data_2.5, aes(x = DATE, y = 농도, color = 국가)) +
  geom_line() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("국가에따른 pm2.5 미세먼지량 추이")

ggplot(data = data_2.5, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth() + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("국가에따른 pm2.5 미세먼지량 추이 - gam")
#####봄에서 여름까지 증가하고 여름부터는 다시 증가하는 경향을 보인다. 중국과 한국이 비슷한 계절성을 보인다.

ggplot(data = data_2.5, aes(x = DATE, y = 농도, color = 국가)) +
  geom_smooth(method = "loess") + scale_x_date(date_breaks="6 month") + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14)) +ggtitle("국가에따른 pm2.5 미세먼지량 추이 - loess")
#####중국은 감소하다가 2018년 여름을 기준으로 다시 증가하는 경향을 보이지만 한국은 꾸준히 감소하는 경향을 보인다.


ggplot(data = chn_data, aes(x = mday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line()  +
  facet_wrap(~ 월) +
  theme_bw() +
  geom_smooth()+ theme(plot.title = element_text(face = "bold", size = 14)) + ggtitle("미세먼지 종류에따른 중국의 월별 미세먼지량 추이") +xlab("일")

ggplot(data = chn_data, aes(x = mday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() +
  facet_grid(~ 월) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 14))+
  ggtitle("미세먼지 종류에따른 중국의 월별 미세먼지량 추이")+xlab("일")

ggplot(data = chn_data, aes(x = mday(DATE), y = 농도 , group=종류, colour=종류)) +
  facet_grid(~ 월) +
  theme_bw() +
  geom_smooth()+ theme(plot.title = element_text(face = "bold", size = 14)) + ggtitle("미세먼지 종류에따른 중국의 월별 미세먼지량 추이")+xlab("일")


ggplot(data = chn_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~ 년) +
  theme_bw()+ theme(plot.title = element_text(face = "bold", size = 14)) +
  ggtitle("미세먼지 종류에따른 중국의 연도별 미세먼지량 추이")+xlab("일")

ggplot(data = chn_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~ 년) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 14))+
  geom_smooth() +ggtitle("미세먼지 종류에따른 중국의 연도별 미세먼지량 추이")+xlab("일")


ggplot(data = kor_data, aes(x = mday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() +
  facet_wrap(~월) +
  theme_bw() + theme(axis.text.x = element_text(angle = 30, hjust=1), plot.title = element_text(face = "bold", size = 14))+
  geom_smooth() + ggtitle("미세먼지 종류에따른 한국의 월별 미세먼지량 추이")+xlab("일")

ggplot(data = kor_data, aes(x = mday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line()+
  facet_grid(~월) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 14))+
  ggtitle("미세먼지 종류에따른 한국의 월별 미세먼지량 추이")+xlab("일")

ggplot(data = kor_data, aes(x = mday(DATE), y = 농도 , group=종류, colour=종류)) +
  scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~월) +
  theme_bw() +
  geom_smooth()+ theme(plot.title = element_text(face = "bold", size = 14)) + ggtitle("미세먼지 종류에따른 한국의 월별 미세먼지량 추이")+xlab("일")


ggplot(data = kor_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_wrap(~년) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 14))+
  ggtitle("미세먼지 종류에따른 한국의 연도별 미세먼지량 추이")+xlab("일")

ggplot(data = kor_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  geom_line() + scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~년) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 14))+
  ggtitle("미세먼지 종류에따른 한국의 연도별 미세먼지량 추이")+xlab("일")

ggplot(data = kor_data, aes(x = yday(DATE), y = 농도 , group=종류, colour=종류)) +
  scale_x_discrete(breaks = seq(1,30,5)) +
  facet_grid(~년) +
  theme_bw() + theme(plot.title = element_text(face = "bold", size = 14))+
  geom_smooth() +ggtitle("미세먼지 종류에따른 한국의 연도별 미세먼지량 추이")+xlab("일")


ggplot(data = chn10, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14))+ggtitle("연도별 중국의 pm10 미세먼지량 추이")+xlab("일")

ggplot(data = chn2.5, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14)) +ggtitle("연도별 중국의 pm2.5 미세먼지량 추이")+xlab("일")

ggplot(data = kor10, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14)) +ggtitle("연도별 한국의 pm10 미세먼지량 추이")+xlab("일")

ggplot(data = kor2.5, aes(x = yday(DATE), y = 농도, group = 년, color = 년)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14)) +ggtitle("연도별 한국의 pm2.5 미세먼지량 추이")+xlab("일")

ggplot(data = data_10, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14)) +ggtitle("연도별 한국과 중국의 pm10 미세먼지량 추이")+xlab("일")

ggplot(data = data_10, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_smooth()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14)) +ggtitle("연도별 한국과 중국의 pm10 미세먼지량 추이")+xlab("일")

ggplot(data = data_2.5, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_line()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14)) +ggtitle("연도별 한국과 중국의 pm2.5 미세먼지량 추이")+xlab("일")

ggplot(data = data_2.5, aes(x = yday(DATE), y = 농도, group = 국가, color = 국가)) +
  geom_smooth()  + facet_wrap(~ 년) +
  theme(axis.text.x = element_text(angle = 30, hjust=1),plot.title = element_text(face = "bold", size = 14)) +ggtitle("연도별 한국과 중국의 pm2.5 미세먼지량 추이")+xlab("일")

