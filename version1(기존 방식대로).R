library(readxl)
library(data.table)
library(lubridate)
library(ggplot2)

file_list <- list.files(pattern=".xlsx")
file_path <- paste0(getwd(),"/",file_list)

TC_avg <- as.data.table(read_excel(path="5TC.xlsx",col_names = TRUE, col_types = c("date", "numeric", "numeric", "numeric")))
Time_C <- as.data.table(read_excel(path="용선료.xlsx",col_names = TRUE))

names(TC_avg) <- c("Date", "Pana", "Supra", "Cape")

names(Time_C) <- c("Date", "Pana_6m", "Pana_1y", "Pana_3y", "Pana_5y",
                   "Cape_6m", "Cape_1y", "Cape_3y", "Cape_5y",
                   "Supra_6m", "Supra_1y", "Supra_3y", "Supra_5y")

TC_avg[,Date := as.Date.POSIXct(Date)]
Time_C[,Date := as.Date.POSIXct(Date)]

# data는 2001년도부터 사용 #

Cape <- TC_avg[!is.na(Cape), .(Date, Cape)]
Pana <- TC_avg[!is.na(Pana), .(Date, Pana)]
Supra <- TC_avg[!is.na(Supra), .(Date, Supra)]

# 주간으로 data 통합 #

C <- Cape[, .(max = max(Cape), min = min(Cape), mean = mean(Cape), St = Cape[1], Cl = Cape[length(Cape)]), 
     by = .(year = year(Date), week = format(Date,"%W"))]

Cape_Data <- Time_C[, .(Date, Cape_6m, Cape_1y, Cape_3y, Cape_5y)]
Cape_Data[,`:=`(year = year(Date), week = format(Date,"%W"))]
Cape_Data <- Cape_Data[C,,on = .(year, week), nomatch=FALSE]
Cape_Data[,`:=`(year = NULL, week =NULL)]

# 1주일 후 예측 데이터

seq(100,by = -1,length.out=2)

Cape_Data[-length(Date), mean_1w := Cape_Data[-1, mean]]
Cape_Data[-seq(length(Date), by = -1, length.out=2), mean_2w := Cape_Data[-(1:2), mean]]
Cape_Data[-seq(length(Date), by = -1, length.out=3), mean_3w := Cape_Data[-(1:3), mean]]
Cape_Data[-seq(length(Date), by = -1, length.out=4), mean_4w := Cape_Data[-(1:4), mean]]

# 의미있는 변수 도출 #

Result <- matrix(,6,4)
colnames(Result) <- c("1w", "2w", "3w", "4w")
rownames(Result) <- c("6m/1y", "6m/3y", "6m/5y", "1y/3y", "1y/5y", "3y/5y")

Result[1,1] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_1y, mean_1w - mean>0))$p.value]
Result[2,1] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_3y, mean_1w - mean>0))$p.value]
Result[3,1] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_5y, mean_1w - mean>0))$p.value]

Result[4,1] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_3y, mean_1w - mean>0))$p.value]
Result[5,1] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_5y, mean_1w - mean>0))$p.value]

Result[6,1] <- Cape_Data[,chisq.test(table(Cape_3y > Cape_5y, mean_1w - mean>0))$p.value]


Result[1,2] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_1y, mean_2w - mean>0))$p.value]
Result[2,2] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_3y, mean_2w - mean>0))$p.value]
Result[3,2] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_5y, mean_2w - mean>0))$p.value]

Result[4,2] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_3y, mean_2w - mean>0))$p.value]
Result[5,2] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_5y, mean_2w - mean>0))$p.value]

Result[6,2] <- Cape_Data[,chisq.test(table(Cape_3y > Cape_5y, mean_2w - mean>0))$p.value]


Result[1,3] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_1y, mean_3w - mean>0))$p.value]
Result[2,3] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_3y, mean_3w - mean>0))$p.value]
Result[3,3] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_5y, mean_3w - mean>0))$p.value]

Result[4,3] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_3y, mean_3w - mean>0))$p.value]
Result[5,3] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_5y, mean_3w - mean>0))$p.value]

Result[6,3] <- Cape_Data[,chisq.test(table(Cape_3y > Cape_5y, mean_3w - mean>0))$p.value]


Result[1,4] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_1y, mean_4w - mean>0))$p.value]
Result[2,4] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_3y, mean_4w - mean>0))$p.value]
Result[3,4] <- Cape_Data[,chisq.test(table(Cape_6m > Cape_5y, mean_4w - mean>0))$p.value]

Result[4,4] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_3y, mean_4w - mean>0))$p.value]
Result[5,4] <- Cape_Data[,chisq.test(table(Cape_1y > Cape_5y, mean_4w - mean>0))$p.value]

Result[6,4] <- Cape_Data[,chisq.test(table(Cape_3y > Cape_5y, mean_4w - mean>0))$p.value]
round(Result,2)
Cape_Data[,cor.test(Cape_6m, mean_1w)]
Cape_Data[,cor.test(Cape_1y, mean_1w)]
Cape_Data[,cor.test(Cape_3y, mean_1w)]
Cape_Data[,cor.test(Cape_5y, mean_1w)]

Cape_Data[,cor.test(Cape_6m, mean_2w)]
Cape_Data[,cor.test(Cape_1y, mean_2w)]
Cape_Data[,cor.test(Cape_3y, mean_2w)]
Cape_Data[,cor.test(Cape_5y, mean_2w)]

Cape_Data[,cor.test(Cape_6m, mean_3w)]
Cape_Data[,cor.test(Cape_1y, mean_3w)]
Cape_Data[,cor.test(Cape_3y, mean_3w)]
Cape_Data[,cor.test(Cape_5y, mean_3w)]

Cape_Data[,cor.test(Cape_6m, mean_4w)]
Cape_Data[,cor.test(Cape_1y, mean_4w)]
Cape_Data[,cor.test(Cape_3y, mean_4w)]
Cape_Data[,cor.test(Cape_5y, mean_4w)]


g <- Cape_Data[Date<"2014-01-01",lm(mean_1w~ mean + max + min + St + Cl)]
g1 <- lm(Cape_Data[Date<"2014-01-01",mean_1w]~Cape_Data[Date<"2014-01-01",mean])
summary(g)
par(mfrow=c(1,2))
plot(Cape_Data[,.(Date, mean_1w)], type="l", xlim=c(as.Date("2004-01-01"),as.Date("2018-12-31")))

# lines(Cape_Data[as.numeric(names(g$fitted.values)),Date], g$fitted.values, col=2)
lines(Cape_Data[,Date],predict(g,Cape_Data),col=2)
plot(Cape_Data[,Date],Cape_Data[,mean_1w]-predict(g,Cape_Data),col=2,type="l")
lines(Cape_Data[,Date],455.3182+Cape_Data[,mean*0.9901],col=3)


Cape_Data[,chisq.test(table(Cape_6m > Cape_1y, mean_4w - mean>0))$p.value]
Cape_Data[,chisq.test(table(Cape_6m > Cape_3y, mean_4w - mean>0))$p.value]
Cape_Data[,chisq.test(table(Cape_6m > Cape_5y, mean_4w - mean>0))$p.value]

Cape_Data[,chisq.test(table(Cape_1y > Cape_3y, mean_4w - mean>0))$p.value]
Cape_Data[,chisq.test(table(Cape_1y > Cape_5y, mean_4w - mean>0))$p.value]

Cape_Data[,chisq.test(table(Cape_3y > Cape_5y, mean_4w - mean>0))$p.value]



ggplot(data = Cape_Data) + 
  geom_line(aes(x = Date, y = Cl), color = "#999999", size = 1) +
  geom_line(aes(x = Date, y = Cape_6m), color = "orange", size = 1) +
  geom_line(aes(x = Date, y = Cape_1y), color = "sky blue", size = 1) +
  geom_line(aes(x = Date, y = Cape_3y), color = "#CC79A7", size = 1) +
  geom_line(aes(x = Date, y = Cape_5y), color = "yellow", size = 1) 








Cape_Da




plot(Cape, type="l")
lines(Pana, col=2)
lines(Supra, col=3)

col <- Cape_Data[,ifelse(Cl>St, '#D55E00', '#0072B2')]

ggplot(data = Cape_Data, aes(x = Date, y = Cl)) + 
  geom_line(color = '#999999', size = 1) +
  geom_linerange(aes(x = Date, ymin = min, ymax = max), color = col, size = 1) 
  
  ggtitle('Cape 5T/C') +
  xlab('Date') + ylab('Cape') + theme_bw() +
  theme(plot.title = element_text(size = 17, color = 'violetred'),
        axis.title = element_text(size = 13),
        axis.title.y = element_text(angle = 0),
        axis.text.x = element_text(angle = 90, size = 13))

  
  
