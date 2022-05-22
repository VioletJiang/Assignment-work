
library(openxlsx)
library(corrplot)
library(forecast)
library(ggplot2)
library(tseries)
library(xts)

setwd("C:/Users/DELL/Documents/GitHub/May2022_homework/data/")
ts<-read.xlsx("Assignment 3 - timeseries_data.xlsx",detectDates = TRUE)
ts$DATETIME<-paste(ts$MARKETDAY,ts$HOURENDING,sep=" ")

summary(ts)

#NA-Wind
plot(ts$`ERCOT.(WIND_RTI)`,type="l")
#Random fluctuations, we can consider using the average of the near value of the missing value to fill NA.
ts1<-ts
posi<-c(which(is.na(ts1$`ERCOT.(WIND_RTI)`)))
for (i in 1:length(posi)) {
  j=posi[i]
  ts1$`ERCOT.(WIND_RTI)`[j]<-mean(ts1$`ERCOT.(WIND_RTI)`[(j-5):(j+5)],na.rm=TRUE)
}
summary(ts1$`ERCOT.(WIND_RTI)`)
#NA-solar
plot(ts1$`ERCOT.(GENERATION_SOLAR_RT)`,type="l")
#show a rising trend,we can consider using the average of the near value of the missing value to fill NA.
posi1<-c(which(is.na(ts1$`ERCOT.(GENERATION_SOLAR_RT)`)))
for (i in 1:length(posi1)) {
  j=posi1[i]
  ts1$`ERCOT.(GENERATION_SOLAR_RT)`[j]<-mean(ts1$`ERCOT.(GENERATION_SOLAR_RT)`[(j-5):(j+5)],na.rm=TRUE)
}
summary(ts1$`ERCOT.(GENERATION_SOLAR_RT)`)

#change month to numeric type
ts1$month_number<-substr(ts1$MARKETDAY,6,7)


#RTLMP monthly
boxplot(ts1[,2]~ts1$month_number,main="RTLMP monthly",ylab="RTLMP",xlab="month",par(las="1"))
#Each month has different degrees of outliers, among which Jan, Jun, Jul outliers deviate greatly.
Rmon<-aggregate(ts1$`HB_NORTH.(RTLMP)`,by=list(month=ts1$month_number),sum)
barplot(Rmon[,2]~Rmon[,1],main="RTLMP monthly",ylab="RTLMP",xlab="month")
#RTLMP hourly
boxplot(ts1[,2]~ts1[,6],main="RTLMP hourly",ylab="RTLMP",xlab="hour",par(las="1"))
Rhour<-aggregate(ts1$`HB_NORTH.(RTLMP)`,by=list(hour=ts1$HOURENDING),sum)
barplot(Rhour[,2]~Rhour[,1],main="RTLMP hourly",ylab="RTLMP",xlab="hour")
#The price is the highest in the afternoon(16-17), showing a trend of first rising and then falling as a whole within a day.
#RTLMP Peaktype
Rtype<-aggregate(ts1$`HB_NORTH.(RTLMP)`,by=list(peaktype=ts1$PEAKTYPE),sum)
pie(Rtype$x,labels=Rtype$peaktype)

#year
boxplot(ts1$`HB_NORTH.(RTLMP)`~ts1$YEAR,main="RTLMP yearly",ylab="RTLMP",xlab="Year")
#Outliers are more pronounced during 2018. The average RTLMP of each year is similar.




#class peaktype
table(ts1$PEAKTYPE)
for(i in 1:length(ts1$PEAKTYPE)){
  if(ts1$PEAKTYPE[i]=="OFFPEAK"){
    ts1$pt[i]<-1
  }else if(ts1$PEAKTYPE[i]=="WDPEAK"){
    ts1$pt[i]<-2
  }else if(ts1$PEAKTYPE[i]=="WEPEAK"){
    ts1$pt[i]<-3
  }
}
ts1$pt<-as.numeric(ts1$pt)


#WIND_RTI
summary(ts1$`ERCOT.(WIND_RTI)`)
#monthly
boxplot(ts1$`ERCOT.(WIND_RTI)`~ts1$month_number,main="WIND_RTI monthly",ylab="WIND_RTI",xlab="month",par(las="1"))
#There are no outliers, and the monthly mean shows a certain periodicity

#hourly
Whour<-aggregate(ts1$`ERCOT.(WIND_RTI)`,by=list(hour=ts1$HOURENDING),sum)
barplot(Whour[,2]~Whour[,1],main="Wind hourly",ylab="Wind",xlab="hour")
#Wind generation are higher in the morning and at night, with lower wind generation at noon

#peaktype
windtype<-aggregate(ts1$`ERCOT.(WIND_RTI)`,by=list(peaktype=ts1$PEAKTYPE),sum)
pie(windtype$x,labels=windtype$peaktype)
#Wind generation in WEPEAK is the lowest during 2017-2018. And Wind genertation in OFFPEAK and WDPEAK seems similar.

#year
boxplot(ts1$`ERCOT.(WIND_RTI)`~ts1$YEAR,main="Wind yearly",ylab="Wind",xlab="Year")
#the wind generation in 2018 is higher than that of 2017




#SOLAR
#monthly
boxplot(ts1$`ERCOT.(GENERATION_SOLAR_RT)`~ts1$month_number,main="SOLAR monthly",ylab="SOLAR generation",xlab="month",par(las="1"))
#Outliers are more pronounced in January and December.April to September is the month with strong solar generation, and the mean of solar generation is almost 0 in other months.

#hourly
Shour<-aggregate(ts1$`ERCOT.(GENERATION_SOLAR_RT)`,by=list(hour=ts1$HOURENDING),sum)
barplot(Shour[,2]~Shour[,1],main="solar hourly",ylab="solar",xlab="hour")
#8am to 9pm is the period of solar generation. During this period, the solar generation shows a trend of rising first, reaching the highest peak at 2pm, and then decreasing. It is consistent with the intensity of the sun we perceive in our daily life

#peaktype
solartype<-aggregate(ts1$`ERCOT.(GENERATION_SOLAR_RT)`,by=list(peaktype=ts1$PEAKTYPE),sum)
pie(solartype$x,labels=solartype$peaktype)
#OffPEAK solar energy production is almost 0, the main solar energy generation period is WDPEAK

#year
boxplot(ts1$`ERCOT.(GENERATION_SOLAR_RT)`~ts1$YEAR,main="solar yearly",ylab="solar",xlab="Year")
#the solar generation in 2018 is higher than that of 2017

#RTLOAD
summary(ts1$`ERCOT.(RTLOAD)`)
#monthly
boxplot(ts1$`ERCOT.(RTLOAD)`~ts1$month_number,main="RTLOAD monthly",ylab="RTLOAD",xlab="month")
#RTLOAD has a clear upward arch during May to September, but there are no outliers during this period, and the other months are in a lower position, but there are more outliers.

#hourly
RThour<-aggregate(ts1$`ERCOT.(RTLOAD)`,by=list(hour=ts1$HOURENDING),sum)
barplot(RThour[,2]~RThour[,1],main="RTLOAD hourly",ylab="RTLOAD",xlab="hour")
#RTLOAD has a clear upward arch during afternoon.

#peaktype
RTtype<-aggregate(ts1$`ERCOT.(RTLOAD)`,by=list(peaktype=ts1$PEAKTYPE),sum)
pie(RTtype$x,labels=RTtype$peaktype)
#Maximum RTLOAD during WDPEAK period

#year
boxplot(ts1$`ERCOT.(RTLOAD)`~ts1$YEAR,main="RTLOAD yearly",ylab="RTLOAD",xlab="Year")
#the RTLOAD in 2018 is higher than that of 2017



#correlation
a<-ts1[,c(2:6,11)]
colnames(a)<-c("RTLMP","WIND","SOLAR","RTLOAD","H","pt")
a$pt<-as.numeric(a$pt)
co<-cor(a)
co
corrplot(co,type="upper", tl.col="black",tl.srt=45)
#SOLAR and RTLOAD, SOLAR and class of peaktype are weakly related.


#predict RTLMP

#30% trainset and 70% testset
n<-length(ts1[,1])
n*0.7

train<-ts1[1:10491,]
test<-ts1[10492:n,]
colnames(train)<-c("Datetime","RTLMP","WIND","SOLAR","RTLOAD","H","day","peak","M","Y","pt","mon_num")
colnames(test)<-c("Datetime","RTLMP","WIND","SOLAR","RTLOAD","H","day","peak","M","Y","pt","mon_num")

#model1-linear regression model with other factor.
library(MASS)
a<-train[,c(2:6,8:10)]
fullmodel <- lm(RTLMP~., data = a)
summary(fullmodel)

stepmodel <- stepAIC(fullmodel, direction = "both", trace = TRUE)
summary(stepmodel)

#It can be seen that the linear regression model performs poorly


#whole time series
#Trend-ADF test
adf.test(ts1$`HB_NORTH.(RTLMP)`)
#the time series data are stationary.

plot(ts1$`HB_NORTH.(RTLMP)`)

acf(ts1$`HB_NORTH.(RTLMP)`)
#
df<-ts1
df$DATETIME<-paste(df$DATETIME,"00","00",sep = ":")
df$DATETIME<-as.POSIXct(df$DATETIME, format='%Y-%m-%d %H:%M:%S')
dfxts<-xts(x=df$`HB_NORTH.(RTLMP)`,order.by=df$DATETIME)

#handle the outliers
#tsoutliers(dfxts)
dfxts_1<-tsclean(dfxts)
summary(dfxts_1)
plot(dfxts)
plot(dfxts_1)
a<-ts(dfxts_1)
acf(a)
adf.test(a)
#Time series is more stable after handling outliers, and acf plot shows a circle.



#eliminate periodicity
nsdiffs(dfxts_1)
ndiffs(dfxts_1)
dfnew<-diff(dfxts_1,1)
ndiffs(dfnew)
plot(dfnew)
acf(ts(dfnew[2:14987]))

#train and test set
train2<-first(dfnew,10491)
train2<-train2[2:10491]
test2<-last(dfnew,4496)


#ARIMA MODEL
arima_model<-auto.arima(ts(train2),trace = T,seasonal = FALSE)
accuracy(arima_model)
arima.model2 <- Arima(test2,model=arima_model)
accuracy(arima.model2)

checkresiduals(arima_model)
checkresiduals(arima.model2)
#the residuals is not a white noise

#forecast
plot(forecast(arima_model,h=4496))
#Only the mean is predicted, the volatility is not predicted



#ARIMA- growth rate
n<-length(dfxts)
re<-as.data.frame(matrix(NA,nrow=(n-1),ncol=1))
for(i in 25:n){
  re[(i-24),]<-(dfxts[[i]]-dfxts[[(i-24)]])/dfxts[[(i-24)]]
}

rets<-ts(re)
rets_1<-tsclean(rets)
plot(rets,type="l")
plot(rets_1,type="l")
adf.test(rets_1)
acf(rets_1)

train3<-first(rets_1,10491)
test3<-last(rets_1,4496)

m<-auto.arima(train3)
summary(m)
accuracy(m)
checkresiduals(m)

fit<-Arima(test3,model = m)
summary(fit)


val<-last(rets_1,300)
fit2<-Arima(val,model=m)
plot(forecast(fit2,h=100))

#predicted RTLMP in next 24 hours
pre<-forecast(fit2,h=24)
Yt<-pre$mean
R<-c()
for(i in 1:24){
  R[i]<-(Yt[i]+1)*df$`HB_NORTH.(RTLMP)`[14986+i-25]
}
x<-as.vector(df$`HB_NORTH.(RTLMP)`[14900:14987])
plot(x,type="l",xlim=c(0,120),ylab="RTLMP")
R1<-as.data.frame(R)
R1$y<-seq(89,112,1)
lines(R1$y,y=R1$R,col="red")



#nnetar growth rate
fit<-nnetar(ts(train3))
fit2<-nnetar(ts(test3),model=fit)
summary(fit)
accuracy(fit)
accuracy(fit2)

#check residuals
Box.test(fit$residuals,type="Ljung-Box")
a<-as.data.frame(fit$residuals)
acf(a[27:10491,])

Box.test(fit2$residuals,type="Ljung-Box")
a<-as.data.frame(fit2$residuals)
acf(a[27:4496,])

#much better results than arima

val<-last(rets_1,300)
te<-nnetar(ts(val),model=fit)
plot(forecast(te,h=100,PI=TRUE))

#prediction RTLMP for next 24 hours
all<-nnetar(ts(rets_1),model=fit)
pre<-forecast(all,h=24)
Y<-pre$mean
R<-c()
for(i in 1:24){
  R[i]<-(Y[i]+1)*df$`HB_NORTH.(RTLMP)`[14986+i-25]
}
x<-as.vector(df$`HB_NORTH.(RTLMP)`[14900:14987])
plot(x,type="l",xlim=c(0,120),ylab="RTLMP")
R1<-as.data.frame(R)
R1$y<-seq(89,112,1)
points(R1$y,y=R1$R,col="red")




