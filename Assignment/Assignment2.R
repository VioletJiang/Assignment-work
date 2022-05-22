#Assignment2
setwd("C:/Users/DELL/Documents/GitHub/May2022_homework/data/")
newapp<-read.csv("Assignment 2 - new.app4.csv")
UAA<-read.csv("Assignment 2 - USA_AL_Auburn-Opelika.AP.722284_TMY3_BASE.csv")

library(tidyverse)
library(dplyr)
library(stringr)
library(ggplot2)
df<-separate(newapp, col = time, into = c("date", "minutes"), sep = " ")
df$date<-substr(df$date,1,nchar(df$date)-5)
df<-separate(df,col=minutes,into=c("hour","minutes"),sep=":")

df1<-df %>%
  group_by(date,hour) %>%
  summarize(kW_hourly = sum(W_min)/1000)
df1$date<-gsub('[/]', '-', df1$date)
df1$date<-as.Date(df1$date,format="%m-%d")
df1$date<-substr(df1$date,6,10)
df1$hour[df1$hour=="0"]<-"24"

UAA$Date.Time<-trimws(UAA$Date.Time,which=c("left"))
UAA1<-separate(UAA,col=Date.Time,into=c("date","hour"),sep="\\s+")
UAA1$hour<-as.character(as.numeric(substr(UAA1$hour,1,2)))
UAA1$date<-gsub('[/]', '-', UAA1$date)

mergedata<-df1%>%
  right_join(UAA1,by=c("date","hour"))

mergedata<-as.data.frame(mergedata)
mergedata_1<-mutate(mergedata,total=rowSums(as.data.frame(select(mergedata,contains("kW"))),na.rm = TRUE))

mergeall<-as.data.frame(mergedata_1)
mergeall$hour<-as.numeric(mergeall$hour)
mergeall= mergeall[order(mergeall[,2],mergeall[,1]),]
mergeall$date<-as.Date.character(paste("2013",mergeall$date,sep = "-"))
write.table(mergeall,"mergeall.csv",row.names=FALSE,col.names=TRUE,sep=",")

#Hourly:

#boxplot
boxplot(as.data.frame(select(mergeall,contains("kW"))),xlab="electricity product",ylab="electricity consumption",main="electricity consumption of each project")
#From the figure, we can see the electricity consumption of each project. The difference between the electricity consumption of the newapp and other electricity consumption is obvious. The average hourly electricity consumption of newapp is about 40kw.

#total
a<-as.data.frame(mergedata_1)
a$hour<-as.numeric(a$hour)
a1 = a[order(a[,2],a[,1]),]
hourtotal<-aggregate(a1$total,by=list(hour=a1$hour),sum)
barplot(hourtotal[,2]~hourtotal[,1],main="total electricity consumption hourly",ylab="electricity consumption",xlab="hour")

#The noon period is the peak period of electricity consumption, and the electricity consumption in the morning and evening is more average


#monthly:

a1$month<-substr(a1$date,1,2)
monthtotal<-aggregate(a1$total,by=list(month=a1$month),sum)
barplot(monthtotal[,2]~monthtotal[,1],main="total electricity consumption monthly",ylab="electricity consumption",xlab="month")
#Judging from the monthly total electricity consumption, there is a certain semi-annual periodicity.Electricity consumption decreased from January to May and rebounded rapidly in June. From July to November, it showed a downward trend, and began to rise after November.

all<-aggregate(a1[,3:16],by=list(month=a1$month),FUN=sum,na.rm=TRUE)
all1<-as.data.frame(all)
all1$month<-as.numeric(all1$month)
product<-colnames(a1[,3:16])
y<-as.data.frame(rep(product,each=12))
y$month<-rep(1:12,14)
colnames(y)<-c("product","month")

z<-c(NULL)
for (i in 1:14) {
  z1<-t(all1[,i+1])
  z<-c(z,z1)
}
y$kw<-z


ggplot(data=y,aes(month,kw,fill=product))+  
  geom_bar(stat="identity",position="stack", color="black", width=0.7,size=0.25)


#It can be seen that the electricity consumption of newapp mainly occurs between June and September. Consumption of water heater (pink) remain at similar levels throughout the year. Consumption of Heating (blue) occurs in cooler months (Q1 and Q4), on the contrary, consumption of cooling (orange) mainly occurs when it is warmer(June to September).


#weekday
a1$year<-as.Date(paste("2013",a1$date,sep="-"))
library(lubridate)
library(timeDate)
Sys.setlocale("LC_TIME", "English")
a1$day<-weekdays(a1$year)

weekdaytotal<-aggregate(a1$total,by=list(day=a1$day),sum)
barplot(weekdaytotal[,2]~weekdaytotal[,1],main="total electricity consumption weekdays",ylab="electricity consumption",xlab="weekday")
#Yearly electricity usage is similar from a weekly perspective.

#each product

weekall<-aggregate(a1[,3:16],by=list(week=a1$day),FUN=sum,na.rm=TRUE)
weekall1<-as.data.frame(weekall)
product<-colnames(a1[,3:16])
y<-as.data.frame(rep(product,each=7))
week<-weekall1$week
y$week<-rep(t(week),14)
colnames(y)<-c("product","week")

z<-c(NULL)
for (i in 1:14) {
  z1<-t(weekall1[,i+1])
  z<-c(z,z1)
}
y$kw<-z


ggplot(data=y,aes(week,kw,fill=product))+  
  geom_bar(stat="identity",position="stack", color="black", width=0.7,size=0.25)

#The electricity consumption of each product is relatively uniform from the point of view of the week, and there is no drastic increase or decrease.


#Overall:
#1.although from an hourly perspective, whether there is a new app's electricity consumption has a great impact on the hourly electricity consumption. But looking at the monthly and weekly data, the newapp's electricity usage did not have an abnormal effect.
#2.monthly data shows a semi-annual cycle
#3.Yearly electricity usage is similar from a weekly perspective.
