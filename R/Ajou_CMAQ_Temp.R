library(dplr)
library(lubridate)
library(ggplot2)

setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\기상자료\\2006-2011")
lf1<-list.files()
setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\기상자료\\2012-2019")
lf2<-list.files()
setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\기상자료\\2020-2023\\2020-2023")
lf3<-list.files()

temp_index1<-grep("daily.AFNLv10.K09.TEMP2",lf1)
temp_index2<-grep("daily.AFNLv20e.K03B.TEMP2",lf2)
temp_index3<-lf3[grep("daily",lf3)]
temp_index3<-grep("TEMP",temp_index3)

#풍속 IDNEX
wind_index1<-grep("daily.AFNLv10.K09.WSPD",lf1)
wind_index2<-grep("daily.AFNLv20e.K03B.WSPD",lf2)
wind_index3<-lf3[grep("daily",lf3)]
wind_index3<-grep("WSPD",wind_index3)

temp_dat1=NULL
temp_dat2=NULL
temp_dat3=NULL

wind_dat1=NULL
wind_dat2=NULL
wind_dat3=NULL

setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\기상자료\\2006-2011")
for(i in 1:length(temp_index1)){
  
  temp_dat1[[i]]<-read.table(lf1[temp_index1[i]],header=T)
  print(i)}
for(i in 1:length(wind_index1)){
  wind_dat1[[i]]<-read.table(lf1[wind_index1[i]],header=T)
  print(i)}

setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\기상자료\\2012-2019")
for(i in 1:length(temp_index2)){
  temp_dat2[[i]]<-read.table(lf2[temp_index2[i]],header=T)
  print(i)}
for(i in 1:length(wind_index2)){
  wind_dat2[[i]]<-read.table(lf2[wind_index2[i]],header=T)
  print(i)}

setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\기상자료\\2020-2023\\2020-2023")
for(i in 1:length(temp_index3)){
  temp_dat3[[i]]<-read.table(lf3[temp_index3[i]],header=T)
  print(i)}


for(i in 1:length(wind_index3)){
  wind_dat3[[i]]<-read.table(lf3[wind_index3[i]],header=T)
  print(i)}

temp_06_11<-do.call(rbind,temp_dat1)
temp_12_19<-do.call(rbind,temp_dat2)
temp_20_23<-do.call(rbind,temp_dat3)

wind_06_11<-do.call(rbind,wind_dat1)
wind_12_19<-do.call(rbind,wind_dat2)
wind_20_23<-do.call(rbind,wind_dat3)

temp<-rbind(temp_06_11,temp_12_19,temp_20_23)
wind<-rbind(wind_06_11,wind_12_19,wind_20_23)

temp$DATE=ymd(temp$DATE)
wind$DATE=ymd(wind$DATE)

summary(temp$DATE)
summary(wind$DATE)

library(reshape2)
long_temp<-melt(temp,id.vars="DATE")
long_wind<-melt(wind,id.vars="DATE")
head(long_wind)

temp_wind<-as.data.frame(cbind(long_temp,long_wind[,3]))
names(temp_wind)=c("date","SGG","meanT","WSPD")

head(temp_wind)

library(readxl)
sgg<-read_excel("D:\\EUMC\\데이터관리\\아주대PM2.5\\CMAQ_SGG_link.xlsx")
sgg$SGG=paste0("X",sgg$SGG)
library(dplyr)

temp_wind_sgg_ajou<-temp_wind  %>% left_join(sgg,by="SGG")

setwd("D:\\EUMC\\데이터관리\\아주대PM2.5\\기상자료")
write.csv(temp_wind_sgg_ajou,file="temp_wind_sgg_ajou.csv",row.names=F,na="",fileEncoding = "euc-kr")

