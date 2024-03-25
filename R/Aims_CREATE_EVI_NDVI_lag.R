#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
pacman::p_load(dplyr,ggplot2,lubridate,readxl,reshape2)
setwd("D:\\EUMC\\데이터관리\\대기오염모델링자료_SNU\\data")

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
list.files()

evi1 <-read.csv("evi_nomask.csv",fileEncoding="euc-kr")
evi2 <-read.csv("evi_summaryeq0.csv",fileEncoding="euc-kr")
ndvi1<-read.csv("ndvi_nomask.csv",fileEncoding="euc-kr")
ndvi2<-read.csv("ndvi_summaryeq0.csv",fileEncoding="euc-kr")

unique(evi1$SIGUNGU_CD) %>% length
unique(evi2$SIGUNGU_CD) %>% length
unique(ndvi1$SIGUNGU_CD) %>% length
unique(ndvi2$SIGUNGU_CD) %>% length

table(sort(unique(evi1$SIGUNGU_CD))==sort(unique(evi2$SIGUNGU_CD)))
sgg<-sort(unique(evi1$SIGUNGU_CD))

evi_ndvi_func<-function(data){
  file.list=NULL
  
  for(i in 1:length(sgg)){
    
    sgg<-unique(data$SIGUNGU_CD)
    d<-subset(data,SIGUNGU_CD==sgg[i])
    
    d2<-d %>% select(year,month1:month12)
    d3<-melt(d2,id.vars="year")
    names(d3)=c("year","month","EXP")
    d3$month=as.numeric(gsub("month","",d3$month))
    d3$SIGUNGU_CD=unique(d$SIGUNGU_CD)
    d3$SIDO_NM   =unique(d$SIDO_NM)
    d3$SIGUNGU_NM=unique(d$SIGUNGU_NM)
    d3<-d3 %>% arrange(year,month)
    
    d3$EXP_s1 =lag(d3$EXP,1)
    d3$EXP_s2 =lag(d3$EXP,2)
    d3$EXP_s3 =lag(d3$EXP,3)
    d3$EXP_s4 =lag(d3$EXP,4)
    d3$EXP_s5 =lag(d3$EXP,5)
    d3$EXP_s6 =lag(d3$EXP,6)
    d3$EXP_s7 =lag(d3$EXP,7)
    d3$EXP_s8 =lag(d3$EXP,8)
    d3$EXP_s9 =lag(d3$EXP,9)
    d3$EXP_s10=lag(d3$EXP,10)
    d3$EXP_s11=lag(d3$EXP,11)
    d3$EXP_s12=lag(d3$EXP,12)
    d3$EXP_s13=lag(d3$EXP,13)
    d3$EXP_s14=lag(d3$EXP,14)
    d3$EXP_s15=lag(d3$EXP,15)
    d3$EXP_s16=lag(d3$EXP,16)
    d3$EXP_s17=lag(d3$EXP,17)
    d3$EXP_s18=lag(d3$EXP,18)
    d3$EXP_s19=lag(d3$EXP,19)
    d3$EXP_s20=lag(d3$EXP,20)
    d3$EXP_s21=lag(d3$EXP,21)
    d3$EXP_s22=lag(d3$EXP,22)
    d3$EXP_s23=lag(d3$EXP,23)
    d3$EXP_s24=lag(d3$EXP,24)
    d3$EXP_s25=lag(d3$EXP,25)
    d3$EXP_s26=lag(d3$EXP,26)
    d3$EXP_s27=lag(d3$EXP,27)
    d3$EXP_s28=lag(d3$EXP,28)
    d3$EXP_s29=lag(d3$EXP,29)
    d3$EXP_s30=lag(d3$EXP,30)
    d3$EXP_s31=lag(d3$EXP,31)
    d3$EXP_s32=lag(d3$EXP,32)
    d3$EXP_s33=lag(d3$EXP,33)
    d3$EXP_s34=lag(d3$EXP,34)
    d3$EXP_s35=lag(d3$EXP,35)
    d3$EXP_s36=lag(d3$EXP,36)
    d3$EXP_s37=lag(d3$EXP,37)
    d3$EXP_s38=lag(d3$EXP,38)
    d3$EXP_s39=lag(d3$EXP,39)
    d3$EXP_s40=lag(d3$EXP,40)
    d3$EXP_s41=lag(d3$EXP,41)
    d3$EXP_s42=lag(d3$EXP,42)
    d3$EXP_s43=lag(d3$EXP,43)
    d3$EXP_s44=lag(d3$EXP,44)
    d3$EXP_s45=lag(d3$EXP,45)
    d3$EXP_s46=lag(d3$EXP,46)
    d3$EXP_s47=lag(d3$EXP,47)
    d3$EXP_s48=lag(d3$EXP,48)
    d3$EXP_s49=lag(d3$EXP,49)
    d3$EXP_s50=lag(d3$EXP,50)
    d3$EXP_s51=lag(d3$EXP,51)
    d3$EXP_s52=lag(d3$EXP,52)
    d3$EXP_s53=lag(d3$EXP,53)
    d3$EXP_s54=lag(d3$EXP,54)
    d3$EXP_s55=lag(d3$EXP,55)
    d3$EXP_s56=lag(d3$EXP,56)
    d3$EXP_s57=lag(d3$EXP,57)
    d3$EXP_s58=lag(d3$EXP,58)
    d3$EXP_s59=lag(d3$EXP,59)
    d3$EXP_s60=lag(d3$EXP,60)
    
    d3$EXP_m1 =with(d3,apply(d3 %>% select(EXP,EXP_s1),1,mean,na.rm=T))
    d3$EXP_m2 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s2 ),1,mean,na.rm=T))
    d3$EXP_m3 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s3 ),1,mean,na.rm=T))
    d3$EXP_m4 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s4 ),1,mean,na.rm=T))
    d3$EXP_m5 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s5 ),1,mean,na.rm=T))
    d3$EXP_m6 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s6 ),1,mean,na.rm=T))
    d3$EXP_m7 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s7 ),1,mean,na.rm=T))
    d3$EXP_m8 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s8 ),1,mean,na.rm=T))
    d3$EXP_m9 =with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s9 ),1,mean,na.rm=T))
    d3$EXP_m10=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s10),1,mean,na.rm=T))
    d3$EXP_m11=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s11),1,mean,na.rm=T))
    d3$EXP_m12=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s12),1,mean,na.rm=T))
    d3$EXP_m13=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s13),1,mean,na.rm=T))
    d3$EXP_m14=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s14),1,mean,na.rm=T))
    d3$EXP_m15=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s15),1,mean,na.rm=T))
    d3$EXP_m16=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s16),1,mean,na.rm=T))
    d3$EXP_m17=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s17),1,mean,na.rm=T))
    d3$EXP_m18=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s18),1,mean,na.rm=T))
    d3$EXP_m19=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s19),1,mean,na.rm=T))
    d3$EXP_m20=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s20),1,mean,na.rm=T))
    d3$EXP_m21=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s21),1,mean,na.rm=T))
    d3$EXP_m22=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s22),1,mean,na.rm=T))
    d3$EXP_m23=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s23),1,mean,na.rm=T))
    d3$EXP_m24=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s24),1,mean,na.rm=T))
    d3$EXP_m25=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s25),1,mean,na.rm=T))
    d3$EXP_m26=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s26),1,mean,na.rm=T))
    d3$EXP_m27=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s27),1,mean,na.rm=T))
    d3$EXP_m28=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s28),1,mean,na.rm=T))
    d3$EXP_m29=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s29),1,mean,na.rm=T))
    d3$EXP_m30=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s30),1,mean,na.rm=T))
    d3$EXP_m31=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s31),1,mean,na.rm=T))
    d3$EXP_m32=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s32),1,mean,na.rm=T))
    d3$EXP_m33=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s33),1,mean,na.rm=T))
    d3$EXP_m34=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s34),1,mean,na.rm=T))
    d3$EXP_m35=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s35),1,mean,na.rm=T))
    d3$EXP_m36=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s36),1,mean,na.rm=T))
    d3$EXP_m37=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s37),1,mean,na.rm=T))
    d3$EXP_m38=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s38),1,mean,na.rm=T))
    d3$EXP_m39=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s39),1,mean,na.rm=T))
    d3$EXP_m40=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s40),1,mean,na.rm=T))
    d3$EXP_m41=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s41),1,mean,na.rm=T))
    d3$EXP_m42=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s42),1,mean,na.rm=T))
    d3$EXP_m43=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s43),1,mean,na.rm=T))
    d3$EXP_m44=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s44),1,mean,na.rm=T))
    d3$EXP_m45=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s45),1,mean,na.rm=T))
    d3$EXP_m46=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s46),1,mean,na.rm=T))
    d3$EXP_m47=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s47),1,mean,na.rm=T))
    d3$EXP_m48=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s48),1,mean,na.rm=T))
    d3$EXP_m49=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s49),1,mean,na.rm=T))
    d3$EXP_m50=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s50),1,mean,na.rm=T))
    d3$EXP_m51=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s51),1,mean,na.rm=T))
    d3$EXP_m52=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s52),1,mean,na.rm=T))
    d3$EXP_m53=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s53),1,mean,na.rm=T))
    d3$EXP_m54=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s54),1,mean,na.rm=T))
    d3$EXP_m55=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s55),1,mean,na.rm=T))
    d3$EXP_m56=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s56),1,mean,na.rm=T))
    d3$EXP_m57=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s57),1,mean,na.rm=T))
    d3$EXP_m58=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s58),1,mean,na.rm=T))
    d3$EXP_m59=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s59),1,mean,na.rm=T))
    d3$EXP_m60=with(d3,apply(d3 %>% select(EXP,EXP_s1:EXP_s60),1,mean,na.rm=T))
    
    file.list[[i]]<-d3;print(i)}
  do.call(rbind,file.list)}
lag_evi1 <-evi_ndvi_func(evi1)
lag_evi2 <-evi_ndvi_func(evi2)
lag_ndvi1<-evi_ndvi_func(ndvi1)
lag_ndvi2<-evi_ndvi_func(ndvi2)

names(lag_evi1) =gsub("EXP","EVI" ,names(lag_evi1))
names(lag_evi2) =gsub("EXP","EVI" ,names(lag_evi2))
names(lag_ndvi1)=gsub("EXP","NDVI",names(lag_ndvi1))
names(lag_ndvi2)=gsub("EXP","NDVI",names(lag_ndvi2))

write.csv(lag_evi1 ,file="evi_nomask_lag_rev.csv"     ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(lag_evi2 ,file="evi_summaryeq0_lag_rev.csv" ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(lag_ndvi1,file="ndvi_nomask_lag_rev.csv"    ,row.names=F,na="",fileEncoding = "euc-kr")
write.csv(lag_ndvi2,file="ndvi_summaryeq0_lag_rev.csv",row.names=F,na="",fileEncoding = "euc-kr")

#-------------------------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------------------------#
warm_evi1<-lag_evi1 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:8)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_EVI=mean(EVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()

warm_evi2<-lag_evi2 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:8)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_EVI=mean(EVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()
warm_ndvi1<-lag_ndvi1 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:8)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_NDVI=mean(NDVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()
warm_ndvi2<-lag_ndvi2 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:8)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_NDVI=mean(NDVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()

filelist1=NULL;filelist2=NULL;filelist3=NULL;filelist4=NULL

for(i in 1:length(sgg)){
    
  sgg<-unique(data$SIGUNGU_CD)
  d1<-subset(warm_evi1,SIGUNGU_CD==sgg[i])
  d2<-subset(warm_evi2,SIGUNGU_CD==sgg[i])
  d3<-subset(warm_ndvi1,SIGUNGU_CD==sgg[i])
  d4<-subset(warm_ndvi2,SIGUNGU_CD==sgg[i])
  
  d1$warm_EVI_s1 =lag(d1$warm_EVI,1)
  d1$warm_EVI_s2 =lag(d1$warm_EVI,2)
  d1$warm_EVI_s3 =lag(d1$warm_EVI,3)
  d1$warm_EVI_s4 =lag(d1$warm_EVI,4)
  d1$warm_EVI_s5 =lag(d1$warm_EVI,5)

  d1$warm_EVI_m1 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1),1,mean,na.rm=T)
  d1$warm_EVI_m2 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s2),1,mean,na.rm=T)
  d1$warm_EVI_m3 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s3),1,mean,na.rm=T)
  d1$warm_EVI_m4 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s4),1,mean,na.rm=T)
  d1$warm_EVI_m5 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s5),1,mean,na.rm=T)
  
  d2$warm_EVI_s1 =lag(d2$warm_EVI,1)
  d2$warm_EVI_s2 =lag(d2$warm_EVI,2)
  d2$warm_EVI_s3 =lag(d2$warm_EVI,3)
  d2$warm_EVI_s4 =lag(d2$warm_EVI,4)
  d2$warm_EVI_s5 =lag(d2$warm_EVI,5)
  
  d2$warm_EVI_m1 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1),1,mean,na.rm=T)
  d2$warm_EVI_m2 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s2),1,mean,na.rm=T)
  d2$warm_EVI_m3 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s3),1,mean,na.rm=T)
  d2$warm_EVI_m4 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s4),1,mean,na.rm=T)
  d2$warm_EVI_m5 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s5),1,mean,na.rm=T)
  
  d3$warm_NDVI_s1 =lag(d3$warm_NDVI,1)
  d3$warm_NDVI_s2 =lag(d3$warm_NDVI,2)
  d3$warm_NDVI_s3 =lag(d3$warm_NDVI,3)
  d3$warm_NDVI_s4 =lag(d3$warm_NDVI,4)
  d3$warm_NDVI_s5 =lag(d3$warm_NDVI,5)
  
  d3$warm_NDVI_m1 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1),1,mean,na.rm=T)
  d3$warm_NDVI_m2 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s2),1,mean,na.rm=T)
  d3$warm_NDVI_m3 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s3),1,mean,na.rm=T)
  d3$warm_NDVI_m4 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s4),1,mean,na.rm=T)
  d3$warm_NDVI_m5 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s5),1,mean,na.rm=T)
  
  d4$warm_NDVI_s1 =lag(d4$warm_NDVI,1)
  d4$warm_NDVI_s2 =lag(d4$warm_NDVI,2)
  d4$warm_NDVI_s3 =lag(d4$warm_NDVI,3)
  d4$warm_NDVI_s4 =lag(d4$warm_NDVI,4)
  d4$warm_NDVI_s5 =lag(d4$warm_NDVI,5)
  
  d4$warm_NDVI_m1 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1),1,mean,na.rm=T)
  d4$warm_NDVI_m2 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s2),1,mean,na.rm=T)
  d4$warm_NDVI_m3 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s3),1,mean,na.rm=T)
  d4$warm_NDVI_m4 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s4),1,mean,na.rm=T)
  d4$warm_NDVI_m5 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s5),1,mean,na.rm=T)
  
  filelist1[[i]]<-d1
  filelist2[[i]]<-d2
  filelist3[[i]]<-d3
  filelist4[[i]]<-d4
  print(i)
}

warm5_8_EVI1 <-do.call(rbind,filelist1)
warm5_8_EVI2 <-do.call(rbind,filelist2)
warm5_8_NDVI1<-do.call(rbind,filelist3)
warm5_8_NDVI2<-do.call(rbind,filelist4)

write.csv(warm5_8_EVI1,file="warm5_8_EVI1.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(warm5_8_EVI1,file="warm5_8_EVI2.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(warm5_8_NDVI1,file="warm5_8_NDVI1.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(warm5_8_NDVI2,file="warm5_8_NDVI2.csv",row.names=F,na="",fileEncoding = "euc-kr")



warm_evi1<-lag_evi1 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:9)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_EVI=mean(EVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()

warm_evi2<-lag_evi2 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:9)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_EVI=mean(EVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()
warm_ndvi1<-lag_ndvi1 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:9)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_NDVI=mean(NDVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()
warm_ndvi2<-lag_ndvi2 %>% dplyr::select(year:SIGUNGU_NM) %>% filter(month %in% c(5:9)) %>% 
  group_by(year,SIGUNGU_CD) %>% dplyr::summarise(warm_NDVI=mean(NDVI,na.rm=T)) %>% 
  arrange(SIGUNGU_CD,year) %>% ungroup()

filelist1=NULL;filelist2=NULL;filelist3=NULL;filelist4=NULL

for(i in 1:length(sgg)){
  
  sgg<-unique(data$SIGUNGU_CD)
  d1<-subset(warm_evi1,SIGUNGU_CD==sgg[i])
  d2<-subset(warm_evi2,SIGUNGU_CD==sgg[i])
  d3<-subset(warm_ndvi1,SIGUNGU_CD==sgg[i])
  d4<-subset(warm_ndvi2,SIGUNGU_CD==sgg[i])
  
  d1$warm_EVI_s1 =lag(d1$warm_EVI,1)
  d1$warm_EVI_s2 =lag(d1$warm_EVI,2)
  d1$warm_EVI_s3 =lag(d1$warm_EVI,3)
  d1$warm_EVI_s4 =lag(d1$warm_EVI,4)
  d1$warm_EVI_s5 =lag(d1$warm_EVI,5)
  
  d1$warm_EVI_m1 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1),1,mean,na.rm=T)
  d1$warm_EVI_m2 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s2),1,mean,na.rm=T)
  d1$warm_EVI_m3 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s3),1,mean,na.rm=T)
  d1$warm_EVI_m4 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s4),1,mean,na.rm=T)
  d1$warm_EVI_m5 =apply(d1 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s5),1,mean,na.rm=T)
  
  d2$warm_EVI_s1 =lag(d2$warm_EVI,1)
  d2$warm_EVI_s2 =lag(d2$warm_EVI,2)
  d2$warm_EVI_s3 =lag(d2$warm_EVI,3)
  d2$warm_EVI_s4 =lag(d2$warm_EVI,4)
  d2$warm_EVI_s5 =lag(d2$warm_EVI,5)
  
  d2$warm_EVI_m1 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1),1,mean,na.rm=T)
  d2$warm_EVI_m2 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s2),1,mean,na.rm=T)
  d2$warm_EVI_m3 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s3),1,mean,na.rm=T)
  d2$warm_EVI_m4 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s4),1,mean,na.rm=T)
  d2$warm_EVI_m5 =apply(d2 %>% dplyr::select(warm_EVI,warm_EVI_s1:warm_EVI_s5),1,mean,na.rm=T)
  
  d3$warm_NDVI_s1 =lag(d3$warm_NDVI,1)
  d3$warm_NDVI_s2 =lag(d3$warm_NDVI,2)
  d3$warm_NDVI_s3 =lag(d3$warm_NDVI,3)
  d3$warm_NDVI_s4 =lag(d3$warm_NDVI,4)
  d3$warm_NDVI_s5 =lag(d3$warm_NDVI,5)
  
  d3$warm_NDVI_m1 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1),1,mean,na.rm=T)
  d3$warm_NDVI_m2 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s2),1,mean,na.rm=T)
  d3$warm_NDVI_m3 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s3),1,mean,na.rm=T)
  d3$warm_NDVI_m4 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s4),1,mean,na.rm=T)
  d3$warm_NDVI_m5 =apply(d3 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s5),1,mean,na.rm=T)
  
  d4$warm_NDVI_s1 =lag(d4$warm_NDVI,1)
  d4$warm_NDVI_s2 =lag(d4$warm_NDVI,2)
  d4$warm_NDVI_s3 =lag(d4$warm_NDVI,3)
  d4$warm_NDVI_s4 =lag(d4$warm_NDVI,4)
  d4$warm_NDVI_s5 =lag(d4$warm_NDVI,5)
  
  d4$warm_NDVI_m1 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1),1,mean,na.rm=T)
  d4$warm_NDVI_m2 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s2),1,mean,na.rm=T)
  d4$warm_NDVI_m3 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s3),1,mean,na.rm=T)
  d4$warm_NDVI_m4 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s4),1,mean,na.rm=T)
  d4$warm_NDVI_m5 =apply(d4 %>% dplyr::select(warm_NDVI,warm_NDVI_s1:warm_NDVI_s5),1,mean,na.rm=T)
  
  filelist1[[i]]<-d1
  filelist2[[i]]<-d2
  filelist3[[i]]<-d3
  filelist4[[i]]<-d4
  print(i)
}

warm5_9_EVI1 <-do.call(rbind,filelist1)
warm5_9_EVI2 <-do.call(rbind,filelist2)
warm5_9_NDVI1<-do.call(rbind,filelist3)
warm5_9_NDVI2<-do.call(rbind,filelist4)

write.csv(warm5_9_EVI1,file="warm5_9_EVI1.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(warm5_9_EVI1,file="warm5_9_EVI2.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(warm5_9_NDVI1,file="warm5_9_NDVI1.csv",row.names=F,na="",fileEncoding = "euc-kr")
write.csv(warm5_9_NDVI2,file="warm5_9_NDVI2.csv",row.names=F,na="",fileEncoding = "euc-kr")
