setwd("D:\\국건영\\data")

# install.packages("haven")
# install.packages("survey")
library(dplyr)
library(haven)
library(survey)

hn15_all<-read_sas("hn15_all.sas7bdat")
head(hn15_all)

#기본 분석 형태
table(hn15_all$HE_HP)
addmargins(table(hn15_all$HE_HP))

#복합표본설계
#예) 분석에 이용할 변수만 추출
z <-hn15_all %>% select(id,psu,kstrata,wt_itvex,HE_HP)
z2<-z[complete.cases(z),] #결측 제거

head(z2);nrow(z2)
com.svy<-svydesign(ids    =~psu,     #집락
                   strata =~kstrata, #층
                   weights=~wt_itvex,#가중치
                   data   =z2)       #데이터

round(rbind(svytable(~HE_HP,design=com.svy),
            svytable(~HE_HP,design=com.svy,Ntotal=T)),2)


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
hn13_all<-read_sas("hn13_all.sas7bdat")
hn14_all<-read_sas("hn14_all.sas7bdat")
hn15_all<-read_sas("hn15_all.sas7bdat")
hn16_all<-read_sas("hn16_all.sas7bdat")
hn17_all<-read_sas("hn17_all.sas7bdat")
hn18_all<-read_sas("hn18_all.sas7bdat")
hn19_all<-read_sas("hn19_all.sas7bdat")
hn20_all<-read_sas("hn20_all.sas7bdat")
hn21_all<-read_sas("hn21_all.sas7bdat")
hn22_all<-read_sas("hn22_all.sas7bdat")

hn<-rbind(hn13_all %>% select(year),
      hn14_all %>% select(year),
      hn15_all %>% select(year),
      hn16_all %>% select(year),
      hn17_all %>% select(year),
      hn18_all %>% select(year),
      hn19_all %>% select(year),
      hn20_all %>% select(year),
      hn21_all %>% select(year),
      hn22_all %>% select(year))

table(hn$year)


length(table(hn13_all$psu))
length(table(hn14_all$psu))
length(table(hn15_all$psu))
length(table(hn16_all$psu))
length(table(hn17_all$psu))
length(table(hn18_all$psu))
length(table(hn19_all$psu))
length(table(hn20_all$psu))
length(table(hn21_all$psu))
length(table(hn22_all$psu))


#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#기수내 통합 가중치
#예시: 성별
ex13 <-hn13_all %>% select(id,psu,kstrata,wt_itvex,sex,year);nrow(ex13)
ex14 <-hn14_all %>% select(id,psu,kstrata,wt_itvex,sex,year);nrow(ex14)
ex15 <-hn15_all %>% select(id,psu,kstrata,wt_itvex,sex,year);nrow(ex15)

ex13_rev<-ex13[complete.cases(ex13),];nrow(ex13_rev)
ex14_rev<-ex14[complete.cases(ex14),];nrow(ex14_rev)
ex15_rev<-ex15[complete.cases(ex15),];nrow(ex15_rev)

ex_1315<-rbind(ex13_rev,
               ex14_rev,
               ex15_rev)

com.svy13<-svydesign(ids    =~psu,     #집락
                     strata =~kstrata, #층
                     weights=~wt_itvex,#가중치
                     data   =ex13_rev) #데이터
    
com.svy14<-svydesign(ids    =~psu,     #집락
                     strata =~kstrata, #층
                     weights=~wt_itvex,#가중치
                     data   =ex14_rev) #데이터

com.svy15<-svydesign(ids    =~psu,     #집락
                     strata =~kstrata, #층
                     weights=~wt_itvex,#가중치
                     data   =ex15_rev) #데이터

#통합 가중치를 고려하지 않은 경우
com.svy1315<-svydesign(ids    =~psu,      #집락
                       strata =~kstrata,  #층
                       weights=~wt_itvex, #가중치
                       data   =ex_1315)#데이터

svytotal(~sex==2,design=com.svy13)
svytotal(~sex==2,design=com.svy14)
svytotal(~sex==2,design=com.svy15)
svytotal(~sex==2,design=com.svy1315)

round(rbind(svytable(~sex,design=com.svy13),
            svytable(~sex,design=com.svy13,Ntotal=T)),2)
round(rbind(svytable(~sex,design=com.svy14),
            svytable(~sex,design=com.svy14,Ntotal=T)),2)
round(rbind(svytable(~sex,design=com.svy15),
            svytable(~sex,design=com.svy15,Ntotal=T)),2)

round(rbind(svytable(~sex,design=com.svy1315),
            svytable(~sex,design=com.svy1315,Ntotal=T)),2)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#통합 가중치를 고려한 경우
ex_1315$wt_itvex2=ex_1315$wt_itvex/3
com.svy1315_rev<-svydesign(ids    =~psu,      #집락
                           strata =~kstrata,  #층
                           weights=~wt_itvex2, #가중치
                           data   =ex_1315)#데이터

svytotal(~sex==2,design=com.svy1315_rev)

svytable(~sex+year,design=com.svy1315)
svytable(~sex+year,design=com.svy1315_rev)


#2013~2020년 가중치 
ex13 <-hn13_all %>% select(id,psu,kstrata,wt_itvex,sex,year);nrow(ex13)
ex14 <-hn14_all %>% select(id,psu,kstrata,wt_itvex,sex,year);nrow(ex14)
ex15 <-hn15_all %>% select(id,psu,kstrata,wt_itvex,sex,year);nrow(ex15)
ex16 <-hn16_all %>% select(ID,psu,kstrata,wt_itvex,sex,year);nrow(ex16)
ex17 <-hn17_all %>% select(ID,psu,kstrata,wt_itvex,sex,year);nrow(ex17)
ex18 <-hn18_all %>% select(ID,psu,kstrata,wt_itvex,sex,year);nrow(ex18)
ex19 <-hn19_all %>% select(ID,psu,kstrata,wt_itvex,sex,year);nrow(ex19)
ex20 <-hn20_all %>% select(ID,psu,kstrata,wt_itvex,sex,year);nrow(ex20)

names(ex16)[1]="id"
names(ex17)[1]="id"
names(ex18)[1]="id"
names(ex19)[1]="id"
names(ex20)[1]="id"

ex_1320    <-rbind(ex13,ex14,ex15,ex16,ex17,ex18,ex19,ex20)
ex_1320_rev<-ex_1320[complete.cases(ex_1320),]

ex_1320_rev$wt_itvex2<-ex_1320_rev$wt_itvex/8
com.svy1320<-svydesign(ids    =~psu,       #집락
                       strata =~kstrata,   #층
                       weights=~wt_itvex2, #가중치
                       data   =ex_1320_rev)#데이터
com.svy1320
svytable(~sex+year,design=com.svy1320)

#------------------------------------------------------------------------------#
#------------------------------------------------------------------------------#
#실내공기질 조사자료
hn_iaq<-read_sas("HN_IAQ.sas7bdat")
names(hn_iaq)
length(na.omit(hn_iaq$IAQ_PM2_5))
addmargins(table(hn_iaq$DI3_pr))
dat<-hn_iaq %>% select(ID,kstrata,psu,
                  wt_iaq_hs,                      #실내 공기질 가중치
                  region,sex,age,incm,edu,occp,  #인구학적 특징
                  IAQ_Window:IAQ_PM2_5_atmos,     #가정 실내 공기질 조사 자료
                  DI3_pr                          #뇌졸중
                  ) %>% mutate(Stroke=ifelse(DI3_pr==1,1,
                                             ifelse(DI3_pr==0 | DI3_pr==8,0,9))) %>% 
  filter(Stroke %in% c(0,1) )
str(dat$Stroke)
dat$Stroke=as.integer(dat$Stroke)
table(dat$Stroke)
#DI3_pr: 0 없음,1 있음, 8 비해당, 9 모름
#복합표본 설계 고려
nrow(dat)
com.svy_iaq<-svydesign(ids    =~psu,       #집락
                       strata =~kstrata,   #층
                       weights=~wt_iaq_hs, #가중치
                       data   =dat)        #데이터
com.svy_iaq
svytable(~sex,design=com.svy_iaq)

#선형 회귀분석시; 
svyglm(Y~X,
       data  =dat,
       design=com.svy_iaq,family="gaussian")


#로지스틱회귀분석시;
fit<-svyglm(Stroke~age,
       data  =dat,
       design=com.svy_iaq,family="quasibinomial")
fit
summary(fit)

fit2<-svyglm(Stroke~IAQ_PM2_5,
            data  =dat,
            design=com.svy_iaq,family="quasibinomial")
fit2
summary(fit2)
names(dat)
fit3<-svyglm(Stroke~IAQ_PM2_5+age+
              factor(sex)+
              factor(region)+
              factor(incm)+
              factor(edu)+
              factor(occp),
            data  =dat,
            design=com.svy_iaq,family="quasibinomial")

summary(fit3)
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#매개분석 
library(mediation)
library(survey)

m1<-svyglm(mediator~X+Covariates,
           data  =원자료,
           family="gaussian"   #quasibinomial, etc
           design=복합표본설계)

m2<-svyglm(Y~X+Covariates,
           family="gaussian"   #quasibinomial, etc
           design=복합표본설계)

summary(m1)
summary(m2)

fit_med<-mediate(model.m=m1,model.y=m2,sims=1000,treat="X",mediator = "mediator")
summary(fit_med)
#-------------------------------------------------------------------------------#
#-------------------------------------------------------------------------------#
#대기오염/기상자료 연계
#2013~2020년 가중치 

ex13 <-hn13_all %>% dplyr:: select(id,psu,kstrata,wt_itvex,sex,year,region);nrow(ex13)
ex14 <-hn14_all %>% dplyr:: select(id,psu,kstrata,wt_itvex,sex,year,region);nrow(ex14)
ex15 <-hn15_all %>% dplyr:: select(id,psu,kstrata,wt_itvex,sex,year,region);nrow(ex15)
ex16 <-hn16_all %>% dplyr:: select(ID,psu,kstrata,wt_itvex,sex,year,region);nrow(ex16)
ex17 <-hn17_all %>% dplyr:: select(ID,psu,kstrata,wt_itvex,sex,year,region);nrow(ex17)
ex18 <-hn18_all %>% dplyr:: select(ID,psu,kstrata,wt_itvex,sex,year,region);nrow(ex18)
ex19 <-hn19_all %>% dplyr:: select(ID,psu,kstrata,wt_itvex,sex,year,region);nrow(ex19)
ex20 <-hn20_all %>% dplyr:: select(ID,psu,kstrata,wt_itvex,sex,year,region);nrow(ex20)

names(ex16)[1]="id"
names(ex17)[1]="id"
names(ex18)[1]="id"
names(ex19)[1]="id"
names(ex20)[1]="id"

ex_1320    <-rbind(ex13,ex14,ex15,ex16,ex17,ex18,ex19,ex20)
ex_1320_rev<-ex_1320[complete.cases(ex_1320),]

head(ex_1320_rev)

ex3<-ex_1320_rev

#지역별 노출 연계하기위해 코드변경 
ex3$region=with(ex3,ifelse(region==1 ,"서울",region))
ex3$region=with(ex3,ifelse(region==2 ,"부산",region))
ex3$region=with(ex3,ifelse(region==3 ,"대구",region))
ex3$region=with(ex3,ifelse(region==4 ,"인천",region))
ex3$region=with(ex3,ifelse(region==5 ,"광주",region))
ex3$region=with(ex3,ifelse(region==6 ,"대전",region))
ex3$region=with(ex3,ifelse(region==7 ,"울산",region))
ex3$region=with(ex3,ifelse(region==8 ,"세종",region))
ex3$region=with(ex3,ifelse(region==9 ,"경기",region))
ex3$region=with(ex3,ifelse(region==10,"강원",region))
ex3$region=with(ex3,ifelse(region==11,"충북",region))
ex3$region=with(ex3,ifelse(region==12,"충남",region))
ex3$region=with(ex3,ifelse(region==13,"전북",region))
ex3$region=with(ex3,ifelse(region==14,"전남",region))
ex3$region=with(ex3,ifelse(region==15,"경북",region))
ex3$region=with(ex3,ifelse(region==16,"경남",region))
ex3$region=with(ex3,ifelse(region==17,"제주",region)) 

ex3$key=with(ex3,paste0(year,"-",region))

head(ex3)

ap_temp<-read_excel("기상자료_대기오염_업데이트_일별_2001_2020_OJM_new2.xlsx",sheet=2,
                    guess_max=100000)

names(ap_temp)
head(ap_temp)
ap_temp_yr<-ap_temp %>% group_by(year,area) %>% summarise(meanT=mean(meantemp,na.rm=T),
                                              PM10=mean(pm10,na.rm=T),
                                              SO2 =mean(so2,na.rm=T),
                                              NO2 =mean(no2,na.rm=T),
                                              CO  =mean(co,na.rm=T),
                                              O3  =mean(o3,na.rm=T)) %>% 
  filter(year>=2010) %>% mutate(key=paste0(year,"-",area)) %>% ungroup %>% 
  dplyr::select(key,meanT:O3) 

head(ex3)
ap_temp_yr

ex3 %>% left_join(ap_temp_yr,by="key")
