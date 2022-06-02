library(readxl)      ;library(tm)
library(stringr)     ;library(dplyr)
library(wordcloud)   ;library(RColorBrewer)
library(ggplot2)     ;library(tidyr)
library(tidytext)    ;library(RcppMeCab)

setwd("D:\\")
dat<-read_excel("의과대학_전임교원_특허실적(2019-2021).xlsx",sheet=1)

#날짜 요약치
summary(dat$출원일)
summary(dat$등록일)

#연도별 빈도 
addmargins(table(substr(dat$출원일,1,4)))
addmargins(table(substr(dat$등록일,1,4)))

#필요한 변수만 추출 
dat.r<-dat %>% select("발명의 명칭","대표발명자","발명자 중 의과대학 전임교원")
names(dat.r)=c("V1","V2","V3")

#중복값 있는지?  / 특허명은 중복 존재 
unique(dat$출원번호) %>% length
unique(dat$`본교 관리번호`) %>% length

#특허명 중복되는 경우 제외하고 보기 
dat.r2<-dat.r[!duplicated(dat.r$V1),]

#특허명 중복되는 경우 제목 
dup_title<-unique(dat.r[duplicated(dat.r$V1),]$V1)
data.frame(dup_title) %>% View

#원자료에서 특허명 중복되는 경우 살펴보기 
dat %>% filter(`발명의 명칭` %in% dup_title ) %>% arrange(`발명의 명칭`) %>% View

zz<-as.data.frame(dup_title)
# write.csv(zz,file="zz.csv",row.names=F,na="",fileEncoding = "euc-kr")
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#발명의 명칭
#한글 전처리 MeCab package 이용
#명사만 추출해서 이용하거나, 제외하고 싶은 부분 제외하고 보기 
#참고: https://bookdown.org/ahn_media/bookdown-demo/clean.html

#한글 품사 태깅
df_v1  <-posParallel(dat.r$V1,format="data.frame")
df_v1.r<-posParallel(dat.r2$V1,format="data.frame")

#불필요한 한글 품사 제거 
df_v1.r<-df_v1 %>% filter(!pos %in% c("JKO","MAJ","XSN","XSA+ETM","VCP+EC","XSV+ETM",
                     "XSV","JKG","ETN","ETM","JX","NNB","JKB","NNP","JC","XPN","XSA","EC",
                     "SF","SE","SSO","SSC","SC","SY","SH","VCP","VCN","JKS","NP","VV+ETM",
                     "VV+EC","EC+VX+ETM","VV+ETN","VX+ETM","NP+JKG","IC","MAG","NR","SN") ) 

df_v1.r2<-df_v1.r %>% filter(!pos %in% c("JKO","MAJ","XSN","XSA+ETM","VCP+EC","XSV+ETM",
                                      "XSV","JKG","ETN","ETM","JX","NNB","JKB","NNP","JC","XPN","XSA","EC",
                                      "SF","SE","SSO","SSC","SC","SY","SH","VCP","VCN","JKS","NP","VV+ETM",
                                      "VV+EC","EC+VX+ETM","VV+ETN","VX+ETM","NP+JKG","IC","MAG","NR","SN") ) 


#단어 빈도만 볼거니까, 전체 카운트 
cnt_df <-df_v1.r  %>% group_by(token) %>% summarise(cnt=table(token)) %>% arrange(-cnt)
cnt_df2<-df_v1.r2 %>% group_by(token) %>% summarise(cnt=table(token)) %>% arrange(-cnt)

#발명의 명칭- 워드클라우드 
x11();wordcloud(cnt_df$token,freq=cnt_df$cnt,min.freq=5,scale=c(8,1),colors=pal,random.order=F, font =2)
x11();wordcloud(cnt_df2$token,freq=cnt_df2$cnt,min.freq=5,scale=c(8,1),colors=pal,random.order=F, font =2)


#한글 인코딩 깨질시 전환 
#iconv(dat.r$V1, from = "UTF-8", to = "CP949")
#iconv(dat.r$V1[1], from = "CP949", to = "UTF-8")
#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#대표 발명자 빈도, 마찬가지로 원시자료/중복제거한 자료 둘다보기 
df_v2  <-as.data.frame(table(dat.r$V2))
df_v2.r<-as.data.frame(table(dat.r2$V2))

names(df_v2)  =c("name","Freq")
names(df_v2.r)=c("name","Freq")

df_v2  <-df_v2   %>% arrange(-Freq)
df_v2.r<-df_v2.r %>% arrange(-Freq)

#Barplot
x11();ggplot(df_v2,aes(reorder(name,Freq),Freq))+geom_bar(stat="identity")+coord_flip()+
  theme_gray(base_size=25)+labs(x="빈도",y="이름")

x11();ggplot(df_v2.r,aes(reorder(name,Freq),Freq))+geom_bar(stat="identity")+coord_flip()+
  theme_gray(base_size=25)+labs(x="빈도",y="이름")

#대표 발명자 별 -워드클라우드 
pal<-brewer.pal(9,"Set1")
x11();wordcloud(df_v2$name,freq=df_v2$Freq,scale=c(6,1),colors=pal,random.order=F, font =2)
x11();wordcloud(df_v2.r$name,freq=df_v2.r$Freq,scale=c(6,1),colors=pal,random.order=F, font =2)

#--------------------------------------------------------------------------------------#
#--------------------------------------------------------------------------------------#
#발명자 중 의과대학 전임교원 
#한글 이름(교수님 성함) 잘 입력되어있고, "," 구분자로 입력되어있으니 "," 기준으로 분할 
v3_list  <-str_split(dat.r$V3,",")
v3_list.r<-str_split(dat.r2$V3,",")

v3_list_c =NULL;v3_list_c2=NULL

#특허별로 교수님 여러분이시니 특허별, 발명자 중 의과대학 전임교원 data set long format
for(i in 1:length(v3_list)){
  v3_list_c[[i]]<-length(v3_list[[i]])
  print(i)}

for(i in 1:length(v3_list.r)){
  
  v3_list_c2[[i]]<-length(v3_list.r[[i]])
  print(i)}

k =do.call(c,v3_list_c)
k2=do.call(c,v3_list_c2)

rep_n =NULL
rep_n2=NULL

for(i in 1: length(k)){
  rep_n[[i]]<-rep(i,each=k[i])
  print(i)}

for(i in 1: length(k2)){
  rep_n2[[i]]<-rep(i,each=k2[i])
  print(i)}

df_v3  <-data.frame(do.call(c,rep_n),unlist(v3_list))
df_v3.r<-data.frame(do.call(c,rep_n2),unlist(v3_list.r))

names(df_v3)  =c("pat_id","name")
names(df_v3.r)=c("pat_id","name")

#이름별로 카운트 
v3_tb1<-df_v3   %>% group_by(name) %>% summarise(cnt=table(name)) %>% arrange(-cnt)
v3_tb2<-df_v3.r %>% group_by(name) %>% summarise(cnt=table(name)) %>% arrange(-cnt)

#발명자 중 의과대학 전임교원 워드클라우드 
x11();with(v3_tb1,wordcloud(name,freq=cnt,scale=c(6,1),colors=pal,random.order=F, font =2))
x11();with(v3_tb2,wordcloud(name,freq=cnt,scale=c(6,1),colors=pal,random.order=F, font =2))
#--------------------------------------------------------------------------------------#