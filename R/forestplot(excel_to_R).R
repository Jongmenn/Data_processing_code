# install.packages("forestplot")
library(forestplot)
library(readxl)
#자료불러오기 
d1<-read_excel("D:\\Table2_210728forFigure.xlsx",sheet=1) #Sheet별로 변경 

#엑셀 자료 (테이블 형태 정리자료)에서 카테고리 아래 (서브 카테고리) 한칸씩 띄어쓸 부분 지정 
subgps <- c(2,3,6:12,15:20,23:28,31:36)
d1$value[subgps] <- paste("  ",d1$value[subgps]) 

#그림에 표현할 텍스트
tabletext <- cbind(c("Participants","\n",d1$value), 
                   c("Event","\n",d1$Eventsv), 
                   c("HR","\n",d1$HRv))


#forestplot() 내에 옵션 fpColors은 열로 묶인 자료에 대해서(그룹별 자료) 일괄적으로 색깔지정
#유의한 그룹/ 유의하지 않은 그룹으로 지정해서 열로 묶기
est<-as.data.frame(cbind(hr=c(NA,NA,d1$HRr),lci=c(NA,NA,d1$LCLr),uci=c(NA,NA,d1$UCLr)))
signi<-ifelse(est$lci>1,1,0)
signi[is.na(signi)]<-0
est$signi=signi

est1<-est;est2<-est

est1$hr =with(est1,ifelse(signi==0,NA,hr))
est1$lci=with(est1,ifelse(signi==0,NA,lci))
est1$uci=with(est1,ifelse(signi==0,NA,uci))

est2$hr =with(est2,ifelse(signi==1 ,NA,hr))
est2$lci=with(est2,ifelse(signi==1,NA,lci))
est2$uci=with(est2,ifelse(signi==1,NA,uci))


x11();forestplot(labeltext=tabletext, graph.pos=4, 
                 mean =cbind(est1$hr,est2$hr), 
                 lower=cbind(est1$lci,est2$lci),
                 upper=cbind(est1$uci,est2$uci),
                 title="",
                 xlab="Hazard Ratio  (95% Confidence intervals)",
                 txt_gp=fpTxtGp(label=gpar(cex=1.25),
                                ticks=gpar(cex=1.1),
                                xlab=gpar(cex = 1.2),
                                title=gpar(cex = 1.2)),
                 col=fpColors(box=c("red","black"), lines=c("red","black"), zero = "gray50"),
                 xticks=c(1:as.integer(max(d1$UCLr,na.rm=T)+2)-1),
                 zero=1, cex=0.9, lineheight = "auto", boxsize=0.3, colgap=unit(5,"mm"),
                 lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2,)


#Save the current figures
tiff(filename="D:\\forestplot.tiff",width=3600,height=4200,res=300)
forestplot(labeltext=tabletext, graph.pos=4, 
           mean =cbind(est1$hr,est2$hr), 
           lower=cbind(est1$lci,est2$lci),
           upper=cbind(est1$uci,est2$uci),
           title="",
           xlab="Hazard Ratio  (95% Confidence intervals)",
           txt_gp=fpTxtGp(label=gpar(cex=1.25),
                          ticks=gpar(cex=1.1),
                          xlab=gpar(cex = 1.2),
                          title=gpar(cex = 1.2)),
           col=fpColors(box=c("red","black"), lines=c("red","black"), zero = "gray50"),
           xticks=c(1:as.integer(max(d1$UCLr,na.rm=T)+2)-1),
           zero=1, cex=0.9, lineheight = "auto", boxsize=0.3, colgap=unit(5,"mm"),
           lwd.ci=2, ci.vertices=TRUE, ci.vertices.height = 0.2,)

dev.off()