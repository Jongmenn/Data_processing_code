setwd("C:\\Users\\kard1\\OneDrive\\문서\\카카오톡 받은 파일")
dat <- read.fwf("krig.county.hourly.RTS_IQ02_Krig.K09.CO.2006.05.txt", 
                col.names = c("hourtime",paste0(0:249)),
                widths=c(10,rep(6,250)))
dat_rev<-dat[-1,]

head(dat_rev)
