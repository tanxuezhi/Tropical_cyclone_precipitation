setwd('D:/Data/JTWCBST/all83-19')

RI <- data.frame(year_TC<-0,month_TC<-0,day_TC<-0,hour_TC<-0,lat_TC<-0,lon_TC<-0,
                 v_TC<-0,area<-"N",rank_lat_TC<-0,rank_lon_TC<-0)
RI <- RI[-1,]
colnames(RI) <- c("year_TC","month_TC","day_TC","hour_TC","lat_TC",
                  "lon_TC","v_TC","area","rank_lat_TC","rank_lon_TC")
NoRI <- data.frame(year_TC<-0,month_TC<-0,day_TC<-0,hour_TC<-0,lat_TC<-0,lon_TC<-0,
                 v_TC<-0,area<-"N",rank_lat_TC<-0,rank_lon_TC<-0)
NoRI <- NoRI[-1,]
colnames(NoRI) <- c("year_TC","month_TC","day_TC","hour_TC","lat_TC",
                  "lon_TC","v_TC","area","rank_lat_TC","rank_lon_TC")

#Track_all <- read.table("NSWAP_4.csv",header=TRUE,sep=",")
Track_all<-read.csv("D:/Data/JTWCBST/all83-19/NSWAP_4.csv")

#number of TC
num<-0
for (i in 1:length(Track_all$year_TC)) {
  if(Track_all$year_TC[i] == 9999){
    num<-num+1
  }
}

#record line for name
Name<- matrix(0,nrow = num,ncol<-1)
numm <- 0
for (i in 1:length(Track_all$year_TC)) {
  if(Track_all$year_TC[i] == 9999){
    numm <- numm+1
    Name[numm]<-i
  }
}

#distinguish
for (j in 1:(length(Name)-1)) {
  ini1<-Name[j]
  ini2<-Name[j+1]
  
  #部分是82年数据，Name行连续，剔除数据
  if((ini2-ini1)>=6){
    numRI <- 0
    for (i in (ini1+1):(ini2-1-4)) {
      delta <- Track_all$v_TC[i+4] - Track_all$v_TC[i]
      if(delta>=30){
        numRI<-numRI+1
      }
    }
    if(numRI>0){
      for (k in (ini1+1):(ini2-1)) {
        RI<-rbind(RI,Track_all[k,])
      }
    }else{
      for (k in (ini1+1):(ini2-1)) {
        NoRI<-rbind(NoRI,Track_all[k,])
      }
    }
    
  }else if((ini2-ini1)>=3){
    delta <-  Track_all$v_TC[ini2-1] - Track_all$v_TC[ini1+1]
    if(delta>=30){
      for (k in (ini1+1):(ini2-1)) {
        RI<-rbind(RI,Track_all[k,])
      }
    }else{
      for (k in (ini1+1):(ini2-1)) {
        NoRI<-rbind(NoRI,Track_all[k,])
      }
    }
    print(ini1)
  }else if((ini2-ini1)==2){
    NoRI<-rbind(NoRI,Track_all[ini1+1,])
  }
  else{print(ini1)}
  
}

#对于最后一组路径数据
ini1 <- Name[length(Name)]
ini2 <- length(Track_all$year_TC)

numRI <- 0
for (i in (ini1+1):(ini2-4)) {
  delta <- Track_all$v_TC[i+4] - Track_all$v_TC[i]
  if(delta>=30){
    numRI<-numRI+1
  }
}
print(ini1)
if(numRI>0){
  for (k in (ini1+1):ini2) {
    RI<-rbind(RI,Track_all[k,])
  }
}else{
  for (k in (ini1+1):ini2) {
    NoRI<-rbind(NoRI,Track_all[k,])
  }
}

write.csv(RI,'NSWAP_RI24.csv',row.names=F,quote=F)
write.csv(NoRI,'NSWAP_NoRI24.csv',row.names=F,quote=F)
