setwd("/scratch/xtan/Elsa_1/Semi2/")
library(geosphere)

year <- 2011

#define function
day365 <- c(31,28,31,30,31,30,31,31,30,31,30,31)
day366 <- c(31,29,31,30,31,30,31,31,30,31,30,31)

myfunc<- function(oneyear_TC,onemon_TC,oneday_TC){
  ddfor <- 0
  if(oneyear_TC %% 4 == 0){
    if(onemon_TC==1){ddfor <- 0}else{
      
      for (trmm in 1:(onemon_TC-1)) {
        ddfor <- ddfor + day366[trmm]
      }
    }
    
  }else{
    if(onemon_TC==1){ddfor <- 0}else{
      
      for (trmm in 1:(onemon_TC-1)) {
        ddfor <- ddfor + day365[trmm]
      }
    }
  }
  oneday_TC <- oneday_TC + ddfor
  return(oneday_TC)
}


for (i in 1:length(year)){
  print(year[i])
  InofTC_all <- read.table(paste0("Tracks/RI/",year[i],"_RI.csv"),header=TRUE,sep=",")
  
  # create folder to record data
  Result_path <- paste0("Results/RI/",year[i],"/")
  dir.create(Result_path)
  pre_path<-paste0(Result_path,year[i],"pre/")
  dir.create(pre_path)
  
  #read data
  InofTC2 <- data.frame(year_TC<-0,month_TC<-0,day_TC<-0,hour_TC<-0,lat_TC<-0,
                       lon_TC<-0,v_TC<-0,area<-"N",rank_lat_TC<-0,rank_lon_TC<-0)
  InofTC2 <- InofTC2[-1,]
  names(InofTC2) <- c("year_TC","month_TC","day_TC","hour_TC","lat_TC",
                      "lon_TC","v_TC","area","rank_lat_TC","rank_lon_TC")
  InofTC_more<- data.frame(year_TC<-0,month_TC<-0,day_TC<-0,hour_TC<-0,lat_TC<-0,
                           lon_TC<-0,v_TC<-0,area<-"N",rank_lat_TC<-0,rank_lon_TC<-0)
  InofTC_more <- InofTC_more[-1,]
  names(InofTC_more) <- c("year_TC","month_TC","day_TC","hour_TC","lat_TC",
                          "lon_TC","v_TC","area","rank_lat_TC","rank_lon_TC")
  #select data
  for (j in 1:length(InofTC_all$year_TC)) {
    if (year[i]==InofTC_all$year_TC[j] & InofTC_all$hour_TC[j]==0){
      
      if(InofTC_all$rank_lat_TC[j] %in% 32:488){
        InofTC2 <- rbind(InofTC2,InofTC_all[j,])
      }else{
        InofTC_more <- rbind(InofTC_more,InofTC_all[j,])
      }
    }
  }
  print(length(InofTC2[,1]))
  InofTC<-InofTC2[!duplicated(InofTC2),]
  print(length(InofTC[,1]))
  
  #record data for one year
  RI_1_1<-matrix(0,ncol=56)
  RI_1_0<-matrix(0,ncol=56)
  RI_1_1<-RI_1_1[-1,]
  RI_1_0<-RI_1_0[-1,]
  
  
  #length(InofTC$year_TC)
  for (k in 1:length(InofTC$year_TC)) {
    
    oneyear_TC <- InofTC$year_TC[k]
    onerank_lat_TC <- InofTC$rank_lat_TC[k]
    
    #由于边界条件的缺失 且本身边界上的点比较少，所以纬度不考虑最上/最下8度的范围
    #distm(c(180,58.5),c(180,52),fun = distGeo)/1000=723.6301
    #distm(c(180,52),c(169,52),fun = distGeo)/1000=754.7369经度要计算前后11度的范围
    if(onerank_lat_TC>=32 &onerank_lat_TC<=448){
      
      onemon_TC <- InofTC$month_TC[k]
      oneday_TC2 <- InofTC$day_TC[k]
      onerank_lon_TC <- InofTC$rank_lon_TC[k]
      
      #首先要进行天数的转换,Persiann-cdr:1-365
      oneday_TC<-myfunc(oneyear_TC,onemon_TC,oneday_TC2)
      
      #find precipitation
      path_P <- paste0("/scratch/xtan/suk_1/persiann-csv/",oneyear_TC)
      filenames_P <- list.files(path_P)
      
      for (PP in 1:length(filenames_P)){
        time_P <- unlist(strsplit(filenames_P[PP],"-"))
        year_P <- as.numeric(time_P[1])
        month_P <- as.numeric(time_P[2])
        day_P <- as.numeric(unlist(strsplit(time_P[3],".",fixed = T))[1])#365/366a
        
        if (oneyear_TC == year_P & onemon_TC == month_P & oneday_TC == day_P){
          
          print(paste("successfully read",year_P,month_P,day_P))
          num1_1 <- matrix(0,ncol = 56)
          num1_0 <- matrix(0,ncol = 56)
          num_none <- 0
          
          #center:InofTC$lon_TC[k],InofTC$lat_TC[k]
          file_P <- paste0(path_P,"/",filenames_P[PP])
          file_open <- file(file_P,open = "r")
          data_P <- read.csv(file_open,header=TRUE,sep=",")
          data_P[is.na(data_P)] <- 0
          
          data_P <- data_P[,-1]#delete the first column
          
          lon_P <- seq(from = 0.125, to = 359.875, by = 0.25)
          lat_P <- seq(from = 59.375, to = -59.375, by = -0.25)
          #对于lat_P:delete 1,2,479,480
          close(file_open)
          
          #all the precipitation stations in 700km
          dist_700 <- data.frame(lon_700<-0,lat_700<-0,P_700<-0)
          dist_700 <- dist_700[-1,]
          num <- 0
          
          #distm(c(180,52),c(169,52),fun = distGeo)/1000=754.7369
          #15*4=60(700km)
          #通过秩次确认需要计算的范围
          if (onerank_lon_TC <= 44){
            llo1 <- 1:(onerank_lon_TC+44)
            llo2 <- (1440-(44-onerank_lon_TC)):1440
            llo <- c(llo1,llo2)
          }else if (onerank_lon_TC > 1396){
            llo1 <- 1:(44-(1440-onerank_lon_TC))
            llo2 <- (onerank_lon_TC-44):1440
            llo <- c(llo1,llo2)
          }else{
            llo <- (onerank_lon_TC-44):(onerank_lon_TC+44)
          }
          
          #记录周围的pre秩次，便于后续检索
          lla<-(onerank_lat_TC-2-32):(onerank_lat_TC-2+32)
          pre<-matrix(-1,nrow=length(lla)+1,ncol=length(llo)+1)
          pre[1,2:length(pre[1,])]<-llo
          pre[2:length(pre[,1]),1]<-lla
          
          #record the coordinates in 700km ——> dist_700
          for (oo in 1:length(llo)) {
            #[180,360]——>[-180,0]
            lo_ind<-llo[oo]
            if (lon_P[lo_ind]>180){
              lon_P[lo_ind] <- lon_P[lo_ind] - 360
            }
            
            #-2 to match the excel
            for (aa in 1:length(lla)) {
              la_ind<-lla[aa]
              dist <- distm(c(InofTC$lon_TC[k],InofTC$lat_TC[k]),
                            c(lon_P[lo_ind],lat_P[la_ind]), fun = distGeo)/1000
              if (dist <= 700){
                num <- num + 1
                dist_700<-rbind(dist_700,c(lon_P[lo_ind],lat_P[la_ind],data_P[(la_ind-2),lo_ind]))
                pre[aa+1,oo+1]<-data_P[(la_ind-2),lo_ind]
                }
            }
          }
          print(paste0(num,"stations in total"))
          
          #cal semi
          for (len_p in 1:(length(dist_700[,1])-1)) {
            for (len_p2 in (len_p+1):length(dist_700[,1])) {
              dist2 <- distm(c(dist_700[len_p,1],dist_700[len_p,2]),
                             c(dist_700[len_p2,1],dist_700[len_p2,2]),
                             fun = distGeo)/1000
              for (n in 1:56) {
                if(dist2 >= 25*(n-1) & dist2 < 25*n){
                  if(dist_700[len_p,3]>=1 & dist_700[len_p2,3]>=1){
                    num1_1[n]<- num1_1[n] + 1
                  }else if(dist_700[len_p,3]<1 & dist_700[len_p2,3]<1){
                    num_none <- num_none + 1
                  }else{num1_0[n]<- num1_0[n] + 1}
                }
              }

            }
          }
          #record precipitation
          write.csv(pre,paste0(pre_path,"line",k,"_",onemon_TC,"_",InofTC$day_TC[k],"_",
                               InofTC$lat_TC[k],"_",InofTC$lon_TC[k],"_",InofTC$v_TC[k],"_",
                               InofTC$area[k],"_",
                               InofTC$rank_lat_TC[k],"_",InofTC$rank_lon_TC[k],"_RI.csv"))
          RI_1_1<-rbind(RI_1_1,num1_1)
          RI_1_0<-rbind(RI_1_0,num1_0)
          
          print(paste("line",k))
          print(InofTC[k,])
          print(num1_1)
          
        }#year = year month = month day = day
      }
      
    }#data for boundary area
  }
  
  write.csv(RI_1_1,paste0(Result_path,year[i],'_','RI_1_1.csv'),quote=F)
  write.csv(RI_1_0,paste0(Result_path,year[i],'_','RI_1_0.csv'),quote=F)
  write.csv(InofTC,paste0(Result_path,year[i],'_','RI_InofTC.csv'),quote=F)
  write.csv(InofTC_more,paste0(Result_path,year[i],'_','RI_InofTC_more.csv'),quote=F)
}

