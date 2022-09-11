setwd('D:/Data/JTWCBST/')#工作目录

InofTC_all <- data.frame(year_TC<-0,month_TC<-0,day_TC<-0,hour_TC<-0,lat_TC<-0,
                         lon_TC<-0,v_TC<-0,area<-"N",rank_lat_TC<-0,rank_lon_TC<-0)
InofTC_all <- InofTC_all[-1,]
colnames(InofTC_all)<-c("year_TC","month_TC","day_TC","hour_TC","lat_TC",
                        "lon_TC","v_TC","rank_lat_TC","rank_lon_TC","area")

year_all <- seq(from = 1983, to = 2019, by = 1)

for(year in year_all){
  print(year)
  #read data
  path_N <- paste0("unzip/Northern_Indian_Ocean/bio",year)
  path_S <- paste0("unzip/Southern_Hemisphere/bsh",year)
  path_W <- paste0("unzip/Western_North_Pacific/bwp",year)
  
  filenames_N <- list.files(path_N)
  filenames_S <- list.files(path_S)
  filenames_W <- list.files(path_W)
  
  InofTC <- data.frame(year_TC<-0,month_TC<-0,day_TC<-0,hour_TC<-0,
                       lat_TC<-0,lon_TC<-0,v_TC<-0,area<-"N")
  InofTC <- InofTC[-1,]
  colnames(InofTC)<-c("year_all","month_all","day_all","hour_all",
                      "lat_all","lon_all","v_all","area")
  
  #棣栧厛璇诲彇Northern_Indian_Ocean璇ュ勾0:00鐨勬暟鎹?
  for (j in filenames_N) {
    path_N_N <- file(paste0(path_N,"/",j),open = "r")#杩涜瀛楃涓叉棤闂撮殧杩炴帴
    line_N = readLines(path_N_N,n=-1L)
    LL<-c(9999,0,0,0,0,0,0,"bio")#to distinguish
    InofTC <- rbind(InofTC,LL,stringsAsFactors=F)
    
    for (ll in 1:length(line_N)) {
      
      if(length(line_N[ll]!=0)){
        data1 <- unlist(strsplit(line_N[ll],","))#渚夸簬绱㈠紩
        year_N <- substr(data1[3],2,5)
        month_N <- substr(data1[3],6,7)
        day_N <- substr(data1[3],8,9)
        hour_N <- as.numeric(substr(data1[3],10,11))
        lat_N <- data1[7]
        lon_N <- data1[8]
        v_N <- data1[9]
        
        LL <- c(year_N,month_N,day_N,hour_N,lat_N,lon_N,v_N,"bio")
        InofTC <- rbind(InofTC,LL,stringsAsFactors=F)
      
      }
    }#print(paste0(j,"宸茶璇诲彇"))
    close(path_N_N)
  }
  #print(InofTC_all)
  
  #Southern_Hemisphere
  for (j in filenames_S) {
    
    path_S_S <- file(paste0(path_S,"/",j),open = "r")#杩涜瀛楃涓叉棤闂撮殧杩炴帴
    line_S <- readLines(path_S_S,n=-1L)
    LL<-c(9999,0,0,0,0,0,0,"bsh")#to distinguish
    InofTC <- rbind(InofTC,LL,stringsAsFactors=F)
    
    for (ll in 1:length(line_S)) {
      
      if(length(line_S[ll])!=0){
        data2 <- unlist(strsplit(line_S[ll],","))
        year_S <- substr(data2[3],2,5)
        month_S <- substr(data2[3],6,7)
        day_S <- substr(data2[3],8,9)
        hour_S <- as.numeric(substr(data2[3],10,11))
        lat_S <- data2[7]
        lon_S <- data2[8]
        v_S <- data2[9]
        
        if (as.numeric(year_S)>1982){
          LL <- c(year_S,month_S,day_S,hour_S,lat_S,lon_S,v_S,"bsh")
          InofTC <- rbind(InofTC,LL,stringsAsFactors=F)
        }
      }
    }
    close(path_S_S)
  }
  #print(paste0(j,"宸茶璇诲彇"))

  #Western_North_Pacific璇ュ勾0:00鐨勬暟鎹?
  for (j in filenames_W) {
    path_W_W <- file(paste0(path_W,"/",j),open="r")#杩涜瀛楃涓叉棤闂撮殧杩炴帴
    line_W <- readLines(path_W_W,n=-1L)
    LL<-c(9999,0,0,0,0,0,0,"bwp")#to distinguish
    InofTC <- rbind(InofTC,LL,stringsAsFactors=F)
    
    for (ll in 1:length(line_W)) {
      
      if(length(line_W[ll])!=0){
        data3 <- unlist(strsplit(line_W[ll],","))
        year_W <- substr(data3[3],2,5)
        month_W <- substr(data3[3],6,7)
        day_W <- substr(data3[3],8,9)
        hour_W <- as.numeric(substr(data3[3],10,11))
        lat_W <- data3[7]
        lon_W <- data3[8]
        v_W <- data3[9]
        if (as.numeric(year_W)>1982){
          LL <- c(year_W,month_W,day_W,hour_W,lat_W,lon_W,v_W,"bwp")
          InofTC <- rbind(InofTC,LL,stringsAsFactors=F)
        }
      }
    }#print(paste0(j,"宸茶璇诲彇"))
    close(path_W_W)
  }
  
  #latitude[-60,60],order[480,1]
  rank_lat <- matrix()
  rank_lat <- rank_lat[-1,]
  
  for (k in 1:length(InofTC[,5])) {
    if(InofTC[k,1]==9999){
      rank_lat <- rbind(rank_lat,0)
    }else{
      num_ns <- nchar(InofTC[k,5])
      NS <- substr(InofTC[k,5],num_ns,num_ns)
      
      if (NS == "N") {
        InofTC[k,5] <- as.numeric(substr(InofTC[k,5],1,num_ns-1))/10
        #此时由于第五列本来的属性就是character，所以经过计算过后还是character
        rank_lat_ini <- 241-ceiling(as.numeric(InofTC[k,5])/0.25)
        rank_lat <- rbind(rank_lat,rank_lat_ini)
        
      }else if (NS == "S"){
        InofTC[k,5] <- as.numeric(substr(InofTC[k,5],1,num_ns-1))*(-1)/10
        rank_lat_ini  <- floor(as.numeric(InofTC[k,5])/ 0.25 )*(-1)+240
        rank_lat <- rbind(rank_lat,rank_lat_ini)
        
      }else {
        print("纬度转换失败")}

    }
  }
  
  InofTC <- cbind(InofTC,rank_lat)# rank of latitude
  
  #longitude[0,360],order[1,1440]
  rank_lon <- matrix()
  rank_lon <- rank_lon[-1,]
  
  for (k in 1:length(InofTC[,6])) {
    if(InofTC[k,1]==9999){
      rank_lon <- rbind(rank_lon,0)
    }else{
      num_we <- nchar(InofTC[k,6])
      WE <- substr(InofTC[k,6],num_we,num_we)
      
      if (WE == "E") {
        InofTC[k,6] <- as.numeric(substr(InofTC[k,6],1,num_we - 1))/10
        rank_lon_ini <- ceiling(as.numeric(InofTC[k,6])/0.25)
        rank_lon <- rbind(rank_lon,rank_lon_ini)
        
      }else if (WE == "W"){
        InofTC[k,6] <- as.numeric(substr(InofTC[k,6],1,num_we - 1))*(-1)/10 + 360
        rank_lon_ini <- ceiling(as.numeric(InofTC[k,6])/0.25)
        rank_lon <- rbind(rank_lon,rank_lon_ini)
        
      }else {
        print("经度转换失败")}
      
    }
  }
  InofTC <- cbind(InofTC,rank_lon)
  
  names(InofTC) <- c("year_all","month_all","day_all","hour_all","lat_all",
                     "lon_all","v_all","area","rank_lat_TC","rank_lon_TC")
  #所有的数据都是character
  InofTC_all <- rbind(InofTC_all,InofTC,stringsAsFactors=F)
}

#HURDAT2_Atlantic_Basin.txt
path_Atlantic <- "unzip/HURDAT2_Atlantic_Basin.txt"
con <- file(path_Atlantic,open ="r")
lines_AB <- readLines(con,n=-1L)

for (ll in 1:length(lines_AB)) {
  print(ll)
  if(length(lines_AB[ll])!=0){
    data4 <- unlist(strsplit(lines_AB[ll],","))
    loc_A <-substr(data4[1],1,1)
    year_A <- substr(data4[1],1,4)
    
    if(loc_A=="A"){
      LLA <- c(9999,0,0,0,0,0,0,"HAB",0,0)
      InofTC_all <- rbind(InofTC_all,LLA,stringsAsFactors=F)
    }else{
      month_A <- substr(data4[1],5,6)
      day_A <- substr(data4[1],7,8)
      hour_A <- as.numeric(substr(data4[2],1,3))
      lat_A <- data4[5]
      lon_A <- data4[6]
      v_A <- data4[7]
      #read N/S
      num_ns <- nchar(lat_A)
      NS <- substr(lat_A,num_ns,num_ns)
      
      if (NS == "N") {
        lat_A <- as.numeric(substr(lat_A,1,num_ns - 1))#最终生成的还是character
        rank_lat_A_ini <- 241 - ceiling(as.numeric(lat_A)/0.25)
      }else if (NS == "S"){
        lat_A <- as.numeric(as.numeric(substr(lat_A,1,num_ns - 1))*(-1))
        rank_lat_A_ini  <- floor(as.numeric(lat_A)/0.25)*(-1)+240
      }else {print("The latitude conversion is failed")}
      
      #read E/W
      num_we <- nchar(lon_A)
      WE <- substr(lon_A,num_we,num_we)
      
      if (WE == "E") {
        lon_A <- as.numeric(substr(lon_A,1,num_we - 1))
        rank_lon_A_ini <- ceiling(as.numeric(lon_A)/0.25)
      }else if (WE == "W"){
        lon_A <- as.numeric(as.numeric(substr(lon_A,1,num_we - 1))*(-1) + 360)
        rank_lon_A_ini <- ceiling(as.numeric(lon_A)/0.25)
      }else {print("The longitude conversion is failed")}
      
      LLA <- c(year_A,month_A,day_A,hour_A,lat_A,lon_A,v_A,"HAB",rank_lat_A_ini,rank_lon_A_ini)
      InofTC_all <- rbind(InofTC_all,LLA,stringsAsFactors=F)
      
    }
  }
}
close(con)

#HURDAT2_Northeast_Pacific_Basin.txt
path_Northeast <- "unzip/HURDAT2_Northeast_Pacific_Basin.txt"
con2 <- file(path_Northeast,open="r")
lines_NP <- readLines(con2,n=-1L)

for(ll in 1:length(lines_NP)){
  print(ll)
  if(length(lines_NP[ll])!=0){
    data5 <- unlist(strsplit(lines_NP[ll],","))
    loc_NP <-substr(data5[1],1,1)
    year_NE <- substr(data5[1],1,4)
    
    if(loc_NP %in% c("E","C")){
      LLNE <- c(9999,0,0,0,0,0,0,"HNPB",0,0)
      InofTC_all <- rbind(InofTC_all,LLNE,stringsAsFactors=F)
    }else{
      month_NE <- substr(data5[1],5,6)
      day_NE <- substr(data5[1],7,8)
      hour_NE <- as.numeric(substr(data5[2],1,3))
      lat_NE <- data5[5]
      lon_NE <- data5[6]
      v_NE <- data5[7]
      
      #read N/S
      num_ns <- nchar(lat_NE)
      NS <- substr(lat_NE,num_ns,num_ns)
      
      if (NS == "N") {
        lat_NE <- as.numeric(substr(lat_NE,1,num_ns - 1))
        rank_lat_NE_ini <- 241 - ceiling(as.numeric(lat_NE)/0.25)
      }else if (NS == "S"){
        lat_NE <- as.numeric(substr(lat_NE,1,num_ns - 1))*(-1)
        rank_lat_NE_ini  <- floor(as.numeric(lat_NE)/0.25)*(-1)+240
      }else {print("The latitude conversion is failed")}
      
      #read W/S
      num_we <- nchar(lon_NE)
      WE <- substr(lon_NE,num_we,num_we)
      
      if (WE == "E") {
        lon_NE <- substr(lon_NE,1,num_we - 1)
        rank_lon_NE_ini <- ceiling(as.numeric(lon_NE)/0.25)
      }else if (WE == "W"){
        lon_NE <- as.numeric(substr(lon_NE,1,num_we - 1))*(-1) + 360
        rank_lon_NE_ini <- ceiling(as.numeric(lon_NE)/0.25)
      }else {print("The longitude conversion is failed")}
      
      LLNE <- c(year_NE,month_NE,day_NE,hour_NE,lat_NE,lon_NE,v_NE,"HNPB",rank_lat_NE_ini,rank_lon_NE_ini)
      InofTC_all <- rbind(InofTC_all,LLNE,stringsAsFactors=F)
      
    }
  }
}
close(con2)

names(InofTC_all) <- c("year_TC","month_TC","day_TC","hour_TC","lat_TC",
                         "lon_TC","v_TC","area","rank_lat_TC","rank_lon_TC")

# for (j in 1:length(InofTC_all$lon_TC)) {
#   if (as.numeric(InofTC_all$lon_TC[j])>180){
#     InofTC_all$lon_TC[j] <- as.numeric(InofTC_all$lon_TC[j])-360
#     print(j)
#     #经过计算之后InofTC$lon_TC仍是character列
#   }
# }
library(dplyr)
InofTC_all2<-InofTC_all %>%
  filter(InofTC_all$hour_TC %in% c("0","6","12","18","24"))

write.csv(InofTC_all2,'all83-19/NSWAP_4.csv',row.names=F,quote=F)
# quote=F数据不会有引号