#In for RI data
#ls reg category 
library(gstat)
library(ggplot2)
library(sp)#point.in.ploygon
setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/5.8draw/Results/RI/")

#Step 1:cal ls
year_RI<-c(1983:2019)

#set reg
region<-rgdal::readOGR("D:/Data/shp文件/my分区/region3/region3_polygon.shp")
#判断是否在不同区域内 points.in.polygon
#RI和非RI都需要画成子图形式:以区域为划分
#先交给老师中文纸质版
#对于region
#0:South Pacific
#1:South Atlantic
#2:South Indian
#3:Eastern Pacific
#4:North Atlantic
#5:North Indian
#6:Western Pacific
location<-fortify(region)
x_SP<-location$long[which(location$id==0)]
y_SP<-location$lat[which(location$id==0)]
x_SA<-location$long[which(location$id==1)]
y_SA<-location$lat[which(location$id==1)]
x_SI<-location$long[which(location$id==2)]
y_SI<-location$lat[which(location$id==2)]
x_EP<-location$long[which(location$id==3)]
y_EP<-location$lat[which(location$id==3)]
x_NA<-location$long[which(location$id==4)]
y_NA<-location$lat[which(location$id==4)]
x_NI<-location$long[which(location$id==5)]
y_NI<-location$lat[which(location$id==5)]
x_WP<-location$long[which(location$id==6)]
y_WP<-location$lat[which(location$id==6)]

#read all RI data
for (i in 1:length(year_RI)) {
  print(year_RI[i])
  result_path<-paste0(year_RI[i],'/')
  
  data_In<-read.csv(paste0(result_path,year_RI[i],"_RI_InofTC.csv"))#row header is not included
  data_1_1<-read.csv(paste0(result_path,year_RI[i],"_RI_1_1.csv"))
  data_1_0<-read.csv(paste0(result_path,year_RI[i],"_RI_1_0.csv"))
  
  data_1_1<-data_1_1[,-1]#delete the first col
  data_1_0<-data_1_0[,-1]#delete the first col
  
  #!match the record data(1_1/1_0)
  path_P<-paste0(result_path,year_RI[i],"pre/")
  filenames_P<-list.files(path_P)
  
  Ind_true<-numeric()
  for (j in 1:length(filenames_P)){
    aa<-unlist(strsplit(filenames_P[j],"_"))[1]
    bb<-as.numeric(substr(aa,5,nchar(aa)))
    Ind_true[j]<-bb
  }
  
  #save
  Ind_true<-sort(Ind_true)
  data_In2<-data_In[Ind_true,]
  
  data_1_1<-matrix(unlist(data_1_1),ncol=56)
  data_1_0<-matrix(unlist(data_1_0),ncol=56)
  
  #delete error data
  Ind_error<-which(data_In2$v_TC<0)
  data_In2<-data_In2[-Ind_error,]
  data_1_1<-data_1_1[-Ind_error,]
  data_1_0<-data_1_0[-Ind_error,]
  
  #set grid 1:480-[60,-60],1:1440[0,180,-180]
  #cal the coord of the center of the grid
  data_In3<-data_In2
  for (k in 1:length(data_In3$rank_lat_TC)) {
    rr_la<-data_In3$rank_lat_TC[k]
    rr_lo<-data_In3$rank_lon_TC[k]
    
    if(rr_la<=240){
      data_In3$rank_lat_TC[k]<-59.875-(rr_la-1)/4
    }else{
      data_In3$rank_lat_TC[k]<--0.125-(rr_la-241)/4
    }
    
    if(rr_lo<=720){
      data_In3$rank_lon_TC[k]<-0.125+(rr_lo-1)/4
    }else{
      data_In3$rank_lon_TC[k]<--179.875+(rr_lo-721)/4
    }
  }
  
  #cal gamma(h)----experimental semivariogram
  gam<-data_1_1
  np<-data_1_1
  
  for (m in 1:length(data_1_1[,1])) {
    for (n in 1:length(data_1_1[m,])) {
      np[m,n]<-data_1_0[m,n]+data_1_1[m,n]
      if(np[m,n]==0){
        gam[m,n]<-0
      }else{
        gam[m,n]<-0.5*(data_1_0[m,n]/np[m,n])}
    }
  }
  
  #cal l_s
  #l_s<-rep("ll",times=length(data_In3[,1]))
  l_s<-matrix(0,nrow=length(data_In3[,1]),ncol=1)
  data_In3<-cbind(data_In3,l_s)
  
  for (j in 1:length(gam[,1])) {
    if(sum(gam[j,])!=0){
      name<-c('np','dist','gamma','dir.hor','dir.ver','id')
      dist<-c(1:56)*25-12.5
      vario<-matrix(0,nrow=56,ncol=6)
      vario[,1]<-np[j,]
      vario[,2]<-dist
      vario[,3]<-gam[j,]
      vario[,4]<-vario[,5]<-0
      vario[,6]<-1
      
      vario<-as.data.frame(vario)
      colnames(vario)<-name
      class(vario)<-c('gstatVariogram','data.frame')
      vfit<-fit.variogram(vario,vgm(model='Sph'))
      data_In3$l_s[j]<-vfit$range
      #print(vfit$range)
      #dev.new()
      #plot(vario,vfit,type="o",cex = 1,lwd=2,pch=16,col="steelblue4")
    }
  }
  
  #cal reg
  reg<-rep("SP",times=length(data_In3[,1]))
  data_In3<-cbind(data_In3,reg)
  n=0
  for (j in 1:length(data_In3[,1])) {
    
    if(point.in.polygon(data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j],
                        x_SP,y_SP, mode.checked=FALSE)==1){
      data_In3$reg[j]<-"SP"
    }else if(point.in.polygon(data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j],
                              x_SA,y_SA, mode.checked=FALSE)==1){
      data_In3$reg[j]<-"SA"
    }else if(point.in.polygon(data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j],
                              x_SI,y_SI, mode.checked=FALSE)==1){
      data_In3$reg[j]<-"SI"
    }else if(point.in.polygon(data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j],
                              x_EP,y_EP, mode.checked=FALSE)==1){
      data_In3$reg[j]<-"EP"
    }else if(point.in.polygon(data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j],
                              x_NA,y_NA, mode.checked=FALSE)==1){
      data_In3$reg[j]<-"NA"
    }else if(point.in.polygon(data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j],
                              x_NI,y_NI, mode.checked=FALSE)==1){
      data_In3$reg[j]<-"NI"
    }else if(point.in.polygon(data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j],
                              x_WP,y_WP, mode.checked=FALSE)==1){
      data_In3$reg[j]<-"WP"
    }else{print(paste(j,data_In3$rank_lon_TC[j],data_In3$rank_lat_TC[j]))
      n=n+1}
    
  }
  print(n)
  #部分点在边界上
  #InofRI$reg[518]<-"SP"
  #InofRI$reg[1826]<-"WP"
  
  #cal category
  # TD<=33,TS:34-63,One:64-82,Two:83-95,Three:96-112
  # Four:113-136,Five>=137
  category<-matrix("TD",nrow=length(data_In3$v_TC))
  data_In3<-cbind(data_In3,category)
  for (j in 1:length(data_In3$v_TC)) {
    if(data_In3$v_TC[j]>=137){
      data_In3$category[j]<-"Five"
    }else if(data_In3$v_TC[j]>=113){
      data_In3$category[j]<-"Four"
    }else if(data_In3$v_TC[j]>=96){
      data_In3$category[j]<-"Three"
    }else if(data_In3$v_TC[j]>=83){
      data_In3$category[j]<-"Two"
    }else if(data_In3$v_TC[j]>=64){
      data_In3$category[j]<-"One"
    }else if(data_In3$v_TC[j]>=34){
      data_In3$category[j]<-"TS"
    }else{data_In3$category[j]<-"TD"}
  }
  data_In3$category=factor(data_In3$category,levels=c('TD','TS','One','Two','Three','Four','Five'))
  
  #cal land_or_ocean
  data_In3[,c(6:8,10:12)]<-apply(data_In3[,c(6:8,10:12)],2,as.numeric)
  land_or_ocean<-rep("ll",times=length(data_In3[,1]))
  data_In3<-cbind(data_In3,land_or_ocean)
  
  land_476<-read.csv("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/5.99all_data/land_mask.csv")
  land_476<-land_476[,-1]
  #identify the location order
  for (j in 1:length(data_In3[,1])) {
    latt<-data_In3$rank_lat_TC[j]
    lonn<-data_In3$rank_lon_TC[j]
    
    if(latt<=0){
      rank_tt<-(-latt+0.125)*4+240
    }else{rank_tt<-241-(latt+0.125)*4}
    
    if(lonn<=0){
      rank_nn<-720+(lonn+0.125+180)*4
    }else{rank_nn<-(lonn+0.125)*4}
    
    if(is.na(land_476[rank_tt-2,rank_nn])==TRUE){
      data_In3$land_or_ocean[j]<-"ocean"
    }else if(land_476[rank_tt-2,rank_nn]==1){
      data_In3$land_or_ocean[j]<-"land"
    }else{data_In3$land_or_ocean[j]<-"aaa"}
    
  }
  
  write_path<-"D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/5.99all_data/l_s/RI/"
  write.csv(data_In3,paste0(write_path,year_RI[i],"_RI_data_In.csv"),quote=F)
}
