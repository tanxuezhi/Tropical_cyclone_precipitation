setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/5.99all_data/l_s/")
library(gstat);library(ggplot2)
library(maps);library(maptools)
library(ggpubr)
library(infotheo)#可以做等宽/等频分箱
library(hexbin)
library(RColorBrewer)
library(ggsci)
library(viridis)#color/subplot
library(ggpubr)#stat_compare_means
year<-1983:2019

#read RI data
InofRI_RI<-matrix(0,nrow=1,ncol=14)
InofRI_RI<-as.data.frame(InofRI_RI[-1,])
names(InofRI_RI)<-c("year_TC","month_TC","day_TC","hour_TC",
                    "lat_TC","lon_TC","v_TC","area",
                    "rank_lat_TC","rank_lon_TC",
                    "l_s","reg","category","land_or_ocean")
for (i in 1:length(year)) {
  
  Inof<-read.csv(paste0("RI/",year[i],"_RI_data_In.csv"))
  Inof2<-Inof[,-c(1,2)]
  Inof2[,c(5:7,9:11)]<-apply(Inof[,c(7:9,11:13)],2,as.numeric)
  
  Inof2$reg[is.na(Inof2$reg)]<-"NA"#na->NA
  InofRI_RI<-rbind(InofRI_RI,Inof2)
  
}

#read NoRI data
InofRI_NoRI<-matrix(0,nrow=1,ncol=14)
InofRI_NoRI<-as.data.frame(InofRI_NoRI[-1,])
names(InofRI_NoRI)<-c("year_TC","month_TC","day_TC","hour_TC",
                      "lat_TC","lon_TC","v_TC","area",
                      "rank_lat_TC","rank_lon_TC",
                      "l_s","reg","category","land_or_ocean")
for (i in 1:length(year)) {
  
  Inof<-read.csv(paste0("NoRI/",year[i],"_NoRI_data_In.csv"))
  Inof2<-Inof[,-c(1,2)]
  Inof2[,c(5:7,9:11)]<-apply(Inof[,c(7:9,11:13)],2,as.numeric)
  
  Inof2$reg[is.na(Inof2$reg)]<-"NA"#na->NA
  InofRI_NoRI<-rbind(InofRI_NoRI,Inof2)
  
}
#combine data
#InofRI2 数据未离散化
#InofRI 数据离散化
InofRI1<-rbind(InofRI_RI,InofRI_NoRI)
InofRI2<-InofRI1[-which(InofRI1$v_TC<0),]
InofRI<-InofRI2

InofRI$v_TC<-as.factor(as.matrix(discretize(InofRI2$v_TC,"equalwidth",9)))
InofRI$l_s<-as.factor(as.matrix(discretize(InofRI2$l_s,"equalwidth",9)))

#NoRI_RI 数据均未离散化
NoRI_RI<-rep("RI",times=length(InofRI_RI$year_TC))
InofRI_RI2<-cbind(InofRI_RI,NoRI_RI)
NoRI_RI<-rep("NoRI",times=length(InofRI_NoRI$year_TC))
InofRI_NoRI2<-cbind(InofRI_NoRI,NoRI_RI)
InofRI_All2<-rbind(InofRI_RI2,InofRI_NoRI2)
InofRI_All<-InofRI_All2[-which(InofRI_All2$v_TC<0),]

InofRI_land<-InofRI_All[which(InofRI_All$land_or_ocean=="land"),]
InofRI_ocean<-InofRI_All[which(InofRI_All$land_or_ocean=="ocean"),]


library(data.table)
p_path<-"D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/5.99all_data/pre/"
pre<-fread(paste0(p_path,"P_All.csv"),stringsAsFactors=T,encoding="UTF-8")
pre<-as.matrix(pre)

pre_cate<-matrix(0,nrow=1,ncol=5)
pre_cate<-pre_cate[-1,]
pre_cate<-as.data.frame(pre_cate)
names(pre_cate)<-c("line","p","cate","NoRI_RI","land_or_ocean")

for (i in 1:length(InofRI_All$year_TC)) {
  print(i)
  line_n<-which(pre[,2]==i)
  for (j in line_n) {
    for (m in 3:91) {
      if(as.numeric(pre[j,m])>0){
        #print(c(j,m))
        ll<-i
        p<-as.numeric(pre[j,m])
        cate<-InofRI_All$category[i]
        NoRI_RI<-InofRI_All$NoRI_RI[i]
        land_or_ocean<-InofRI_All$land_or_ocean[i]
        pre_cate<-rbind(pre_cate,
                        c(ll,p,cate,NoRI_RI,land_or_ocean))
      }
    }
  }
}
names(pre_cate)<-c("line","p","cate","NoRI_RI","land_or_ocean")
write.csv(pre_cate,paste0(p_path,'pre_cate.csv'),quote=F)
