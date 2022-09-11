setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件")
library(data.table)

path1<-"6.0pictures/result/"
coor1<-fread(paste0(path1,"P.csv"),stringsAsFactors=F,encoding="UTF-8")
coor<-subset(coor1,select=c(rank_lat,rank_lon))
pmean_37<-subset(coor1,select=c(rank_lat))
names(pmean_37)<-"pmean"
pmean_37$pmean<-NA

trend1<-fread(paste0(path1,"cal_trend/trend/trend_pmean.csv"),stringsAsFactors=F,encoding="UTF-8")
trend<-subset(trend1,select=-c(V1))
trend<-as.matrix(trend)

system.time(for (i in 1:1) {
  for (j in 1:10) {
    ind<-which(coor1$rank_lat==i&coor1$rank_lon==j)
    if(length(ind)!=0){
      if(is.na(trend[i,j])==FALSE){
        pmean_37$pmean[ind]<-trend[i,j]
      }
    }
  }
})


system.time(for (i in 1042358:1042368) {
  
  ind1<-coor1$rank_lat[i]
  ind2<-coor1$rank_lon[i]
  if(is.na(trend[ind1,ind2])==FALSE){
    pmean_37$pmean[i]<-trend[ind1,ind2]
  }
  
})

#costs too long time
# 
# write.csv(Pt_geo_cal,"Pt_trend_cal.csv",quote=F)



# setwd("F:/12.23/result/")
# library(data.table)
# 
# Pt_geo<-fread("Pt_geo.csv",stringsAsFactors=F,encoding="UTF-8")
# Pt_geo0<-subset(Pt_geo,select=-V1)
# Pt_geo0<-
#   InofRI_All0_trend<-fread("InofRI_All0_trend.csv",stringsAsFactors=F,encoding="UTF-8")
# InofRI_All0_trend<-subset(InofRI_All0_trend,select=-V1)
# InofRI_All0_trend$reg[which(is.na(InofRI_All0_trend$reg)==TRUE)]<-"NA"
# cal<-subset(InofRI_All0_trend,select=c(rank_lat_TC,trend_vmean,trend_v95,
#                                        trend_vmax,trend_lsmean,
#                                        trend_ls95,trend_lsmax))
# regn<-c("EP","NA","NI","SI","SP","WP")
# caten<-c("NoRI_land","RI_land","NoRI_ocean","RI_ocean")
# 
# record_v_ls<-data.frame("reg"<-rep(regn,each=length(caten)),
#                         "cate4"<-rep(caten,times=length(regn)),
#                         "v_max"<-rep(0,times=length(regn)*length(caten)),
#                         "v_95"<-rep(0,times=length(regn)*length(caten)),
#                         "v_mean"<-rep(0,times=length(regn)*length(caten)),
#                         "ls_max"<-rep(0,times=length(regn)*length(caten)),
#                         "ls_95"<-rep(0,times=length(regn)*length(caten)),
#                         "ls_mean"<-rep(0,times=length(regn)*length(caten)),
#                         
#                         "id_v"<-rep("v",times=length(regn)*length(caten)),
#                         "id_ls"<-rep("ls",times=length(regn)*length(caten)))
# colnames(record_v_ls)<-c("reg","cate4","v_max","v_95","v_mean",
#                          "ls_max","ls_95","ls_mean","id_v","id_ls")
# #length(regn)
# for (i in 1:length(regn)) {
#   #length(caten)
#   for (j in 1:length(caten)) {
#     ind<-which(InofRI_All0_trend$reg==regn[i]&InofRI_All0_trend$cate4==caten[j])
#     dd<-cal[ind,]
#     dd_vmean<-subset(dd,select=c(rank_lat_TC,trend_vmean))
#     dd_vmean<-dd_vmean[-which(is.na(dd_vmean$trend_vmean)==TRUE),]
#     
#     dd_v95<-subset(dd,select=c(rank_lat_TC,trend_v95))
#     dd_v95<-dd_v95[-which(is.na(dd_v95$trend_v95)==TRUE),]
#     
#     dd_vmax<-subset(dd,select=c(rank_lat_TC,trend_vmax))
#     dd_vmax<-dd_vmax[-which(is.na(dd_vmax$trend_vmax)==TRUE),]
#     
#     dd_lsmean<-subset(dd,select=c(rank_lat_TC,trend_lsmean))
#     dd_lsmean<-dd_lsmean[-which(is.na(dd_lsmean$trend_lsmean)==TRUE),]
#     
#     dd_ls95<-subset(dd,select=c(rank_lat_TC,trend_ls95))
#     dd_ls95<-dd_ls95[-which(is.na(dd_ls95$trend_ls95)==TRUE),]
#     
#     dd_lsmax<-subset(dd,select=c(rank_lat_TC,trend_lsmax))
#     dd_lsmax<-dd_lsmax[-which(is.na(dd_lsmax$trend_lsmax)==TRUE),]
#     
#     
#     #####vmean
#     ccos<-0
#     mm<-0
#     for (m in 1:length(dd_vmean$rank_lat_TC)) {
#       ll1<-dd_vmean$rank_lat_TC[m]*pi/180
#       ll<-cos(ll1)
#       if(is.na(ll)==TRUE){
#         print(m)
#       }else{
#         mm<-mm+dd_vmean$trend_vmean[m]*ll
#         ccos<-ccos+ll
#       }
#     }
#     if(ccos!=0){
#       record_v_ls$v_mean[(i-1)*length(caten)+j]<-round(mm/ccos,4)
#     }
#     #####v95
#     ccos<-0
#     mm<-0
#     for (m in 1:length(dd_v95$rank_lat_TC)) {
#       ll1<-dd_v95$rank_lat_TC[m]*pi/180
#       ll<-cos(ll1)
#       if(is.na(ll)==TRUE){
#         print(m)
#       }else{
#         mm<-mm+dd_v95$trend_v95[m]*ll
#         ccos<-ccos+ll
#       }
#     }
#     if(ccos!=0){
#       record_v_ls$v_95[(i-1)*length(caten)+j]<-round(mm/ccos,4)
#     }
#     #####vmax
#     ccos<-0
#     mm<-0
#     for (m in 1:length(dd_vmax$rank_lat_TC)) {
#       ll1<-dd_vmax$rank_lat_TC[m]*pi/180
#       ll<-cos(ll1)
#       if(is.na(ll)==TRUE){
#         print(m)
#       }else{
#         mm<-mm+dd_vmax$trend_vmax[m]*ll
#         ccos<-ccos+ll
#       }
#     }
#     if(ccos!=0){
#       record_v_ls$v_max[(i-1)*length(caten)+j]<-round(mm/ccos,4)
#     }
#     
#     #####lsmean
#     ccos<-0
#     mm<-0
#     for (m in 1:length(dd_lsmean$rank_lat_TC)) {
#       ll1<-dd_lsmean$rank_lat_TC[m]*pi/180
#       ll<-cos(ll1)
#       if(is.na(ll)==TRUE){
#         print(m)
#       }else{
#         mm<-mm+dd_lsmean$trend_lsmean[m]*ll
#         ccos<-ccos+ll
#       }
#     }
#     if(ccos!=0){
#       record_v_ls$ls_mean[(i-1)*length(caten)+j]<-round(mm/ccos,4)
#     }
#     #####ls95
#     ccos<-0
#     mm<-0
#     for (m in 1:length(dd_ls95$rank_lat_TC)) {
#       ll1<-dd_ls95$rank_lat_TC[m]*pi/180
#       ll<-cos(ll1)
#       if(is.na(ll)==TRUE){
#         print(m)
#       }else{
#         mm<-mm+dd_ls95$trend_ls95[m]*ll
#         ccos<-ccos+ll
#       }
#     }
#     if(ccos!=0){
#       record_v_ls$ls_95[(i-1)*length(caten)+j]<-round(mm/ccos,4)
#     }
#     #####lsmax
#     ccos<-0
#     mm<-0
#     for (m in 1:length(dd_lsmax$rank_lat_TC)) {
#       ll1<-dd_lsmax$rank_lat_TC[m]*pi/180
#       ll<-cos(ll1)
#       if(is.na(ll)==TRUE){
#         print(m)
#       }else{
#         mm<-mm+dd_lsmax$trend_lsmax[m]*ll
#         ccos<-ccos+ll
#       }
#     }
#     if(ccos!=0){
#       record_v_ls$ls_max[(i-1)*length(caten)+j]<-round(mm/ccos,4)
#     }
#     
#     
#     
#   }
#   
# }
# 
# path_pic<-"D:/Data/Semi_JWTC202007/RI_NoRI/6.0pictures/P4/"
# write.csv(record_v_ls,paste0(path_pic,"record_v_ls.csv"),quote=F)
# 


