setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件")
library(ggplot2)
library(grDevices)#colorRampPalette
library(ggpubr)#ggarrange
library(data.table)

path1<-"6.0pictures/result/"
Data0<-fread(paste0(path1,"P.csv"),stringsAsFactors=F,encoding="UTF-8")
pre_F1<-subset(Data0,select=c(p,cate4))

path2<-"6.0xiugai/newdata/"
Data2<-fread(paste0(path2,"cate4_gai.csv"),stringsAsFactors=F,encoding="UTF-8")
Data2<-Data2[-1,]
pre_F1$cate4<-Data2$V2

#boxplot
mytheme2<-theme(plot.title=element_text(size=30,color="black",hjust = 0.5),
                axis.title=element_text(size=30,color="black"),
                axis.text=element_text(size=30,color="black"),
                panel.background=element_rect(fill="white",color="black",size=1.5),
                #axis.line=element_line(colour="black",size=1),
                panel.border= element_rect(colour="black",fill=NA)
)
my_func99<-function(x){
  return(quantile(x,0.99))
}
my_func95<-function(x){
  return(quantile(x,0.95))
}
my_func90<-function(x){
  return(quantile(x,0.9))
}

col2<-c("#17243E","#243A6A","#5190C7","#A5DBF6","#E8F0EE",
        "#FFFFFF",#"#FFFEF3",
        "#F8E38B","#FFAE38","#F1722E","#B41921")

my_v<-colorRampPalette(col2)
col_my1<-my_v(4)
#c("#4477AA","#68ACEA","#EE9988","#BB4444")
#c("#4477AA","#77AADD","#FFFFFF","#EE9988","#BB4444")

#####################################wind Speed
w2<-ggplot(pre_F1,aes(x=cate4,y=p,fill=cate4))+
  geom_boxplot(width=0.3,outlier.color=NA,position=position_dodge(0.5))+
  # scale_x_discrete(limits=c("RI_ocean","RI_land","NoRI_ocean","NoRI_land"))+
  # scale_fill_manual(values=alpha(c("#FFD027","#162C9a"),0.8),
  #                   labels=c("land","ocean"))+
  # stat_compare_means(aes(group=land_or_ocean),label="p.format",size=8)+
  # test p.signif p.format  #method="t.test"  #comparisons
  stat_summary(fun=my_func99,geom="point",shape=23,size=8,fill="white")+
  stat_summary(fun=my_func95,geom="point",shape=4,size=7)+
  stat_summary(fun=my_func90,geom="point",shape=3,size=7)+#outlier.color=NA)
  scale_fill_manual(name="category",values=col_my1
                    #values=c("#F5BA94","#6AE83A","#2e5ec0","#268AFF"),
                    )+
  labs(y=expression('TCP'~'torals'~'(mm'*'*day'^'-1'~')'))+
  annotate("text",x=3,y=93,label="+p90 ×p95 ◇p99",size=10)+
  theme(axis.title.x=element_blank())+
  scale_x_discrete(limits=c("NoRI_land","NoRI_ocean","RI_land","RI_ocean"),
                   labels=c("NoRI_\nland","NoRI_\nocean","RI_\nland","RI_\nocean"))+
  ylim(0,100)+
  guides(fill=FALSE)
#print(w2+mytheme2)
#ggsave(filename=paste0(path2,"newpictures/P2_1_2",".png"),w2+mytheme2,width=9.3,height=6.94)
ggsave(filename=paste0(path2,"newpictures/P2_1_2",".pdf"),w2+mytheme2,width=9.3,height=6.94)


#Mann-Whitney U检验
library(MASS)
library(dplyr)
NoRI_land<-filter(pre_F1,cate4=="NoRI_land")
NoRI_ocean<-filter(pre_F1,cate4=="NoRI_ocean")
RI_land<-filter(pre_F1,cate4=="RI_land")
RI_ocean<-filter(pre_F1,cate4=="RI_ocean")

Dif_cal<-list(NoRI_land$p,NoRI_ocean$p,RI_land$p,RI_ocean$p)
Dif<-matrix(0,nrow=length(Dif_cal),ncol=length(Dif_cal))
rownames(Dif)<-c("NoRI_land","NoRI_ocean","RI_land","RI_ocean")
colnames(Dif)<-c("NoRI_land","NoRI_ocean","RI_land","RI_ocean")
Med<-matrix(0,nrow=length(Dif_cal),ncol=length(Dif_cal))
rownames(Med)<-c("NoRI_land","NoRI_ocean","RI_land","RI_ocean")
colnames(Med)<-c("NoRI_land","NoRI_ocean","RI_land","RI_ocean")
#1:4,1:4
for (i in 1:length(Dif_cal)) {
  for (j in i:length(Dif_cal)) {
    cat1<-matrix(unlist(Dif_cal[j]),ncol=1)
    cat2<-matrix(unlist(Dif_cal[i]),ncol=1)
    Dif[j,i]<-wilcox.test(cat1,cat2)$p.value
    Med[j,i]<-median(cat1)-median(cat2)
  }
}

#获取下三角信息
library(reshape2)
Dif[upper.tri(Dif)]<-NA
Med[upper.tri(Med)]<-NA
#diag(Dif)<-NA
#diag(Med)<-NA
melt_Dif<-melt(Dif,na.rm=TRUE)
melt_Med<-melt(Med,na.rm=TRUE)
melt_Dif<-cbind(melt_Dif,melt_Med$value)
melt_Dif$value<-round(melt_Dif$value,digits=3)
value2<-matrix("0",nrow=length(melt_Dif$value))
for (i in 1:length(melt_Dif$value)) {
  if(melt_Dif$value[i]<0.001){
    value2[i]<-"<0.001"
  }else{value2[i]<-melt_Dif$value[i]}
}
melt_Dif$`melt_Med$value`<-round(melt_Dif$`melt_Med$value`,digits=0)
melt_Dif<-cbind(melt_Dif,value2)

mytheme3<-theme(plot.title=element_text(size=28,color="black",hjust = 0.5),
                axis.title=element_text(size=30,color="black"),
                axis.text=element_text(size=30,color="black"),
                panel.background=element_rect(fill="white"),#,color="grey"))
                #panel.border= element_rect(colour="black",fill=NA),
                legend.position=c(0.38,0.78),
                legend.direction="horizontal",
                legend.background=element_blank())
# panel.grid.major.x=element_line(color="grey",linetype=1),
# panel.grid.major.y=element_line(color="grey",linetype=1))
# legend.text=element_text(size = 13),
# legend.title=element_text(size = 13),
# legend.position=c(0.3, 0.7),
# legend.direction="horizontal")
library(grDevices)
col2<-c("#243A6A","#5190C7","#A5DBF6","#E8F0EE",
        "#FFFFFF",#"#FFFEF3",
        "#F8E38B","#FFAE38","#F1722E","#B41921")

my_v<-colorRampPalette(col2)

col_my2<-my_v(14)
#colorRampPalette(c("#4477AA","#77AADD","#FFFFFF","#EE9988","#BB4444"))
w3<-ggplot(data=melt_Dif,aes(x=Var1,y=Var2,fill=`melt_Med$value`))+
  geom_tile(color="white")+
  scale_fill_gradientn(name=expression(paste("TCP totals(mm*","day"^{-1},")")),
                       colours=col_my2)+
  labs(x=NULL,y=NULL,title=NULL)+
  geom_text(aes(Var1,Var2,label=`melt_Med$value`),color="black",size=10,vjust=-0.3)+
  geom_text(aes(Var1,Var2,label=value2),fontface='italic',color="black",size=8,vjust=1.5)+
  guides(fill = guide_legend(nrow=1,direction="horizontal",
                             title.position = "top",
                             label.position = "bottom",
                             keywidth = 1.5,keyheight = 1,
                             reverse = FALSE,
                             title.theme = element_text(size=24,colour="black",angle=0),#face ="italic"
                             label.theme = element_text(size=22,colour="black",angle=0)) # 在图例中修改图例主题，一般在主题函数内修改
  )+scale_y_discrete(position="right")+
  scale_x_discrete(limits=c("NoRI_land","NoRI_ocean","RI_land","RI_ocean"),
                   labels=c("NoRI_\nland","NoRI_\nocean","RI_\nland","RI_\nocean"))
#print(w3+mytheme3)
#ggsave(filename=paste0(path2,"newpictures/P2_2_2",".png"),w3+mytheme3,width=11.3*0.9,height=7.94*0.9)
ggsave(filename=paste0(path2,"newpictures/P2_2_2",".pdf"),w3+mytheme3,width=11.3*0.9,height=7.94*0.9)


#record p
record_p<-matrix(0,nrow=4,ncol=6)
qq<-c(0.25,0.5,0.75,0.9,0.95,0.99)
nn<-c("NoRI_land","NoRI_ocean","RI_land","RI_ocean")
for (i in 1:length(qq)) {
  for (j in 1:4) {
    print(paste(i,j))
    cc<-pre_F1[which(pre_F1$cate4==nn[j]),]$p
    record_p[j,i]<-quantile(cc,qq[i])
  }
}
record_p<-as.data.frame(record_p)
names(record_p)<-as.character(qq)
rownames(record_p)<-nn
write.csv(record_p,paste0(path2,"record_p.csv"),quote=F)

