setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/6.0xiugai/newdata")
library(ggplot2)
iin<-c("pmean","p95","pmax")
caten<-c("NoRI_land","RI_land","NoRI_ocean","RI_ocean")

mytheme1<-theme(plot.title=element_text(family="sans",
                                        size=12,color="black",hjust = 0.5),
                axis.title=element_text(family="sans",size=12,color="black"),
                axis.text=element_text(size=12,color="black"),
                panel.background=element_rect(fill="transparent",color="transparent"),
                plot.background=element_rect(fill="transparent",color="transparent"),
                axis.title.y = element_text(margin=margin(t=0,r=0.1,b=0,l=0)),
                axis.line.x = element_line(colour="black",
                                           size=0.6,
                                           lineend = "butt"),
                axis.text.x=element_blank(),
                plot.margin=unit(c(0.8,0.5,0.5,0.5),'lines'),
                axis.line.y = element_line(colour="black",size=0.6
                ))
col<-c("#17243E",#"#243A6A",
       "#5190C7","#A5DBF6",#"#E8F0EE",
       #"#FFFFFF","#FFFEF3",
       #"#F8E38B",
       "#FFAE38","#F1722E","#B41921","#841719")
#length(iin)
for (i in 1:length(iin)) {
  ff<-read.csv(paste0(iin[i],"_trend_record.csv"),stringsAsFactors=F)
  names(ff)<-c("X","reg","cate4","index","id")
  ff$reg[which(is.na(ff$reg)==TRUE)]<-"NA"
  
  # if(i %in% c(2,3)){
  #   ww=1.78*0.8
  # }else{
  #   ww=1.7*0.8
  # }
  #ww=1.7*0.8
  
  #limit_y<-c(min(ff$index),max(ff$index))
  mma<-max(ff$index)
  mmi<-min(ff$index)
  limit_y<-c(mmi-0.005,mma+0.005)
  #lo<-(mma-mmi)/5 #by lo
  
  #+ & -
  inn1<-(mma-0)/2
  inn2<-(0-mmi)/2
  if((inn1-inn2)>=0.06*(mma-mmi)){
    b1<-c(round(c(mmi),2),0)
    b2<-c(round(c(mma/2,mma),2))
    break_y<-c(b1,b2)
  }else if((inn2-inn1)>=0.06*(mma-mmi)){
    b1<-c(round(c(mmi,mmi/2),2),0)
    b2<-c(round(c(mma),2))
    break_y<-c(b1,b2)
  }else{
    b1<-c(round(c(mmi,mmi/2),2),0)
    b2<-c(round(c(mma/2,mma),2))
    break_y<-c(b1,b2)
  }
  
  for (j in 1:length(caten)) {
    if(j == 3){
      ww=1.78*0.8
    }else{
      ww=1.7*0.8
    }
    
    dd<-ff[which(ff$cate4==caten[j]),]
    w1<-ggplot(dd,aes(x=id,y=index,fill=reg))+
      geom_bar(stat="identity",width=0.1,position="dodge")+
      scale_fill_manual(name="category",values=col#,
                        #limits=c("NoRI_land","NoRI_ocean","RI_land","RI_ocean"
      )+
      guides(fill=FALSE)+
      labs(x=NULL,y=NULL,title=caten[j])+
      scale_x_discrete(expand=c(0,0))+
      scale_y_continuous(limits=limit_y,breaks=break_y,expand=c(0,0))+
      mytheme1
    print(w1)
    ggsave(filename=paste0("newpictures/P46/",iin[i],"_",caten[j],".png"),w1,width=ww,height=1.94*0.8,bg = "transparent")
    ggsave(filename=paste0("newpictures/P46/",iin[i],"_",caten[j],".pdf"),w1,width=ww,height=1.94*0.8,bg = "transparent")
    
  }
}


# mytheme_len<-theme(legend.position="bottom",
#                    legend.background=element_blank(),
#                    legend.key=element_blank(),
#                    legend.direction="vertical",#"horizontal",
#                    #legend.text.align=1,
#                    legend.spacing.x = unit(0.1,'cm'),
#                    legend.spacing.y = unit(0.1,'cm'),
#                    legend.text=element_text(size=20,color="black")
# )
# llen<-ggplot(dd,aes(x=id,y=index,fill=reg))+
#   geom_bar(stat="identity",width=0.1,position="dodge")+
#   scale_fill_manual(name="regions",values=col)+
#   guides(fill=guide_legend(#reverse=TRUE,
#     ncol=1,
#     title.theme=element_text(size=14,color="black"),
#     label.theme=element_text(size=12,color="black"),
#     title.position="top",
#     title.hjust=0,
#     label.position="right",
#     keyheight=1.6,keywidth=0.8
#   ))+
#   mytheme_len
# print(llen)
# 
# leg<-cowplot::get_legend(llen)
# # Convert to a ggplot and print
# a<-ggplotify::as.ggplot(leg)
# print(a)
# 
# ggsave(filename=paste0("P34/png/legend.png"),a,width=1.6*0.8,height=3*0.8,bg = "transparent")
# ggsave(filename=paste0("P34/legend.pdf"),a,width=1.6*0.8,height=3*0.8,bg = "transparent")
