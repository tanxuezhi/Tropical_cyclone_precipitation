setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/6.0xiugai/newdata")
library(ggplot2)

mytheme1<-theme(plot.title=element_text(family="sans",
                                        size=17,color="black",hjust = 0.5),
                axis.title=element_text(family="sans",size=18,color="black"),
                axis.text=element_text(size=18,color="black"),
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

ff<-read.csv("pmax_trend_record.csv",stringsAsFactors=F)
ff$region[which(is.na(ff$region)==TRUE)]<-"NA"
bb<-c(min(ff$index),0,max(ff$index)/2,max(ff$index))
#lla<-c((round(c(min(ff$index),2),"0.00",max(ff$index)/2,max(ff$index)),2)

ww<-1.9
cate4<-c("NoRI_land","RI_land","NoRI_ocean","RI_ocean")
limit_y<-c(min(ff$index),max(ff$index))

for(i in 1:length(cate4)){
  
  dd<-ff[which(ff$cate4==cate4[i]),]
  
  w1<-ggplot(dd,aes(x=id,y=index,fill=region))+
    geom_bar(stat="identity",width=0.1,position="dodge")+
    scale_fill_manual(name="category",values=col#,
                      #limits=c("NoRI_land","NoRI_ocean","RI_land","RI_ocean"
    )+
    guides(fill=FALSE)+#图例和之前的顺序一样
    labs(x=NULL,y=NULL)+
    scale_x_discrete(expand=c(0,0))+
    scale_y_continuous(limits=limit_y,breaks=bb,labels=lla,expand=c(0,0))+
    mytheme1
  print(w1)
  ggsave(filename=paste0("newpictures/P6/pmax/",cate4[i],".png"),w1,width=ww,height=ww*0.89,bg = "transparent")
  ggsave(filename=paste0("newpictures/P6/pmax/",cate4[i],".pdf"),w1,width=ww,height=ww*0.89,bg = "transparent")
  
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
# llen<-ggplot(ff,aes(x=id,y=index,fill=region))+
#   geom_bar(stat="identity",width=0.1,position="dodge")+
#   scale_fill_manual(name="regions",values=col)+
#   guides(fill=guide_legend(#reverse=TRUE,
#     ncol=1,
#     title.theme=element_text(size=26,color="black"),
#     label.theme=element_text(size=24,color="black"),
#     title.position="top",
#     title.hjust=0,
#     label.position="right",
#     keyheight=3.8,keywidth=1.1
#   ))+
#   mytheme_len
# print(llen)
# 
# leg<-cowplot::get_legend(llen)
# # Convert to a ggplot and print
# a<-ggplotify::as.ggplot(leg)
# print(a)
# 
# ggsave(filename=paste0("png/legend.png"),a,width=1.6,height=6,bg = "transparent")
# ggsave(filename=paste0("legend.pdf"),a,width=1.6,height=6,bg = "transparent")
