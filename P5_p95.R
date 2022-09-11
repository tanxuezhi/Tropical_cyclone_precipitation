setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/6.0xiugai/newdata")
library(ggplot2)
library(ggpubr)#scale_fill_manual ggarrange

trend_max_reg<-read.csv("p95_single_year.csv")
trend_max_reg$region[which(is.na(trend_max_reg$region)==TRUE)]<-"NA"

col<-c("#17243E",#"#243A6A",
       "#5190C7","#A5DBF6",#"#E8F0EE",
       #"#FFFFFF","#FFFEF3",
       #"#F8E38B",
       "#FFAE38","#F1722E","#B41921","#841719")

mytheme1<-theme(plot.title=element_text(size=30,color="black",hjust = 0.5),
                axis.title=element_text(size=28,color="black"),
                axis.text=element_text(size=26,color="black"),
                panel.background=element_rect(fill="white",color="white",size=1.5),
                #axis.line=element_line(colour="black",size=1),
                #panel.border= element_rect(colour="black",fill=NA),
                axis.line=element_line(colour="black",size=1.2),
                #axis.title.y = element_text(vjust=1),
                axis.title.y = element_text(margin=margin(t=0,r=10,b=0,l=0)),
                plot.margin=unit(c(0.8,2.5,0.5,0.5),'lines'),
                legend.position="bottom",
                legend.background=element_blank(),
                legend.key=element_blank(),
                legend.direction="horizontal",
                #legend.text.align=1,
                legend.spacing.x = unit(0.01,'cm'),
                legend.spacing.y = unit(0.01,'cm'),
                legend.text=element_text(size=20,color="black")
)
# data<-trend_max_reg[which(trend_max_reg$region=="WP"),]
# data<-subset(data,select=c(year,v))
# tdata<-ts(data,start=1983,freq=3)
# m1<-filter(tdata,filter=c(rep(1/5,5)))


max2<-ggplot(trend_max_reg,aes(x=year,y=average5,color=region,group=region))+
  geom_line(size=1.2)+
  scale_color_manual(values=col)+
  # guides(color=guide_legend(#reverse=TRUE,
  #   nrow=1,
  #   title.theme=element_text(size=32,color="black"),
  #   title.position="left",
  #   title.hjust=0,
  #   label.position="bottom",
  #   keyheight=2,keywidth=5
  # ))+
  scale_x_continuous(breaks=c(1983,1990,2000,2010,2019),
                     limits=c(1983,2019),expand=c(0,0))+
  labs(x=NULL,y=expression('TCP'~'intensity'~'(mm'*'*day'^'-1'~')'),
       title="Five year moving average of 95\nTCP intensity in each region from 1983 to 2019")+
  mytheme1
print(max2)
# 
# path_pic<-"F:/12.23/P4/picture4/P3_gai/"
# ma<-ggarrange(max1,max2,max3,max4,ncol=2,nrow=2,
#               legend="bottom",
#               common.legend=TRUE,
#               labels=c("A","B","C","D"),
#               widths=c(0.5,0.5,0.5,0.5),heights=c(0.4,0.4,0.4,0.4),
#               font.label=list(size=28,color="black",face="bold"))
# print(ma)
ggsave(filename="newpictures/P5/95_trend.png",max2,width=10.21,height=6.664)
ggsave(filename="newpictures/P5/95_trend.pdf",max2,width=10.21,height=6.664)


# library(cowplot)
# 
# x<-plot_grid(max1,max2,max3,max4,
#              labels = c("A","B","C","D"),ncol=2,
#              align="h")
# print(x)
# 
# save_plot("x.png", x,
#           ncol = 2, # we're saving a grid plot of 2 columns
#           nrow = 2, # and 2 rows
#           # each individual subplot should have an aspect ratio of 1.3
#           base_aspect_ratio = 1.3
# )


# mytheme_len<-theme(legend.position="bottom",
#                    legend.background=element_blank(),
#                    legend.key=element_blank(),
#                    legend.direction="horizontal",
#                    #legend.text.align=1,
#                    legend.spacing.x = unit(0.01,'cm'),
#                    legend.spacing.y = unit(0.01,'cm'),
#                    legend.text=element_text(size=20,color="black")
# )
# llen<-ggplot(trend_max_reg,aes(x=year,y=p,color=region,group=region))+
#   geom_line(size=1)+
#   scale_color_manual(values=col)+
#   guides(color=guide_legend(#reverse=TRUE,
#     nrow=1,
#     title.theme=element_text(size=32,color="black"),
#     title.position="left",
#     title.hjust=0,
#     label.position="bottom",
#     keyheight=2,keywidth=5
#   ))+
#   mytheme_len
# print(llen)
# 
# leg<-cowplot::get_legend(llen)
# # Convert to a ggplot and print
# a<-ggplotify::as.ggplot(leg)
# print(a)

