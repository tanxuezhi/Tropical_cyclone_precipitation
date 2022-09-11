setwd("D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/6.0xiugai/newdata/newpictures/P2/")
library(ggplot2)
library(grDevices)#colorRampPalette
library(ggpubr)#ggarrange
library(fields)

library(data.table)
path1<-"D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/6.0pictures/result/"
P_F2<-fread(paste0(path1,"P.csv"),stringsAsFactors=F)
catt<-c("NoRI_land","RI_land","NoRI_ocean","RI_ocean")

data_all<-subset(P_F2,select=c(v,p))

path2<-"D:/Data/Semi_JWTC202007/RI_NoRI/正在用到的文件/6.0xiugai/newdata/"
cate4_gai<-fread(paste0(path2,"cate4_gai.csv"),header=TRUE,stringsAsFactors=F)

###plot settings
colp2<-c("#FFFFFF",
         "#F8E38B","#FFAE38","#F1722E","#B41921","#841719")
Lab.palette <- colorRampPalette(colp2, space = "Lab")

# pdf("2_1_3ls.pdf",width=18.3*0.7,height=13.94*0.7)
# #12.81 9.758
# par(oma=c(0,0,0,4)) # margin of 4 spaces width at right hand side
fudgeit <- function(){
  xm <- get('xm', envir = parent.frame(1))
  ym <- get('ym', envir = parent.frame(1))
  z  <- get('dens', envir = parent.frame(1))
  colramp <- get('colramp', parent.frame(1))
  fields::image.plot(xm,ym,z, col = colramp(200),legend.only=T,
                     # breaks=c(0,0.25,0.5,0.75,1),
                     legend.mar=1,
                     legend.width=2,#legend.cex=3,
                     #breaks=brk,
                     horizontal=TRUE,
                     legend.lab="Density",
                     add=F,
                     zlim=c(0,0.1))
}
xxn<-c(NA,NA,"wind speed(knots)","wind speed(knots)")
yyn<-c(expression('TCP'~'intensity'~'(mm'*'*day'^'-1'~')'),NA,
       expression('TCP'~'intensity'~'(mm'*'*day'^'-1'~')'),NA)
#lln<-c(NA,NA,NA,fudgeit)
#pdf("2_1_3ls.pdf",width=18.3*0.7,height=13.94*0.7)
#windows()
pdf("P2_3.pdf",width=18.3*0.7,height=13.94*0.7)
#c(bottom, left, top, right)
par(oma=c(0,0,0,0),mar=.1+c(4.8,6,5,1.3))
set.panel(2,2) 
for (i in 1:4) {
  ind<-which(cate4_gai$x==catt[i])
  dd<-data_all[ind,]
  
  if(i==1){
    smoothScatter(dd,colramp = Lab.palette,
                  nrpoints =100,ret.selection=TRUE,postPlotHook=fudgeit,
                  xlim=c(0,170),
                  ylim=c(0,500),
                  xlab=xxn[i],
                  ylab=yyn[i],
                  cex.axis=2,cex.lab=2.5,cex.main=2.3,
                  main=paste0("Density of ",catt[i]," TCP intensity"))
    
  }else{
    smoothScatter(dd,colramp = Lab.palette,
                  nrpoints =100,ret.selection=TRUE,
                  xlim=c(0,170),
                  ylim=c(0,500),
                  xlab=xxn[i],
                  ylab=yyn[i],
                  cex.axis=2,cex.lab=2.5,cex.main=2.3,
                  main=paste0("Density of ",catt[i]," TCP intensity"))
  
  }
  print(i)
}

# par(oma=c(0,0,0,0),mar=.1+c(7,7,3,7))# reset margin to be much smaller.
# fields::image.plot(legend.only=TRUE,zlim=c(0,0.1),col=Lab.palette,horizontal=FALSE)
dev.off()

