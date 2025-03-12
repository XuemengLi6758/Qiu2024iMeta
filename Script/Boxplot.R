rm(list = ls())


setwd("C:/Users")
data <- read.csv("",header=T,row.names=1)
data <-log2(data+1)

mygroup <- read.csv("",header=T,row.names=1)
rownames(mygroup)<-colnames(data)
data <-t(data)
mydata <- as.data.frame(data)
mydata$group <- mygroup$group
mydata$group <- factor(mydata$group,levels = c("","","",""))

library(RColorBrewer)
library(ggpubr)
library(ggplot2)

my_comparisons <- list(c("",""),
                       c("",""),
                       c("",""),
                       c("",""))

color <-c("#FA7676","#749ABF","#F1CA74","#97D696")
centenarians3 <- head(colnames(mydata), -1)  #此处的-1不需要根据数量修改了

library(rstatix)
library(tidyverse)

plist<-list()
for (i in 1:length(centenarians3)){
  box<-mydata[,c(centenarians3[i],"group")]
  colnames(box)<-c("Expression","group")
  stat.test <- box %>% 
    wilcox_test(Expression ~ group, 
                comparisons = list(c("",""),
                                   c("",""),
                                   c("",""),
                                   c("","")),
                paired = FALSE,
                p.adjust.method = 'fdr', alternative = "two.sided",
                conf.level = 0.95,
                exact = T) %>% add_y_position() 
  pb1<-ggboxplot(box,
                 x="group",
                 y="Expression",
                 color="black",
                 fill ="group",
                 ylab = "level",ylab.size=8,
                 add = "jitter", add.params = list(size=1.5,alpha=0.7),
                 bxp.errorbar.width = 2,
                 width = 0.55,
                 size=0.7,
                 font.label = list(size=12), 
                 palette = color)+
    theme(panel.background =element_blank())+
    theme(axis.line=element_line(colour="black"))+theme(axis.title.x = element_blank())+
    theme(axis.text.x = element_text(size = 12,angle = 30,vjust = 0.5,hjust = 0.5))+
    theme(axis.text.y = element_text(size = 8))+ggtitle(centenarians3[i])+theme(plot.title = element_text(hjust = 0.5,size=10,face="bold"))+
    theme(legend.position = "NA")+
    stat_pvalue_manual(stat.test, 
                     label = "{p}",
                     linetype = 1,
                     bracket.size = 0.3,
                     bracket.nudge.y = 0.00008,
                     bracket.shorten = 0.02,
                     step.increase = 0.04,
                     hide.ns = F,
                     size=3)
  plist[[i]]<-pb1
} 

library(ggpubr)
library(cowplot)

plist[[1]]
plist[[2]]
plist
plot1<-plot_grid(plist[[1]],plist[[2]],plist[[3]],plist[[4]],
                 plist[[5]],plist[[6]],plist[[7]],plist[[8]],
                 plist[[9]],plist[[10]],plist[[11]],plist[[12]],
                 plist[[13]],plist[[14]],plist[[15]],plist[[16]],
                 plist[[17]],plist[[18]],plist[[19]],plist[[20]],
                 plist[[21]],
                 ncol=4)
plot1
ggsave(filename = "plot1.pdf",width = 10,height = 24,units = "in",dpi = 1000)
