
rm(list = ls())
library(corrplot)
setwd("")

cordata <- read.csv("",header = T,row.names = 1)
cordata <- as.matrix(cordata)
pdata<-read.csv("",header = T,row.names = 1)
pdata <- as.matrix(pdata)

cordata2 <- matrix(ifelse(abs(cordata) <=0,0,cordata),nrow(cordata))
cordata2 <- matrix(ifelse(pdata>=0.05,0,cordata2),nrow(cordata))
colnames(cordata2) <- colnames(cordata)
rownames(cordata2) <- rownames(cordata)
cordata2
cordata2 <- as.matrix(cordata2)

pdata8 <- data.frame(y=rep(colnames(cordata2),each=),
                     x=rep(rownames(cordata2),7),
                     r=as.vector(cordata2),
                     p=as.vector(ifelse(pdata>=0.05,0,pdata)))
pdata8
write.csv(pdata8,file="pdata8.csv")

pdata9<-read.csv("pdata9.csv",header = T,row.names = 1)

library(ggplot2)
library(RColorBrewer)
ggplot()+
  geom_point(data = pdata9,aes(x, y, size = abs(-log10(p)),color=r))+
  scale_color_gradientn(colors=c(colorRampPalette(c("#7295B8","white"))(48),
                                 colorRampPalette(c("white","white"))(4),
                                 colorRampPalette(c("white","#EC7372"))(48)),
                        limit=c(-1,1))+
  scale_size_continuous(range = c(0,9),limits = c(0,45),breaks = seq(0,9,3))+
  scale_x_discrete(expand=c(0.2,0.2))+
  scale_y_discrete(expand=c(0.05,0.05))+
  geom_vline(aes(xintercept=seq(0.5,15.5,1)),color= "grey")+
  geom_hline(aes(yintercept=seq(0.5,10.5,1)),color= "grey")+
  theme_minimal()+
  theme(panel.grid = element_blank(), panel.background = element_blank(), 
        axis.ticks = element_blank(), legend.key = element_blank(), 
        axis.text.x = element_text(color="black",angle = 45, hjust = 1, vjust = 1),
        plot.title = element_text(hjust = 0.5)) +
  labs(size = '-log10(p)', color = 'r')


