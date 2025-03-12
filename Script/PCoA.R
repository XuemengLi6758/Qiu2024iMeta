


library(ggplot2)
library(ape)
library(tidyverse)
library(multcomp)
library(extrafont)
library(patchwork)
library(vegan)
library(ggtern)
library(reshape2)
library(ggsci)
library(scales)
library(agricolae)

rm(list = ls())

dist_BC <- read.table(as.matrix(""))
pcoa <- pcoa(dist_BC,correction = "none", rn = NULL) 
groups <- read.table("",row.names = 1,header = T,sep = '\t',comment.char = '',check.names = F)
PC1 <- pcoa$vectors[,1]
PC2 <- pcoa$vectors[,2]
groups<-groups[match(rownames(pcoa$vectors),rownames(groups)),]
pcoadata <- data.frame(rownames(pcoa$vectors),
                       PC1,PC2,groups)
colnames(pcoadata) <- c("sample","PC1","PC2","group")

pcoadata$group <- factor(pcoadata$group,levels = c("","","",""))

#Adonis test
otu.adonis <- adonis(dist_BC~group,data = pcoadata,permutations = 999)
write.table(otu.adonis$aov.tab,'beta_bray-curtis_adonis.tsv',sep = '\t',quote = F)
#-------------------------------------------------------------------------------
#Boxplot
df <- pcoadata
df1 <- df %>% group_by(group) %>% summarise(Max = max(PC1))  
df2 <- df %>% group_by(group) %>% summarise(Max = max(PC2)) 
df1$Max <- df1$Max + max(df1$Max)*0.1
df2$Max <- df2$Max + max(df2$Max)*0.1

tuk1 <- aov(PC1~group,data = pcoadata) %>% 
  glht(alternative = 'two.sided',linfct = mcp(group = "Tukey")) %>% cld(alpah = 0.05)

tuk2 <- aov(PC2~group,data = pcoadata) %>% 
  glht(alternative = 'two.sided',linfct = mcp(group = "Tukey")) %>% cld(alpah = 0.05)

res <- data.frame(PC1 = tuk1$mcletters$Letters,PC2 = tuk2$mcletters$Letters,
  df1 = df1$Max,df2 = df2$Max,group = df1$group)


res$group <- factor(res$group,levels = c("","","",""))
#-------------------------------------------------------------------------
p1 <- ggplot(pcoadata, aes(PC1, PC2,colour = group)) +
  geom_point(aes(colour = group),size = 1)+
  labs(x = (floor(pcoa$values$Relative_eig[1]*100)) %>% 
         paste0("PCoA1 ( ", ., "%", " )"),
       y = (floor(pcoa$values$Relative_eig[2]*100)) %>% 
         paste0("PCoA2 ( ", ., "%", " )")) +
  scale_colour_manual(values = c("#ED0000FF","#E69F00","#00468BFF","#42B540FF")) +
  stat_ellipse(aes(PC1, PC2),level = 0.95)+
  theme(text = element_text(size = 15))+
  geom_vline(aes(xintercept = 0),linetype = "dotted")+
  geom_hline(aes(yintercept = 0),linetype = "dotted")+
  theme(panel.background = element_rect(fill = 'white', colour = 'black'),
        axis.title.x = element_text(colour = 'black', size = 15),
        axis.title.y = element_text(colour = 'black', size = 15),
        axis.text = element_text(colour = 'black',size = 12),
        legend.title = element_blank(),
        legend.key.height = unit(0.4,"cm"),
        legend.position = 'none',
        panel.grid = element_blank())
p1
#-----------------------------------------------------------------------------------
p2 <- ggplot(pcoadata,aes(group,PC1)) +
  geom_boxplot(aes(fill = group,alpha = 0.1),outlier.colour = NA)+
  scale_fill_manual(values = c("#ED0000FF","#E69F00","#00468BFF","#42B540FF"))+
  geom_text(data = res,aes(x = group,y = df1,label = PC1),
            size = 5,color = "black",fontface = "plain")+
  theme(panel.background = element_rect(fill='white',colour='black'),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color = 'black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_text(colour = 'black',size = 12,face = "plain"),
        axis.text.x = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())+coord_flip()
p2
#--------------------------------------------------------------------------------------
p3 <- ggplot(pcoadata,aes(group,PC2)) +
  geom_boxplot(aes(fill = group,alpha = 0.1),outlier.colour = NA) +
  scale_fill_manual(values = c("#ED0000FF","#E69F00","#00468BFF","#42B540FF"))+
  geom_text(data = res,aes(x = group,y = df2,label = PC2),
            size = 5,color = "black",fontface = "plain")+
  theme(panel.background = element_rect(fill = 'white',colour = 'black'),
        axis.ticks.length = unit(0.4,"lines"), 
        axis.ticks = element_line(color='black'),
        axis.line = element_line(colour = "black"), 
        axis.title.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.x = element_text(colour = 'black',size = 12,angle = 0,vjust = 0.5,hjust = 0.5,face = "plain"),
        axis.text.y = element_blank(),
        legend.position = "none",
        panel.grid = element_blank())
p3
p4 <- ggplot(pcoadata,aes(PC1, PC2))+
  geom_text(aes(x = -0.5,y = 0.6,
                label = paste("PERMANOVA:\ndf = ",otu.adonis$aov.tab$Df[1],"\nR2 = ",
                              round(otu.adonis$aov.tab$R2[1],4),"\np-value = ",
                              otu.adonis$aov.tab$`Pr(>F)`[1],
                              sep = "")),size = 2.8,family = "sans",fontface = 1)+
  theme_bw() + xlab(NULL) + ylab(NULL) +
  theme(panel.grid = element_blank(), 
        axis.title = element_blank(),
        axis.line = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

p <- p2+p4+p1+p3 + plot_layout(heights = c(1,4),widths = c(4,1),ncol = 2,nrow = 2)
p
ggsave(plot = p,".pdf",dpi = 400,width = 6,height = 5)