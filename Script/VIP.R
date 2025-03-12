rm(list = ls())


setwd("")

vip_data <- read.csv("",header=T,row.names=1)

if (!is.data.frame(vip_data)) {
  vip_data <- data.frame(vip_data)
}


plot(x = vip_data$VIP_Score, 
     y = 1:length(vip_data$Metabolite), 
     type = 'p', 
     pch = 20,   
     col = "black", 
     main = "Diff Metabolites by VIP Scores",
     xlab = "VIP Scores",
     ylab = "Metabolites", 
     ylim = c(0, length(vip_data$Metabolite) + 1)) 


mtext(text = vip_data$Metabolite, side = 2, at = 1:length(vip_data$Metabolite), las = 1)

#axis(2, at = 1:length(vip_data$VIP_Score), labels = vip_data$Metabolite, las = 1)

#abline(v = 1.0, col = "red", lty = 2)

