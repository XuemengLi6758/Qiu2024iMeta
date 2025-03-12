rm(list = ls())


setwd("")

data <- read.csv("",header=T,row.names=1)
mydata <- as.matrix(data)
head(mydata)  

#install.packages("devtools")
library(devtools)
library(usethis)
#install_github("jokergoo/ComplexHeatmap")
library(grid)
library(ComplexHeatmap)

Heatmap(mydata)

library(circlize)
col_fun <- colorRamp2(c(0.5, 0, -0.5),c("#FF0000","#FFFFFF","#0000FF"))
#col_fun <- colorRamp2(c(1, 0, -1),c("#D73027","#FBFBC0","#4575B4"))

Heatmap(mydata, 
        col = col_fun,
        rect_gp = gpar(col = "white", lwd = 1),
        #column_dend_height = unit(2, "cm"), 
        #row_dend_width = unit(2, "cm"),
        cluster_rows = FALSE,
        cluster_columns = TRUE, 
        #row_names_gp = gpar(fontsize = 8),
        column_names_gp= gpar(fontsize = 8),column_names_rot = 0,column_names_centered = TRUE,
        cell_fun = function(j, i, x, y, width, height, fill) {
          grid.text(sprintf("%.2f", mydata[i, j]), x, y, gp = gpar(fontsize = 5))})


p_data <- read.csv("",header=T,row.names=1)
p_data <- as.matrix(p_data)

Heatmap(mydata, 
        col = col_fun,
        rect_gp = gpar(col = "white", lwd = 1),
        #column_dend_height = unit(2, "cm"), 
        #row_dend_width = unit(2, "cm"),
        cluster_rows = FALSE,
        cluster_columns = TRUE, 
        row_names_gp = gpar(fontsize = 12),
        column_names_gp = gpar(fontsize = 12),column_names_rot = 30,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if (p_data[i,j] < 0.001) {
            grid.text(sprintf("***", data[i, j]), x, y, gp = gpar(fontsize = 8))
          } else if (p_data[i,j]<0.01) {
            grid.text(sprintf("**", data[i, j]), x, y, gp = gpar(fontsize = 8))
          } else if (p_data[i,j]<0.05) {
            grid.text(sprintf("*", data[i, j]), x, y, gp = gpar(fontsize = 8))
          } else {
            grid.text(sprintf("", data[i, j]), x, y, gp = gpar(fontsize = 8))
          }
        })


Heatmap(data, 
        col = col_fun,
        rect_gp = gpar(col = "white", lwd = 1),
        #column_dend_height = unit(2, "cm"), 
        #row_dend_width = unit(2, "cm"),
        cluster_rows = FALSE,
        cluster_columns = TRUE, 
        row_names_gp = gpar(fontsize = 12),
        column_names_gp = gpar(fontsize = 12),column_names_rot = 30,
        cell_fun = function(j, i, x, y, width, height, fill) {
          if (p_data[i,j] < 0.001) {
            grid.text(sprintf("***", data[i, j]), x, y, gp = gpar(fontsize = 16))
          } else if (p_data[i,j]<0.01) {
            grid.text(sprintf("**", data[i, j]), x, y, gp = gpar(fontsize = 16))
          } else if (p_data[i,j]<0.05) {
            grid.text(sprintf("*", data[i, j]), x, y, gp = gpar(fontsize = 16))
          } else {
            grid.text(sprintf("", data[i, j]), x, y, gp = gpar(fontsize = 16))
          }
        },show_heatmap_legend = FALSE,
        heatmap_legend_param = list(col_fun = col_fun, 
                                    at = c(0.5,0,-0.5),
                                    #labels = c("low", "zero", "high"),
                                    title = "Spearman",
                                    legend_height = unit(2, "cm"),
                                    title_position = "topcenter",
                                    title_gp = gpar(fontsize = 5),
                                    labels_gp = gpar(fontsize = 5),
                                    direction = "horizontal",
                                    grid_height = unit(4, "mm")))


lgd <- Legend(col_fun = col_fun, 
              at = c(-0.5, 0, 0.5),
              # labels = c("low", "zero", "high"),
              title = "Spearman",
              legend_height = unit(2, "cm"),
              title_position = "topcenter",
              title_gp = gpar(fontsize = 10),
              labels_gp = gpar(fontsize = 8),
              direction = "vertical", 
              grid_height = unit(4, "mm"))


draw(lgd, x = unit(0.98, "npc"), y = unit(0.90, "npc"))

dev.off()
