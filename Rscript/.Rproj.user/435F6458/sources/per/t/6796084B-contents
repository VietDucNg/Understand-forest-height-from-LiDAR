library(ggplot2)

# PCA
# by default, prcomp() expects the samples to be rows 
# and the attributes to be columns
pca = prcomp(metric.df[2:28], center = TRUE, scale. = TRUE)

# x contains the pCs for drawing a graph
# use first two columns (PCs) in x to draw a 2D plot
plot(pca$x[,1], pca$x[,2])

# calculate how much variation each PC accounts for
# by using the square of standard deviation
pca.var <- pca$sdev^2 # value
pca.var.per <- round(pca.var/sum(pca.var)*100, 1) # percentage

## make a scree plot
png(filename = paste(folder_result,"screePlot.png"), 
    height = 500, width = 700, res = 100)
barplot(pca.var.per,ylab="Percent Variation" )
title(xlab="Principal Component",main="Scree Plot", line = 1)
dev.off()


## now make a fancy looking plot that shows the PCs and the variation:
pca.df <- data.frame(tile_id=metric.df$tile_id,
                     X=pca$x[,1],
                     Y=pca$x[,2])
pca.df

png(filename = paste(folder_result,"PCAplot.png"), 
    height = 700, width = 900, res = 150)
ggplot(data=pca.df, aes(x=X, y=Y, label=tile_id)) +
  geom_point(color="blue")+
  geom_text(aes(vjust=1)) +
  xlab(paste("PC1 - ", pca.var.per[1], "%", sep="")) +
  ylab(paste("PC2 - ", pca.var.per[2], "%", sep="")) +
  theme_bw() +
  ggtitle("PCA plot")+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-8.5,8.5)
dev.off()

# using loading scores to see which metrics have the largest effect
# on where samples are plotted in the PCA plot
# or contribute most to pc1.
# prcomp() function calls the loading scores rotation
# loading scores for PC1
loading_scores <- pca$rotation[,1]
# metrics that push sampes to the left side of the graph will have
# large negative values and variables that push samples to the right will have
# large positive values
variable_scores <- abs(loading_scores) ## get the magnitudes, both side of variables
variable_score_ranked <- sort(variable_scores, decreasing=TRUE)
top_variables <- names(variable_score_ranked)

top_variables

pca$rotation[top_variables,1] ## show the scores (and +/- sign)

# loading scores for PC2
loading_scores2 <- pca$rotation[,2]
# variables thta push sampes to the left side of the graph will have
# large negative values and variables that push samples to the right will have
# large positive values
variable_scores2 <- abs(loading_scores2) ## get the magnitudes, both side of variables
variable_score_ranked2 <- sort(variable_scores2, decreasing=TRUE)
top_variables2 <- names(variable_score_ranked2)

top_variables2

pca$rotation[top_variables2,1] ## show the scores (and +/- sign)


#### biplots ####
# pc1-pc2
library(ggbiplot)
png(filename = paste0(folder_result,"biplot_pc1pc2.png"),
    height = 700, width = 900, res = 150)
ggbiplot(pca, labels = metric.df$tile_id,obs.scale = 1, var.scale = 1)+
  geom_point(color="blue")+
  aes(vjust=1)+
  ggtitle('PC1 - PC2')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  xlim(-8.5,8.5)+
  ylim(-4,6)
dev.off()

# pc2-pc3
png(filename = paste0(folder_result,"biplot_pc2pc3.png"),
    height = 700, width = 900, res = 150)
ggbiplot(pca, labels = metric.df$tile_id,choices = c(2,3),obs.scale = 1, var.scale = 1)+
  geom_point(color='blue')+
  aes(vjust=1)+
  ggtitle('PC2 - PC3')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-2,2)+
  xlim(-4,5.5)
dev.off()

# pc1-pc3
png(filename = paste0(folder_result,"biplot_pc1pc3.png"),
    height = 700, width = 900, res = 150)
ggbiplot(pca, labels = metric.df$tile_id,choices = c(1,3),obs.scale = 1, var.scale = 1)+
  geom_point(color='blue')+
  aes(vjust=1)+
  ggtitle('PC1 - PC3')+
  theme_bw()+
  theme(plot.title = element_text(hjust = 0.5))+
  ylim(-3,3)+
  xlim(-8.5,8.5)
dev.off()
