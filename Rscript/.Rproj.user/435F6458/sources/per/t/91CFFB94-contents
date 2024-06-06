library(factoextra)

# select two first PC
pc = pca$x[,1:2]

# calculate how many clusters needed using sum of squares

png(filename = paste0(folder_result,"08_Kmeans/01_elbowPlot.png"),
    width = 600, height = 500, res = 120)
fviz_nbclust(pc, kmeans, method = "wss")+
  labs(subtitle="Elbow Method")
dev.off()

# Kmeans
set.seed(10)
kmeans.2 <- kmeans(pc, centers=2,nstart=100)
kmeans.3 <- kmeans(pc, centers=3,nstart=100)
kmeans.4 <- kmeans(pc, centers=4,nstart=100)
print(kmeans.2)

# Visualize the clustering algorithm results.
rownames(pc) = metric.df$tile_id

png(filename = paste0(folder_result,"08_Kmeans/02_2clusters.png"),
    width = 1000, height = 500, res = 150)
fviz_cluster(list(data=pc, cluster = kmeans.2$cluster))+
  theme_bw()+ ggtitle("(a)")+ xlim(-2,2)+ylim(-2,3)
dev.off()

png(filename = paste0(folder_result,"08_Kmeans/03_3clusters.png"),
    width = 1000, height = 500, res = 150)
fviz_cluster(list(data=pc, cluster = kmeans.3$cluster))+
  theme_bw()+ ggtitle("(b)")+ xlim(-2,2)+ylim(-2,3)
dev.off()

png(filename = paste0(folder_result,"08_Kmeans/04_4clusters.png"),
    width = 1000, height = 500, res = 150)
fviz_cluster(list(data=pc, cluster = kmeans.4$cluster))+
  theme_bw()+ ggtitle("(c)")+ xlim(-2,2)+ylim(-2,3)
dev.off()

# compare with box-plot
pc=cbind(pc, cluster=kmeans.3$cluster)
cluster.df = data.df

# create new column 'cluster' with corresponding tile
tile_id = unique(cluster.df$tile_id)
cluster = kmeans.3$cluster
cluster.df$cluster = cluster[match(cluster.df$tile_id, tile_id)]
cluster.df$cluster = as.character(cluster.df$cluster)

png(filename = paste0(folder_result,"/08_Kmeans/05_boxplot.png"), 
    width = 1200, height = 1500, res = 200)
ggplot(cluster.df, aes(x=z, y=tile_id, fill = cluster, alpha=0.5))+
  geom_boxplot()+
  scale_fill_manual(name="Cluster", 
                     values = c("red","green","blue"))+
  labs(x = "Height", y="Tile ID")+
  theme_classic()
dev.off()
