library(rpart)
library(rpart.plot)

# select two first PC
pc = pca$x[,1:2]
pc.df = as.data.frame(pc)
pc.df$cluster = kmeans.3$cluster
pc.df$tile_id = metric.df$tile_id

# Set random seed to make results reproducible:
set.seed(200)
# split data into two 80% for training and 20% for testing
sample <- sample(2, nrow(pc.df), replace=T, prob=c(0.8,0.2))
train<- pc.df[sample==1,]
test<- pc.df[sample==2,]
nrow(train)
nrow(test)

# convert class to factor
train$cluter = as.factor(train$cluster)

# decision tree
# minbucket: min number of observations in leaf notes
# cp: complextity parameters
decision_tree = rpart(cluster ~ PC1 + PC2, data = train,
                      control =rpart.control(minsplit =1,minbucket=1, cp=0))

# plot decision tree
png(filename = paste0(folder_result,"11_decisionTree/01_decisionTree.png"),
    width = 600, height = 500, res = 120)
prp(decision_tree, space = 2, split.cex = 1.5, nn.border.col = 1)
dev.off()

rpart.plot(decision_tree)
