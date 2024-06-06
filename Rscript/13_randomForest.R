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
train$cluster = as.factor(train$cluster)

# load library
library(randomForest)
library(caret)

# randomForest function
# keep.forest = T: save the random forest output
# importance = TRUE: assess the importance of each variables
# proximity = TRUE: return the proximity matrix
# mtry = : the number of random variable at each split
# ntree = : the number of decision trees

## NOTE: If the thing we're trying to predict is a continuous number 
## (i.e. "weight" or "height"), then by default, randomForest() will set 
## "mtry", the number of variables to consider at each step, 
## to the total number of variables divided by 3 (rounded down), or to 1 
## (if the division results in a value less than 1).
## If the thing we're trying to predict is a "factor" (i.e. either "yes/no"
## or "ranked"), then randomForest() will set mtry to 
## the square root of the number of variables (rounded down to the next
## integer value).
## Also, by default random forest generates 500 trees

model.rf = randomForest(cluster ~ PC1 + PC2,
                        data = train,
                        keep.forest = T,
                        importance = T,
                        proximity = T)
model.rf

# visualize the out-of-bag error
# create a dataframe for visulizing the error rate
oob.df = data.frame(
  Tree=rep(1:nrow(model.rf$err.rate), times=4),
  Type = rep(c('OOB','Group 1',' Group 2','Group 3'), each=nrow(model.rf$err.rate)),
  Error=c(model.rf$err.rate[,'OOB'], model.rf$err.rate[,'1'],
          model.rf$err.rate[,'2'],model.rf$err.rate[,'3'])
)

png(filename = paste0(folder_result,"13_randomForest/01_oob.png"),
    width = 800, height = 500, res = 120)
ggplot(data=oob.df,aes(x=Tree, y=Error))+
  geom_line(aes(color=Type))+theme_bw()+ labs(x='Number of trees')+
  ylim(0,0.75)
dev.off()


# Now check to see if the random forest better with different parameters
# by comparing OOB value
# try different ntree of 200,400,600,800, and 1000
# try different mtry of 1, 2, 3, and 4
oob=c()
ntree = c(100,200,300,400,500)
for (tree in ntree) {
  for (nvariable in 1:2) {
    model.rf = randomForest(cluster ~ PC1 + PC2,
                            data = train,
                            keep.forest = T,
                            importance = TRUE,
                            proximity =T,
                            mtry = nvariable,
                            ntree = tree)
    print(paste0("ntree = ",tree))
    print(paste0('mtry = ',nvariable))
    oob[length(oob)+1] = model.rf$err.rate[nrow(model.rf$err.rate),1]
    oob[length(oob)]
  }
}

# adding colnames and rownnames for better visualization
oob = matrix(oob, ncol=2, byrow = T)
colnames(oob) = c('mtry = 1','mtry = 2')
rownames(oob) = c('ntree = 100','ntree = 200','ntree = 300','ntree = 400','ntree = 5000')

oob

# final random forest with chosen parameter of mtry = 3 and ntree = 600
model.rf = randomForest(cluster ~ PC1 + PC2,
                        data = train,
                        keep.forest = T,
                        importance = TRUE, 
                        mtry = 1,
                        ntree = 300)

model.rf

# tree nodes distributions
hist(treesize(model.rf))

# plot importance of each predictor variables
png(filename = paste0(folder_result,"13_randomForest/02_vaiableImportant.png"),
    width = 700, height = 500, res = 120)
varImpPlot(model.rf)
dev.off()

 # classify the test dataset
prediction<-predict(model.rf, test ,type = 'class')

# accuracy assessment
confusionMatrix(prediction, test$cluster)
