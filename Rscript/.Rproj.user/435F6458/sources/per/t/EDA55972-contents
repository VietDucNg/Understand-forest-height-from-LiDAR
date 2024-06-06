metric.df2 = metric.df[metric.df$tile_id=='634_5608'|metric.df$tile_id=='637_5609',]

pca2 = prcomp(metric.df2[2:28], center = TRUE, scale. = TRUE)

plot(pca2$x[,1], pca2$x[,2])

pc2 = pca2$x[,1:2]
pc.df2 = as.data.frame(pc)



prediction2 = predict(model.rf, pc.df2, type='class')
