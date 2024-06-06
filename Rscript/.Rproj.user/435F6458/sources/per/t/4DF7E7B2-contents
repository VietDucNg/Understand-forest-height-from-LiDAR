#### task 01, 02 ####

library(dplyr)
library(moments)
library(writexl)

metric.df = data.df%>% group_by(tile_id)%>%
  summarise(Mean = mean(z),
            SD = sd(z),
            Kurtosis = kurtosis(z),
            Skewness = skewness(z),
            Max = max(z),
            Min = min(z),
            Range = max(z)-min(z),
            Interquartile = IQR(z),
            quantile_5th = quantile(z, probs = 0.05),
            quantile_10th = quantile(z, probs = 0.10),
            quantile_15th = quantile(z, probs = 0.15),
            quantile_20th = quantile(z, probs = 0.20),
            quantile_25th = quantile(z, probs = 0.25),
            quantile_30th = quantile(z, probs = 0.30),
            quantile_35th = quantile(z, probs = 0.35),
            quantile_40th = quantile(z, probs = 0.40),
            quantile_45th = quantile(z, probs = 0.45),
            quantile_50th = quantile(z, probs = 0.50),
            quantile_55th = quantile(z, probs = 0.55),
            quantile_60th = quantile(z, probs = 0.60),
            quantile_65th = quantile(z, probs = 0.65),
            quantile_70th = quantile(z, probs = 0.70),
            quantile_75th = quantile(z, probs = 0.75),
            quantile_80th = quantile(z, probs = 0.80),
            quantile_85th = quantile(z, probs = 0.85),
            quantile_90th = quantile(z, probs = 0.90),
            quantile_95th = quantile(z, probs = 0.95))

# save
write_xlsx(metric.df, paste0(folder_result,"01_metric_table.xlsx"))


#### task 03 ####
library(ggplot2)

png(filename = paste0(folder_result,"03_boxplot.png"), 
    width = 1200, height = 1500, res = 200)
ggplot(data.df, aes(x=z, y=tile_id))+
  geom_boxplot() + theme_classic()+
  labs(x = "Height", y="Tile ID")
dev.off()