.libPaths()

getwd()
setwd("E:/OneDrive_HNEE/01_study/03_Master_FIT/04_semester_4/Environmental_data_analysis/exam")
getwd()

folder_data = paste0(getwd(),"/00_data/Mehrfachdownload_rIYYX7_2VX73u/")
folder_result = paste0(getwd(),"/result/")

#### import file xyz
# get list of file
file.list = list.files(path = folder_data, pattern = ".xyz")

# import files to a list
data = lapply(paste0(folder_data,file.list), read.table)
names(data) = stringr::str_replace(file.list, pattern = ".xyz", replacement = "")

# edit
for (i in 1:length(data)) {
  colnames(data[[i]]) = c('x','y','z')
  data[[i]]$tile = names(data)[i]
}

# combine list of df to a single df
data.df = do.call("rbind", data)
data.df$tile_id = substr(data.df$tile,6,13)

# save final data
saveRDS(data.df, file = paste0(getwd(),"/00_data/data.rds"))
