rm(list = ls()); gc()
library(data.table)

# load csv files
data_old <- read.csv("classictrader.csv")
data_new <- read.csv("classictrader_newdata.csv")

# combine data, filter out duplicates
data <- rbind(data_old, data_new)
data <- unique(data)

# save data as csv
fwrite(data, file = "kaggle_dataset.csv")