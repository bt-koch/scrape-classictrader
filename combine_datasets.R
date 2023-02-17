rm(list = ls()); gc()
library(data.table)

# load csv files
data_old <- read.csv("kaggle_dataset.csv")
data_new <- read.csv("classictrader.csv")

# combine data, filter out duplicates
data <- rbind(data_old, data_new)
data <- unique(data)

# save data as csv
fwrite(data, file = "kaggle_dataset.csv")