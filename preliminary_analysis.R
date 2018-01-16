if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr")

subfolders <- list.dirs(path = "data", full.names = TRUE, recursive = TRUE)
subfolders <- subfolders[-which(subfolders == 'data')]
data_files <- paste(subfolders, 'masterFile.csv', sep = .Platform$file.sep)

data <- lapply(data_files, read.csv)
