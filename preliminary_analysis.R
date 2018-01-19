if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr",
               "ggplot2")

subfolders <- list.dirs(path = "data", full.names = TRUE, recursive = TRUE)
subfolders <- subfolders[-which(subfolders == 'data')]
data_files <- paste(subfolders, 'masterFile.csv', sep = .Platform$file.sep)
folders <- gsub('data/', '', subfolders, fixed = T)

data <- lapply(data_files, read.csv)
names(data) <- folders

column_names <- unique(unlist(lapply(data, names)))

# Gives errors on: RectBigSide, RectRatio, SegStatus, Kim
for(column_name in column_names){
  dir.create(file.path('prelim', column_name), recursive = T, showWarnings = F)
  print(column_name)
  combined_columns <- lapply(names(data), function(dataset_name){
    data.frame('dataset' = dataset_name, 
               'value' = data[[dataset_name]][[column_name]])
    
  }) %>% do.call('rbind', .) %>%
    filter(!is.null(value))
  tryCatch({
      prelim_hist <- ggplot(data=combined_columns, 
                            aes(x = value, y = ..density.., colour = dataset)) + 
        geom_freqpoly() +
        ggtitle(paste(column_name))
      ggsave(file.path('prelim', column_name, paste("density_compare_", column_name, ".png", sep = "")), plot = prelim_hist)
    },
    error = function(e) {print(paste('issue with', column_name, e))}
  )
}
