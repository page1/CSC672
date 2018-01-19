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
for(column_name in column_names){#column_names
  dir.create(file.path('prelim', column_name), recursive = T, showWarnings = F)
  print(column_name)
  combined_columns <- lapply(names(data), function(dataset_name){
    data.frame('dataset' = dataset_name, 
               'time' = data[[dataset_name]]$ElapsedTimeInVideo,
               'value' = data[[dataset_name]][[column_name]])
    
  }) %>% do.call('rbind', .) %>%
    filter(!is.null(value))
  tryCatch({
      if(column_name %in% c('RectBigSide', 'RectRatio', 'SegStatus', 'Kim')){
        #do special things
      } else {
        file <- file.path('prelim', column_name, paste("density_compare_", column_name, ".png", sep = ""))
        if(!file.exists(file)){
          prelim_hist <- ggplot(data=combined_columns, 
                                aes(x = value, colour = dataset)) + 
            geom_density() +
            ggtitle(paste(column_name)) +
            ylab('Density') +
            xlab('Value')
          ggsave(file, plot = prelim_hist)
        }
        
        file <- file.path('prelim', column_name, paste("scaled_density_compare_", column_name, ".png", sep = ""))
        if(!file.exists(file)){
          prelim_hist <- ggplot(data=combined_columns, 
                                aes(x = value, y = ..scaled.., colour = dataset)) + 
            geom_density() +
            ggtitle(paste(column_name)) +
            ylab('Scaled Density') +
            xlab('Value')
          ggsave(file, plot = prelim_hist)
        }
        
        file <- file.path('prelim', column_name, paste("timeplot_compare_", column_name, ".png", sep = ""))
        if(!file.exists(file)){
          prelim_timeplot <- ggplot(data=combined_columns, 
                                aes(x = time, y = value, colour = dataset)) + 
            geom_line() +
            ggtitle(paste(column_name)) +
            ylab('Value') +
            xlab('Time')
          ggsave(file, plot = prelim_timeplot)
        }
      }
    },
    error = function(e) {print(paste('issue with', column_name, e))}
  )
}
