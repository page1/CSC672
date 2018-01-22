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
        
        file <- file.path('prelim', column_name, paste("boxplot_compare_", column_name, ".png", sep = ""))
        if(!file.exists(file)){
          prelim_timeplot <- ggplot(data=combined_columns, 
                                    aes(x = dataset, y = value)) + 
            geom_boxplot() +
            ggtitle(paste(column_name)) +
            ylab('Value') +
            xlab('Dataset')
          ggsave(file, plot = prelim_timeplot)
        }
      }
    },
    error = function(e) {print(paste('issue with', column_name, e))}
  )
}
#  This is Steve.  
#Scott was here
# Sarah is alive and the coolest girl in the group

##AG BEGIN##

##TIME##

#Convert Time Elapsed Variable to a DateTime Object for time series plotting
library(readr)
library(ggplot2)
N2_f1 <- read_csv("data/N2_f1.csv", col_types = cols(DirectionCode = col_factor(levels = c("1", "2")), 
        Posture = col_factor(levels = c("0", "1", "2", "3", "4")), 
        Kim = col_factor(levels = c("Backward-ReverseLong", "Backward-ReverseShort", "Forward-NTD", "Forward-Shallow", "Forward-Sharp", "Stopped-ReverseLong", "Stopped-ReverseShort", "Stopped-Stop"))))
rows <- 1:171403
dateString <- rep("2018-01-01 00:00:00",length(rows))
dateTime <- as.POSIXct(dateString)
dateTime <- dateTime + N2_f1$ElapsedTimeInLogFile + 0.0005
N2_f1 <- cbind(dateTime, N2_f1)
op <- options(digits.secs=3)
#options(op) to reset options

# Plot Histogram of Time Elapsed Per Frame
ggplot(data=N2_f1, aes(x= DeltaTimeInLogFile)) + geom_histogram(bins=400) + 
  ggtitle("Time Elapsed Per Frame (in Seconds)") +
  xlab("Elapsed Time") + ylab("Count") +
  scale_x_continuous(breaks=seq(0, 0.1, 0.01)) +
  scale_y_continuous(breaks=seq(0, 50000, 2500)) +
  coord_cartesian(xlim=c(0, 0.1)) +
  theme_light()

# Plot Boxplot of Time Elapsed Per Frame
ggplot(data=N2_f1, aes(x="", y= DeltaTimeInLogFile)) + geom_boxplot() + 
  ggtitle("Time Elapsed Per Frame (in Seconds)") +
  xlab("Time") + ylab("Seconds") + 
  #scale_y_continuous(breaks=seq(0, 0.4, 0.01)) 
  #scale_y_continuous(breaks=seq(0, 200000, 1000)) +
  theme_light()

#Plot Bar Graph of Delta Time by Time
ggplot(data=N2_f1, aes(x=dateTime, y=DeltaTimeInLogFile)) + geom_bar(stat="identity") +
  ggtitle("Time Elapsed Per Frame (in Seconds)") +
  xlab("Elapsed Time") + ylab("Time Per Frame") +
  scale_x_datetime(
    limits = c(
      as.POSIXct("2018-01-01 00:00:00 GMT"),
      as.POSIXct("2018-01-01 00:00:50 GMT")                 
                   )) +
  theme_light()

##POSTURES##

##AG END##






