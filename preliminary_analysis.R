if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr",
               "ggplot2",
               "readr",
               "corrplot",
               "purrr")

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

# Overall Correlation Plot
cor_plot_data <- lapply(data, function(x) x[,1:91]) %>% 
  do.call('rbind', .) %>%
  discard(is.factor) %>%
  discard(is.character)

#easy to use column name groups
posture_column_names <- c("Elongation",
                          "Heywood",
                          "Hydraulic",
                          "IsLoop",
                          "LengthToPixels",
                          "MajorAxisLength",
                          "MinorAxisLength",
                          "Posture",
                          "SkelNumPixels",
                          "SkewerAngle",
                          "SktAmpRatio",
                          "SktCmptFactor",
                          "SktElgFactor",
                          "TrackAmplitude",
                          "TrackPeriod")

size_shape_column_names <- c("Area",
                "Fatness",
                "Length",
                "Perimeter",
                "Thickness")

size_shape_extended_column_names <- c("Area",
                                      "Fatness",
                                      "Length",
                                      "Perimeter",
                                      "Thickness",
                                      "CurvHead",
                                      "CurvTail",
                                      "MaxWidth")

trajectory_column_names <- c("InstantAccel",
                              "InstantVelocity",
                              "Range",
                              "VectorAngle")

na_inf_clean <- function(df){
  df <- df[complete.cases(df),]
  df <- df[is.finite(rowSums(df)),]
  return(df)
}

posture_data <- select(cor_plot_data, posture_column_names)
posture_data <- na_inf_clean(posture_data)
correlations <- cor(posture_data, use = "complete.obs")
corrplot(correlations,
         main = "Posture",
         mar=c(0,0,1.5,0))

size_and_shape_data <- select(cor_plot_data, size_shape_column_names)
size_and_shape_data <- na_inf_clean(size_and_shape_data)
correlations <- cor(size_and_shape_data)
corrplot(correlations,
         main = "Shape and Size",
         mar=c(0,0,1.5,0))

size_and_shape_extended_data <- select(cor_plot_data, size_shape_extended_column_names)
size_and_shape_extended_data <- na_inf_clean(size_and_shape_extended_data)
correlations <- cor(size_and_shape_extended_data)
corrplot(correlations,
         main = "Extended Shape and Size",
         mar=c(0,0,1.5,0))

trajectory_data <- select(cor_plot_data, trajectory_column_names)
trajectory_data <- na_inf_clean(trajectory_data)
correlations <- cor(trajectory_data)
corrplot(correlations,
         main = "Trajectory",
         mar=c(0,0,1.5,0))

trajectory_size_and_shape_extended_data <- select(cor_plot_data, c(trajectory_column_names, size_shape_extended_column_names))
trajectory_size_and_shape_extended_data <- na_inf_clean(trajectory_size_and_shape_extended_data)
correlations <- cor(trajectory_size_and_shape_extended_data)
corrplot(correlations,
         main = "Extended Shape and Size & Trajectory",
         mar=c(0,0,1.5,0))

trajectory_posture_data <- select(cor_plot_data, c(trajectory_column_names, posture_column_names))
trajectory_posture_data <- na_inf_clean(trajectory_posture_data)
correlations <- cor(trajectory_posture_data)
corrplot(correlations,
         main = "Trajectory & Posture",
         mar=c(0,0,1.5,0))

size_shape_extended_posture_data <- select(cor_plot_data, c(size_shape_extended_column_names, posture_column_names))
size_shape_extended_posture_data <- na_inf_clean(size_shape_extended_posture_data)
correlations <- cor(size_shape_extended_posture_data)
corrplot(correlations,
         main = "Extended Shape and Size & Posture",
         mar=c(0,0,1.5,0))

##AG BEGIN##

##TIME##

#Convert Time Elapsed Variable to a DateTime Object for time series plotting
N2_f1 <- read_csv("data/N2_f1/masterFile.csv", col_types = cols(DirectionCode = col_factor(levels = c("1", "2")), 
        Posture = col_factor(levels = c("1", "2", "3", "4")),
        IsLoop = col_factor(levels = c("0", "1")),
        SegStatus = col_factor(levels = c("Good", "Bad")),
        Kim = col_factor(levels = c("Backward-ReverseLong", "Backward-ReverseShort", "Forward-NTD", "Forward-Shallow", "Forward-Sharp", "Stopped-ReverseLong", "Stopped-ReverseShort", "Stopped-Stop"))))
rows <- 1:171403
dateString <- rep("2018-01-01 00:00:00",length(rows))
dateTime <- as.POSIXct(dateString)
dateTime <- dateTime + N2_f1$ElapsedTimeInLogFile + 0.0005
N2_f1 <- cbind(dateTime, N2_f1)
op <- options(digits.secs=3)
#options(op) to reset options
N2_f1_2 <- N2_f1[1:82468,]

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

# N2_nf4 Worm
N2_nf4 <- read_csv("data/N2_nf4/masterFile.csv", col_types = cols(DirectionCode = col_factor(levels = c("1", "2")), 
              Posture = col_factor(levels = c("1", "2", "3", "4")),
              IsLoop = col_factor(levels = c("0", "1")),
              SegStatus = col_factor(levels = c("Good", "Bad")),
              Kim = col_factor(levels = c("Backward-ReverseLong", "Backward-ReverseShort", "Forward-NTD", "Forward-Shallow", "Forward-Sharp", "Stopped-ReverseLong", "Stopped-ReverseShort", "Stopped-Stop"))))

rows <- 1:nrow(N2_nf4)
dateString <- rep("2018-01-01 00:00:00",length(rows))
dateTime <- as.POSIXct(dateString)
dateTime <- dateTime + N2_nf4$ElapsedTimeInLogFile + 0.0005
N2_nf4 <- cbind(dateTime, N2_nf4)
op <- options(digits.secs=3)
#options(op) to reset options

# N2_nf5 Worm
N2_nf5 <- read_csv("data/N2_nf5/masterFile.csv", col_types = cols(DirectionCode = col_factor(levels = c("1", "2")), 
              Posture = col_factor(levels = c("1", "2", "3", "4")), 
              IsLoop = col_factor(levels = c("0", "1")),
              SegStatus = col_factor(levels = c("Good", "Bad")),
              Kim = col_factor(levels = c("Backward-ReverseLong", "Backward-ReverseShort", "Forward", "Forward-Sharp", "Stop"))))

rows <- 1:nrow(N2_nf5)
dateString <- rep("2018-01-01 00:00:00",length(rows))
dateTime <- as.POSIXct(dateString)
dateTime <- dateTime + N2_nf5$ElapsedTimeInLogFile + 0.0005
N2_nf5 <- cbind(dateTime, N2_nf5)
op <- options(digits.secs=3)
#options(op) to reset options

# tph1_f6 Worm
tph1_f6 <- read_csv("data/tph1_f6/masterFile.csv", col_types = cols(DirectionCode = col_factor(levels = c("1", "2")), 
              Posture = col_factor(levels = c("1", "2", "3", "4")), 
              IsLoop = col_factor(levels = c("0", "1")),
              SegStatus = col_factor(levels = c("Good", "Bad")),
              Kim = col_factor(levels = c("Backward-ReverseLong", "Backward-ReverseShort", "Forward-NTD", "Forward-Shallow", "Forward-Sharp", "Stopped-ReverseLong", "Stopped-ReverseShort", "Stopped-Stop"))))

rows <- 1:nrow(tph1_f6)
dateString <- rep("2018-01-01 00:00:00",length(rows))
dateTime <- as.POSIXct(dateString)
dateTime <- dateTime + tph1_f6$ElapsedTimeInLogFile + 0.0005
tph1_f6 <- cbind(dateTime, tph1_f6)
op <- options(digits.secs=3)
#options(op) to reset options

# Plot Boxplot of Postures
ggplot(data=N2_nf4, aes(x=Posture, color=Posture, fill=Posture)) + geom_bar(aes(y=(..count..)/sum(..count..))) + 
  ggtitle("Postures") +
  xlab("Postures") + ylab("Counts") + 
  scale_y_continuous(labels=scales::percent) +
  #scale_y_continuous(breaks=seq(0, 80000, 5000)) +
  theme_light()

# Plot Bar Chart of Postures
ggplot(tph1_f6, aes(x= Posture)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Posture), stat="count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                 y= (..count..)/sum(..count..) ), stat= "count", vjust = -.5) +
  ggtitle("Posture Classification (tph1_f6)") +
  labs(y = "Relative Frequencies", fill="Posture") +
  scale_y_continuous(labels=scales::percent) +
  scale_x_discrete(labels=c("1" = "Non-Loop", "2" = "Delta",
                            "3" = "Omega", "4" = "Gamma"))

# Plot Bar Chart of Movements
ggplot(tph1_f6, aes(x= Kim)) + 
  geom_bar(aes(y = (..count..)/sum(..count..), fill = Kim), stat="count") +
  geom_text(aes(label = scales::percent((..count..)/sum(..count..)),
                y= (..count..)/sum(..count..) ), stat= "count", hjust = -.1) +
  ggtitle("Movement State Classification (tph1_f6)") +
  labs(y = "Relative Frequencies", fill="Movement State") +
  scale_y_continuous(labels=scales::percent) +
  coord_flip() +
  theme_light()

##AG END##






