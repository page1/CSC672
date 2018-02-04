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

cc <- data[[2]][order(data[[2]]$FrameNum),]
cc <- cc %>%
  mutate(xpos = TotalOffsetRows,
         ypos = TotalOffsetCols)
p_hist <- data.frame()
last_point <- c(0, 0)
current_cluster <- 1
clust <- sapply(1:nrow(cc), function(x){
  current_point <- cc[x,c('xpos', 'ypos')]
  if(nrow(p_hist) == 0){
    p_hist <<- current_point
  }
  cent <- colSums(p_hist) / nrow(p_hist)
  d <- dist(rbind(cent, current_point))
  print(paste(x, current_cluster, d[1][1]))
  if(d[1][1] > 400){#new clust
    #cent <<- current_point
    current_cluster <<- current_cluster + 1
    last_point <<- current_point
    p_hist <<- current_point
    #cent <<- current_point #jmp the center
    return(current_cluster)
  } else {
    if(!all(last_point == current_point)){
      #print(last_point)
      #print(current_point)
      #print(last_point == current_point)
      #cent <<- (20 * cent + current_point) / 21 #smooth the center motion
      last_point <<- current_point
      p_hist <<- rbind(p_hist, current_point)
    }
    
    return(current_cluster)
  }
})

cc$clust <- as.factor(clust)
clust_stats <- cc %>% group_by(clust) %>%
  summarize(x_range = max(xpos) - min(xpos), 
            y_range = max(ypos) - min(ypos),
            n = n()) %>%
  mutate(reach = sqrt(x_range^2 + y_range^2),
         reach_rate = reach / n )

clust_stats$reach_group <- kmeans(clust_stats$reach_rate, centers=3, nstart=100)$cluster
cc <- cc %>%
  inner_join(clust_stats, by = c('clust' = 'clust'))
ggplot(aes(x = xpos, y = ypos, group = clust, color = as.factor(reach_group)), data = cc) +
  geom_path()


