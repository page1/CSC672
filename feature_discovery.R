if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr",
               "ggplot2",
               "readr",
               "corrplot",
               "purrr",
               "broom")

subfolders <- list.dirs(path = "data", full.names = TRUE, recursive = TRUE)
subfolders <- subfolders[-which(subfolders == 'data')]
data_files <- paste(subfolders, 'masterFile.csv', sep = .Platform$file.sep)
folders <- gsub('data/', '', subfolders, fixed = T)

data <- lapply(data_files, read.csv)
names(data) <- folders

cc <- data[[2]][order(data[[2]]$FrameNum),]
cc <- cc[seq(1, nrow(cc), 10),]
cc <- cc %>%
  mutate(xpos = CentroidX,
         ypos = CentroidY) %>%
  filter(xpos != lag(xpos) | ypos != lag(ypos))
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
  #print(d)
  print(paste(x, current_cluster, d[1][1]))
  rsq <- summary(lm(p_hist))$r.squared
  #print(rsq)
  
  #last_point <<- current_point
  #\p_hist <<- rbind(current_point, p_hist[seq(1, pmin(100, nrow(p_hist))),])
  
  if(d > 300 & rsq < .9 & nrow(p_hist) >= 300){#new clust
    current_cluster <<- current_cluster + 1
    last_point <<- current_point
    p_hist <<- current_point
    #return(d)
    return(current_cluster)
  } else {
    last_point <<- current_point
    p_hist <<- rbind(current_point, p_hist[seq(1, pmin(300, nrow(p_hist))),])
    #return(d)
    return(current_cluster)
  }
})

cc$clust <- as.factor(clust)
# clust_stats <- cc %>% group_by(clust) %>%
#   #do(tidy(summary(lm(ypos ~ xpos, .))$r.squared))
#   summarize(x_range = max(xpos) - min(xpos), 
#             y_range = max(ypos) - min(ypos),
#             n = n(),
#             delta_time = max(ElapsedTimeInLogFile) - min(ElapsedTimeInLogFile)) %>%
#   mutate(reach = sqrt(x_range^2 + y_range^2),
#          reach_rate = reach / delta_time,
#          pic_rate = delta_time / n) %>%
#   filter(n >= 10)
# 
# clust_stats$reach_group <- kmeans(clust_stats$reach_rate, centers=3, nstart=100)$cluster
# cc <- cc %>%
#   inner_join(clust_stats, by = c('clust' = 'clust'))
cc$every_other <- (as.numeric(clust) %% 2) == 0
ggplot(aes(x = xpos, y = ypos, group = clust, color = every_other), data = cc) +
  geom_path() +
  ggtitle("Worm Path\nSplit when R^2 < 0.9 &\nMin Distance from Rolling Centroid") +
  theme(title = element_text(size=20),
        axis.title = element_text(size=15))
