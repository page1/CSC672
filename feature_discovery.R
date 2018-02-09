if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr",
               "ggplot2",
               "readr",
               "corrplot",
               "purrr",
               "broom",
               "Matrix",
               "kml3d")

subfolders <- list.dirs(path = "data", full.names = TRUE, recursive = TRUE)
subfolders <- subfolders[-which(subfolders == 'data')]
data_files <- paste(subfolders, 'masterFile.csv', sep = .Platform$file.sep)
folders <- gsub('data/', '', subfolders, fixed = T)

data <- lapply(data_files, read.csv)
names(data) <- folders

sub_path_frames <- 50

all_worm_sub_paths <- mapply(function(a_worms_data, worm_name){
  a_worms_data <- a_worms_data[order(a_worms_data$FrameNum),]
  a_worms_data_sampled <- a_worms_data[seq(1, nrow(a_worms_data), 10),]
  
  a_worms_data_sampled <- a_worms_data_sampled %>%
    mutate(xpos = CentroidX,
           ypos = CentroidY) %>%
    head((nrow(a_worms_data_sampled) %/% sub_path_frames) * sub_path_frames)
  
  sub_paths <- lapply(seq(1, nrow(a_worms_data_sampled), sub_path_frames), function(x){
    sub_path <- a_worms_data_sampled[x:(x+sub_path_frames-1), c('xpos', 'ypos')]
    #shift path so first point is at 0,0
    first_point <- sub_path[1,]
    sub_path_centered <- apply(sub_path, 1, function(x) x - first_point) %>% do.call("rbind", .)

    #rotate path so last point falls on x axis y = 0
    last_point <- sub_path_centered[nrow(sub_path_centered),]
    
    theta <- atan(last_point$ypos/last_point$xpos)
    rotation_matrix <- matrix(c(cos(theta), -sin(theta), sin(theta), cos(theta)),
                              ncol = 2,
                              nrow = 2,
                              byrow = T)
    
    rotated_sub_path <-  as.matrix(sub_path_centered) %*% rotation_matrix
    
    if(sign(last_point$xpos) == -1){
      half_rotate <- matrix(c(-1, 0, 0, -1),
                            ncol = 2,
                            nrow = 2,
                            byrow = T)
      rotated_sub_path <- as.matrix(rotated_sub_path) %*% half_rotate
    }
    
    rotated_sub_path <- as.data.frame(rotated_sub_path)
    colnames(rotated_sub_path) <- colnames(sub_path)

    #plot(as.integer(rotated_sub_path$xpos), as.integer(rotated_sub_path$ypos))
    
    #path now starts at 0,0 and ends on x axis where y = 0
    return(data.frame(worm_name = worm_name,
                      xpos = sub_path$xpos,
                      ypos = sub_path$ypos,
                      rotated_xpos = rotated_sub_path$xpos,
                      rotated_ypos = rotated_sub_path$ypos))
  })
  
  return(sub_paths)
}, data, names(data)) %>% flatten()


#we need to get data into [sub_path, time, variable] matrix
tradjectory_data <- sapply(1:length(all_worm_sub_paths), function(row_id){
  matrix(c(all_worm_sub_paths[[row_id]]$rotated_xpos, all_worm_sub_paths[[row_id]]$rotated_ypos),
         ncol = 2,
         byrow = F)
}, simplify = "array") %>%
  aperm(c(3,1,2))

# this data structure is needed for kml3d
ld3 <- clusterLongData3d(traj=tradjectory_data,
                         idAll=paste("I-",1:dim(tradjectory_data)[1],sep=""),
                         time=1:dim(tradjectory_data)[2],
                         varNames = c("rotated_xpos", "rotated_ypos"))
clust <- kml3d(ld3, nbClusters = c(3, 5, 7, 9), nbRedrawing = 200)

# plot stats of one of the trialed cluster nb values vs time
plot(ld3, 7, main = paste("All Worms", sub_path_frames, "Frames Sampled 1/10"))

# plot clusters vs x & y - irrespective of time
number_of_clusters <- 7

all_worm_df <- lapply(1:length(all_worm_sub_paths), function(x) {
  all_worm_sub_paths[[x]]$sub_path_id <- x
  all_worm_sub_paths[[x]]$cluster <- ld3[paste('c', number_of_clusters, sep = "")][[1]]@clusters[x]
  return(all_worm_sub_paths[[x]])
}) %>%
  do.call("rbind", .)

for(cluster_number in 1:number_of_clusters){
  cluster_letter <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')[cluster_number]
  
  cluster_paths <- all_worm_df %>%
    filter(cluster == cluster_letter)
  
  g <- ggplot(aes(x = rotated_xpos, y = rotated_ypos, group = sub_path_id, color = sub_path_id), data = cluster_paths) +
    geom_path() +
    ggtitle(paste("Cluster", cluster_letter, "-", cluster_number, "of", number_of_clusters))
  
  print(g)
}

for(worm_name_ in unique(all_worm_df$worm_name)){
  g <- ggplot(aes(x = xpos, y = ypos, group = sub_path_id, color = cluster), data = filter(all_worm_df, worm_name == worm_name_)) +
    geom_path() +
    ggtitle(paste("Clusters transitions through path -", worm_name_))
  
  print(g)
}

