if (!require("pacman")) install.packages("pacman")
pacman::p_load("dplyr",
               "ggplot2",
               "readr",
               "corrplot",
               "purrr",
               "broom",
               "Matrix",
               "kml3d",
               "tidyr",
               "HMM",
               "clue",
               "parallel")

subfolders <- list.dirs(path = "data", full.names = TRUE, recursive = TRUE)
subfolders <- subfolders[-which(subfolders == 'data')]
data_files <- paste(subfolders, 'masterFile.csv', sep = .Platform$file.sep)
folders <- gsub('data/', '', subfolders, fixed = T)

data <- lapply(data_files, read.csv)
names(data) <- folders

sub_path_frames <- 50
sample_rate <- 10
min_samples <- as.integer(min(sapply(data, nrow)) / sample_rate) # don't overweight worm with most examples

all_worm_sub_paths <- mapply(function(a_worms_data, worm_name){
  a_worms_data <- a_worms_data[order(a_worms_data$FrameNum),]
  a_worms_data_sampled <- a_worms_data[seq(1, nrow(a_worms_data), sample_rate),] %>%
    head(min_samples)
  
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
}, data, names(data))


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
clust <- kml3d(ld3, nbClusters = c(3, 4, 5, 6, 7), nbRedrawing = 20)

# plot clusters vs x & y
number_of_clusters <- 4

# plot stats of one of the trialed cluster nb values vs time
plot(ld3, number_of_clusters, main = paste("All Worms ", sub_path_frames, " Frames Sampled 1/", sample_rate, sep = ""), addLegend = F)


all_worm_df <- lapply(1:length(all_worm_sub_paths), function(x) {
  all_worm_sub_paths[[x]]$sub_path_id <- x
  all_worm_sub_paths[[x]]$cluster <- ld3[paste('c', number_of_clusters, sep = "")][[1]]@clusters[x]
  return(all_worm_sub_paths[[x]])
}) %>%
  do.call("rbind", .)

#time spent in each observed cluster
View(all_worm_df %>%
  group_by(worm_name, sub_path_id, cluster) %>%
  summarize() %>%
  group_by(worm_name, cluster) %>%
  summarize(n = n()) %>%
  mutate(fraction = n / sum(n)) %>%
  dplyr::select(-n) %>%
    spread(cluster, fraction, fill = 0))

#transitions between each observed cluster
transition_table <- all_worm_df %>%
  group_by(worm_name, sub_path_id, cluster) %>%
  summarize() %>%
  group_by(worm_name) %>%
  mutate(last_state = paste("Last", lag(cluster), sep = "_")) %>%
  group_by(worm_name, cluster, last_state) %>%
  summarize(n = n()) %>%
  group_by(worm_name, cluster) %>%
  mutate(fraction = n / sum(n)) %>%
  select(-n) %>%
  spread(last_state, fraction, fill = 0) %>%
  arrange(cluster)

View(transition_table)

#WARNING: The following will cook your cpu if its not great!
#fit many HMM to each worms observed states
worm_hmm_lists <- mclapply(unique(all_worm_df$worm_name), function(a_worm_name){
  #list of observed states from a worm
  observed_states <- all_worm_df %>%
    group_by(worm_name, sub_path_id, cluster) %>%
    summarize() %>%
    filter(worm_name == a_worm_name) %>%
    ungroup() %>%
    dplyr::select(cluster)
  
  #fit several hmm's to the observed sequences
  hmm_list <- mclapply(1:13, function(x){
    hidden_states <- 4
    emission_states <- number_of_clusters
    #random init of transition and emission tables
    transProbs <- matrix(runif(hidden_states * hidden_states), hidden_states)
    transProbs <- transProbs / rowSums(transProbs)
    emissionProbs <- matrix(runif(hidden_states * emission_states), hidden_states)
    emissionProbs <- emissionProbs / rowSums(emissionProbs)
    hmm <- initHMM(c("P", "Q", "R", "S", "T", "U", "V", "W", "X", "Y", "Z")[1:hidden_states],
                   c("A", "B", "C", "D", "E", "F", "G", "H", "I")[1:emission_states],
                   transProbs=transProbs,
                   emissionProbs=emissionProbs)
    #fit hmm with baumWelch
    baumWelch(hmm, observed_states$cluster, maxIterations = 20000, pseudoCount = .01)$hmm
  }, mc.cores = detectCores(), mc.preschedule = F)
  
  #return sets of fitted models for further ensembling
  return(list("worm_name" = a_worm_name,
    "observed_states" = observed_states,
    "hmm_list" = hmm_list))
}, mc.cores = detectCores(), mc.preschedule = F)

#Investigate dissimilarity of hidden states between pairwise worms
dissimilarity_table <- mclapply(1:length(worm_hmm_lists), function(x){
  sapply(1:length(worm_hmm_lists), function(y){
    #Identify most likely sequence of hidden states for worm X
    partitions <- lapply(worm_hmm_lists[[x]]$hmm_list, function(hmm){
      as.cl_partition(viterbi(hmm, worm_hmm_lists[[x]]$observed_states$cluster))
    })
    ensemble <- cl_ensemble(list = partitions)
    #ensemble the viterbi sequence of model X on dataset X
    x_consensus <- cl_consensus(ensemble, method = "soft/symdiff")
    
    partitions <- lapply(worm_hmm_lists[[y]]$hmm_list, function(hmm){
      as.cl_partition(viterbi(hmm, worm_hmm_lists[[x]]$observed_states$cluster))
    })
    ensemble <- cl_ensemble(list = partitions)
    #ensemble the viterbi sequence of model Y on dataset X
    y_consensus <- cl_consensus(ensemble, method = "soft/symdiff")
    
    #Compair the ensemble viterbi sequences
    cl_dissimilarity(x_consensus, y_consensus)
  })
}, mc.cores = detectCores())

dissimilarity_table <- data.frame(dissimilarity_table)
colnames(dissimilarity_table) <- paste(lapply(worm_hmm_lists, function(x) as.character(x$worm_name)), "states", sep = "_")
rownames(dissimilarity_table) <- paste(lapply(worm_hmm_lists, function(x) as.character(x$worm_name)), "hmm", sep = "_")
View(dissimilarity_table)

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
