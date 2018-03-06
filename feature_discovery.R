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

trials <- data.frame('sample_rate' =     c(2, 3, 3, 10, 10, 20),
                     'sub_path_frames' = c(66, 50, 166, 50, 50, 25),
                     'clusters' =        c(3, 5, 4, 4, 6, 4))

bic_tables <- lapply(1:nrow(trials), function(trial){
  print(paste("trial", trial))
  sub_path_frames <- trials$sub_path_frames[trial]
  sample_rate <- trials$sample_rate[trial]
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
  ld3 <<- clusterLongData3d(traj=tradjectory_data,
                           idAll=paste("I-",1:dim(tradjectory_data)[1],sep=""),
                           time=1:dim(tradjectory_data)[2],
                           varNames = c("rotated_xpos", "rotated_ypos"))
  clust <- kml3d(ld3, nbClusters = c(3, 4, 5, 6, 7), nbRedrawing = 100)
  
  # plot clusters vs x & y
  number_of_clusters <- trials$clusters[trial]
  
  # plot stats of one of the trialed cluster nb values vs time
  path <- file.path("results", paste("ds", sample_rate), paste("length", sub_path_frames), paste("clusters", number_of_clusters))
  dir.create(path, showWarnings = F, recursive = T)
  png(file.path(path, paste("tradj graph ", sample_rate, "_", sub_path_frames, "_", number_of_clusters, ".png", sep = "")),
      width = 1000,
      height = 700)
  plot(ld3, number_of_clusters, 
       #parTraj=parTRAJ(type="n"),
       parMean=parMEAN(pchPeriod = 10),
       main = paste(number_of_clusters, " Clusters, ", sub_path_frames, " Frames, Sampled 1/", sample_rate, sep = ""), addLegend = F)
  dev.off()
  
  all_worm_df <- lapply(1:length(all_worm_sub_paths), function(x) {
    all_worm_sub_paths[[x]]$sub_path_id <- x
    all_worm_sub_paths[[x]]$cluster <- ld3[paste('c', number_of_clusters, sep = "")][[1]]@clusters[x]
    return(all_worm_sub_paths[[x]])
  }) %>%
    do.call("rbind", .)
  
  
  for(cluster_number in 1:number_of_clusters){
    cluster_letter <- c('A', 'B', 'C', 'D', 'E', 'F', 'G', 'H', 'I')[cluster_number]
    
    cluster_paths <- all_worm_df %>%
      filter(cluster == cluster_letter) %>%
      mutate(cluster = ifelse(cluster == 'A', 'Ahead',
                                       ifelse(cluster == 'B', 'Ahead Slow',
                                              ifelse(cluster == 'C', 'Right', 
                                                     ifelse(cluster == 'D', 'Left', cluster)))))
    
    g <- ggplot(aes(x = rotated_xpos, y = rotated_ypos, group = sub_path_id), data = cluster_paths) +
      geom_path() +
      labs(title = paste("Cluster", cluster_paths$cluster[[1]], "-", cluster_number, "of", number_of_clusters),
           subtitle = paste(number_of_clusters, " Clusters, ", sub_path_frames, " Frames, Sampled 1/", sample_rate, sep = "")) +
      coord_cartesian(xlim = c(min(all_worm_df$rotated_xpos), max(all_worm_df$rotated_xpos)),
                      ylim = c(min(all_worm_df$rotated_ypos), max(all_worm_df$rotated_ypos)))
    
    print(g)
    ggsave(file.path(path, paste("cluster graph ", sample_rate, "_", sub_path_frames, "_", number_of_clusters, "_", cluster_letter, ".png", sep = "")), plot = g)
  }
  
  for(worm_name_ in unique(all_worm_df$worm_name)){
    worm_data <- all_worm_df %>%
      filter(worm_name == worm_name_) %>%
      mutate(cluster = ifelse(cluster == 'A', 'Ahead',
                              ifelse(cluster == 'B', 'Ahead Slow',
                                     ifelse(cluster == 'C', 'Right', 
                                            ifelse(cluster == 'D', 'Left', cluster)))))
    
    g <- ggplot(aes(x = xpos, y = ypos, group = sub_path_id, color = cluster), data = worm_data) +
      geom_path() +
      labs(title = paste("Clusters transitions through path -", worm_name_),
           subtitle = paste(number_of_clusters, " Clusters, ", sub_path_frames, " Frames, Sampled 1/", sample_rate, sep = ""))
    
    print(g)
    ggsave(file.path(path, paste("worm path graph ", sample_rate, "_", sub_path_frames, "_", number_of_clusters, "_", worm_name_, ".png", sep = "")), plot = g)
  }
  
  #time spent in each observed cluster
  time_per_state <- all_worm_df %>%
    group_by(worm_name, sub_path_id, cluster) %>%
    summarize() %>%
    group_by(worm_name, cluster) %>%
    summarize(n = n()) %>%
    mutate(fraction = n / sum(n)) %>%
    dplyr::select(-n) %>%
    spread(cluster, fraction, fill = 0)
  View(time_per_state)
  write.csv(time_per_state, row.names = F, file = file.path(path, "time_per_state.csv"))
  
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
    dplyr::select(-n) %>%
    spread(last_state, fraction, fill = 0) %>%
    arrange(cluster)
  
  View(transition_table)
  write.csv(transition_table, row.names = F, file = file.path(path, "observed_transition_table.csv"))
  
  
  hmm_list_file_path <- paste("hmm_lists_by_hidden_states", number_of_clusters, "movement_clusters",
                              sub_path_frames, "frames", sample_rate, "downsample.Rdata", sep = "_")
  if(file.exists(hmm_list_file_path)){
    load(hmm_list_file_path)
  } else {
    print("begining long simulation!!!!")
    #WARNING: The following will cook your cpu if its not great!
    #fit many HMM to each worms observed states
    hmm_lists_by_hidden_states <- mclapply(3:10, function(num_hidden_states){
      worm_hmm_lists <- mclapply(unique(all_worm_df$worm_name), function(a_worm_name){
        #list of observed states from a worm
        observed_states <- all_worm_df %>%
          group_by(worm_name, sub_path_id, cluster) %>%
          summarize() %>%
          filter(worm_name == a_worm_name) %>%
          ungroup() %>%
          dplyr::select(cluster)
        
        #fit several hmm's to the observed sequences
        hmm_list <- mclapply(1:7, function(x){
          hidden_states <- num_hidden_states
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
      
      return(worm_hmm_lists)
    }, mc.cores = as.integer((1 + detectCores()/13) * 2), mc.preschedule = F)
    
    save(hmm_lists_by_hidden_states, file = hmm_list_file_path)
  }
  
  # load the hmm_lists .Rdata files and run them through to get bic_tables
  bic_tables <- lapply(hmm_lists_by_hidden_states, function(hidden_state_data){
      foward_bic_loss_table <- mclapply(1:length(hidden_state_data), function(x){
        sapply(1:length(hidden_state_data), function(y){
          #Identify most likely sequence of hidden states for worm X
          base_prob <- sapply(hidden_state_data[[x]]$hmm_list, function(hmm){
            forward_probs <- forward(hmm, hidden_state_data[[x]]$observed_states$cluster)
            max(forward_probs[,ncol(forward_probs)])
          })
  
          num_hidden_states <- length(hidden_state_data[[x]]$hmm_list[[1]]$States)
          base_prob_bic <- log(length(hidden_state_data[[x]]$observed_states$cluster)) * num_hidden_states -
            2 * mean(base_prob)
  
          alt_prob <- sapply(hidden_state_data[[y]]$hmm_list, function(hmm){
            forward_probs <- forward(hmm, hidden_state_data[[x]]$observed_states$cluster)
            max(forward_probs[,ncol(forward_probs)])
          })
  
          alt_prob_bic <- log(length(hidden_state_data[[x]]$observed_states$cluster)) * num_hidden_states -
            2 * mean(alt_prob)
  
          loss <- alt_prob_bic
          return(loss)
        })
      }, mc.cores = detectCores())
      
      foward_bic_loss_table <- data.frame(foward_bic_loss_table)
      colnames(foward_bic_loss_table) <- paste(lapply(hidden_state_data, function(x) as.character(x$worm_name)), "states", sep = "_")
      foward_bic_loss_table$worm_model <- paste(lapply(hidden_state_data, function(x) as.character(x$worm_name)), "hmm", sep = "_")
      return(foward_bic_loss_table)
  })

  bic_table_combine <- mapply(function(x, n){
    x$hidden_states <- n
    return(x)
  }, bic_tables, sapply(hmm_lists_by_hidden_states, function(x) length(x[[1]]$hmm_list[[1]]$States)), SIMPLIFY = F) %>%
    do.call("rbind", .) %>%
    select(hidden_states, worm_model, N2_f1_states, N2_nf4_states, N2_nf5_states, tph1_f6_states)

  write.csv(bic_table_combine, row.names = F, file = file.path(path, "HMM_BIC_table_combine.csv"))
  
  return(bic_table_combine)
})


all_bic <- lapply(1:length(bic_tables), function(trial){
  bic_tables[[trial]]$trial <- trial
  return(bic_tables[[trial]])
}) %>% do.call("rbind", .)

special_stand <- function(x){
  (x - mean(x)) / sd(x)
}
percent_minus_min <- function(x){
  (x - min(x)) / min(x)
}
trials$trial_number <- 1:nrow(trials)
trials_to_keep <- trials %>%
  filter(trial_number == 4)
normalized_bic <- all_bic %>%
  filter(trial %in% trials_to_keep$trial_number) %>%
  group_by(hidden_states, trial) %>%
  mutate_if(is.numeric, percent_minus_min)
  #mutate_if(is.numeric, special_stand)

summary_bic <- normalized_bic %>%
  ungroup() %>%  
  select(-trial) %>%
  group_by(worm_model) %>%
  summarize_all(mean)

d <- as.matrix(summary_bic[,3:6])
min_val <- min(d[d!=0])
max_val <- max(d[d!=0])
d[d!=0] <- 1 - 2 * ((d[d!=0]-min_val)/(max_val - min_val))
rownames(d) <- summary_bic$worm_model


corrplot(d, is.corr = F,
         main = "-1 to 1 Rescaled Mean BIC",
         mar=c(0,0,1,0))


load("hmm_lists_by_hidden_states_4_movement_clusters_50_frames_10_downsample.Rdata")
models <- hmm_lists_by_hidden_states[[1]]
for(worm_model in models){
  hmm <- worm_model$hmm_list[[1]]
  
  hidden_states_edge_list <- lapply(0:(dim(hmm$transProbs)[1] * dim(hmm$transProbs)[2] - 1), function(index){
    data.frame('Source' = rownames(hmm$transProbs)[index %% dim(hmm$transProbs)[1] + 1],
               'Target' = colnames(hmm$transProbs)[index %/% dim(hmm$transProbs)[1] + 1],
               'Weight' = hmm$transProbs[index %% dim(hmm$transProbs)[1] + 1, index %/% dim(hmm$transProbs)[1] + 1])
  }) %>% do.call("rbind", .)
  transmision_edge_list <- lapply(0:(dim(hmm$emissionProbs)[1] * dim(hmm$emissionProbs)[2] - 1), function(index){
    data.frame('Source' = rownames(hmm$emissionProbs)[index %% dim(hmm$emissionProbs)[1] + 1],
               'Target' = colnames(hmm$emissionProbs)[index %/% dim(hmm$emissionProbs)[1] + 1],
               'Weight' = hmm$emissionProbs[index %% dim(hmm$emissionProbs)[1] + 1, index %/% dim(hmm$emissionProbs)[1] + 1])
  }) %>% do.call("rbind", .)
  
  write.csv(rbind(hidden_states_edge_list, transmision_edge_list), 
            file = paste(worm_model$worm_name, ".csv", sep = ""),
            row.names = F)
}

