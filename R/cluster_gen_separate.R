cluster_gen_separate <- function(n_levels, c_mean_list, clusters, n_obs,
                                 labels, collapse, n_X,
                                 n_W, c_mean, ...) {
  out    <- list()
	sample <- list()  # will store all BG questionnaires
    for (l in seq(n_levels)) {
      # TODO: allow custom c_mean for each cluster or only levels (implemented)? Idea for this: lists of lists (ideally, something simpler, though)

      # Adapting additional parameters to questionnaire_gen format
      if (class(c_mean_list) == "list") {
        c_mean <- c_mean_list[[l]]
      }

      level_label <- labels[l<- ifelse(test = l < n_levels,
                                 yes  = labels[l],
                                 no   = "subject")

      if (l > 1) {
        clusters[l] <- clusters[l] * clusters[l - 1]
        previousClusterID <- as.vector(sapply(sample[[l - 1]], function(x) x$clusterID))
      }
      for (c in 1:clusters[l]) {
        # Generating data
        cluster_bg <- questionnaire_gen(n_obs[l],
                                        n_X = n_X[[l]], n_W = n_W[[l]],
                                        c_mean = c_mean, verbose = FALSE,...)
        # Generating unique IDs
        respID <- paste0(next_level_label, seq(cluster_bg$subject))
        if (l > 1) {
          previous_c <- rep(seq(clusters[l] / clusters[l - 1]), clusters[l])[c]
          cluster_bg$clusterID <- paste0(level_label, previous_c, "_",
                                         previousClusterID[c])
        } else {
          cluster_bg$clusterID <- paste0(level_label, c)
        }
        cluster_bg$uniqueID <- paste(respID, cluster_bg$clusterID, sep = "_")

        # Saving the questionnaire to the final list (sample)
        cluster_bg -> sample[[level_label]][[c]]
      }
      if (collapse) {
        out[[level_label]] <- do.call(rbind, sample[[level_label]])
        out[[level_label]]["clusterID"] <- NULL
      } else {
        out <- sample
        out$clusterID <- NULL
      }
    }
	return(out)
}