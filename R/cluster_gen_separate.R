cluster_gen_separate <- function(n_levels, c_mean_list, clusters, n_obs, cluster_labels, resp_labels, collapse, n_X, n_W, c_mean, ...) {
	sample <- list()  # will store all BG questionnaires
    for (l in seq(n_levels)) {
      # TODO: if n_X, n_W are not provided, use consistent values for each level?
      # TODO: allow custom c_mean for each cluster or only levels (implemented)?
      #   Idea for this: lists of lists (ideally, something simpler, though)

      # Adapting additional parameters to questionnaire_gen format
      if (class(c_mean_list) == "list") {
        c_mean <- c_mean_list[[l]]
      }

      level_label <- cluster_labels[l]
      resp_label <- resp_labels[l]
      if (l > 1) {
        clusters[l] <- clusters[l] * clusters[l - 1]
      } else {
        id_prefix <- ""
      }
      for (c in 1:clusters[l]) {
        # Generating data
        cluster_bg <- questionnaire_gen(n_obs[l], n_X = n_X[[l]], n_W = n_W[[l]],
                                        c_mean = c_mean, verbose = FALSE,...)
        # Generating unique IDs
        respID <- paste0(resp_label, seq(nrow(cluster_bg)))
        clusterID <- paste0(level_label, c)
        cluster_bg$uniqueID <- paste(respID, clusterID, sep = "_")

        # Relabeling the subjects
        last_subject <- n_obs[l] * c
        first_subject <- last_subject - n_obs[l] + 1
        cluster_bg$subject <- first_subject:last_subject

        # Saving the questionnaire to the final list (sample)
        cluster_bg -> sample[[level_label]][[c]]
      }
      if (collapse) sample[[level_label]] <- do.call(rbind, sample[[level_label]])
    }
	return(sample)
}