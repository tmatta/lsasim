cluster_gen_together <- function(n_levels, c_mean_list, clusters, n_obs, cluster_labels, resp_labels, collapse, n_X, n_W, c_mean, ...) {
	sample <- list()  # will store all BG questionnaires
	level_combos <- list()  # will store ID combinations
    for (c in 1:n_levels) {
      level_combos[[n_levels - c + 1]] <- seq(from = 1, to = clusters[c])
    }
    id_combos <- expand.grid(level_combos)
    id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
    colnames(id_combos) <- cluster_labels

    for (c in seq(ncol(id_combos))) {
      id_combos[, c] <- paste0(cluster_labels[c], id_combos[, c])
    }

    num_questionnaires <- nrow(id_combos)
      for (c in 1:num_questionnaires) {
        # Adapting additional parameters to questionnaire_gen format
        # if (class(c_mean_list) == "list") {
        #   browser()
        #   c_mean <- c_mean_list[[l]]
        # }

        # Generating data
        cluster_bg <- questionnaire_gen(sum(n_obs), n_X = n_X, n_W = n_W,
                                        c_mean = c_mean, verbose = FALSE,...)

        # Creating new ID variable

        studentID <- paste0("student", seq(nrow(cluster_bg)))
        clusterID <- paste(id_combos[c, ], collapse = "_")
        cluster_bg$uniqueID <- paste(studentID, clusterID, sep = "_")

        # Relabeling the subjects
        # last_subject <- n_obs[l] * c
        # first_subject <- last_subject - n_obs[l] + 1
        # cluster_bg$subject <- first_subject:last_subject

        # Saving the questionnaire to the final list (sample)
        cluster_bg -> sample[[c]]
      }
      if (collapse) sample <- do.call(rbind, sample)
	  return(sample)
}