#' @title Generate cluster samples with lowest-level questionnaires
#' @description This is a subfunction of `cluster_gen` that performs cluster sampling where only the lowest-level individuals (e.g. students) fill out questionnaires.
#' @param n_levels number of cluster levels
#' @param c_mean_list list of `c_means` for each level
#' @param clusters numeric vector with the number of clusters on each level
#' @param n_obs numeric vector with the number of observations in each cluster
#' @param labels character vector with the names of each cluster level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @export
cluster_gen_together <- function(n_levels, c_mean_list, clusters, n_obs,
                                 labels, collapse, n_X, n_W, c_mean, ...) {
	sample <- list()  # will store all BG questionnaires
	level_combos <- list()  # will store ID combinations
    for (c in 1:n_levels) {
      level_combos[[n_levels - c + 1]] <- seq(from = 1, to = clusters[c])
    }
    id_combos <- expand.grid(level_combos)
    id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
    colnames(id_combos) <- labels

 in seq(ncol(id_combos))) {

                                     id_combos[, c] <- paste0(labels[c [, c])
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
        clusterID <- paste(rev(id_combos[c, ]), collapse = "_")
        cluster_bg$uniqueID <- paste(studentID, clusterID, sep = "_")

        # Relabeling the subjects
        # last_subject <- n_obs[l] * c
        # first_subject <- last_subject - n_obs[l] + 1
        # cluster_bg$subject <- first_subject:last_subject

        # Saving the questionnaire to the final list (sample)
        cluster_bg -> sample[[c]]
      }
      if (collapse) {
        sample <- do.call(rbind, sample)
        sample$subject <- seq(nrow(sample))
      }
	  return(sample)
}