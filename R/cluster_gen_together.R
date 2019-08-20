#' @title Generate cluster samples with lowest-level questionnaires
#' @description This is a subfunction of `cluster_gen` that performs cluster sampling where only the lowest-level individuals (e.g. students) fill out questionnaires.
#' @param n_levels number of cluster levels
#' @param n_obs numeric vector with the number of clusters on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param N numeric vector with the population size of each level
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @export
cluster_gen_together <- function(n_levels, n_obs, N,
                                 cluster_labels, resp_labels, collapse, n_X, n_W, c_mean, ...) {
	sample <- list()  # will store all BG questionnaires
	level_combos <- list()  # will store ID combinations

  for (c in 1:(n_levels - 1)) {
    level_combos[[n_levels - c]] <- seq(from = 1, to = n_obs[c])
  }

  id_combos <- expand.grid(level_combos)
  id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
  colnames(id_combos) <- cluster_labels[-n_levels]

  for (c in seq(ncol(id_combos))) {
    id_combos[, c] <- paste0(cluster_labels[c], id_combos[, c])
  }

  num_questionnaires <- nrow(id_combos)
    for (c in 1:num_questionnaires) {

      # Generating data
      cluster_bg <- questionnaire_gen(n_obs[n_levels], n_X = n_X, n_W = n_W,
                                      c_mean = c_mean, verbose = FALSE,...)
      # Adding weights
      cluster_bg$weight <-  N[n_levels] / n_obs[n_levels]

      # Creating new ID variable
      studentID <- paste0("student", seq(nrow(cluster_bg)))
      clusterID <- paste(rev(id_combos[c, ]), collapse = "_")
      cluster_bg$uniqueID <- paste(studentID, clusterID, sep = "_")

      # Saving the questionnaire to the final list (sample)
      cluster_bg -> sample[[c]]
    }

    if (collapse == "none") {
      names(sample) <- paste0(cluster_labels[n_levels - 1], seq(length(sample)))
    } else {
      sample <- do.call(rbind, sample)
      sample$subject <- seq(nrow(sample))
    }
  return(sample)
}