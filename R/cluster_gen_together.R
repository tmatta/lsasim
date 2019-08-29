#' @title Generate cluster samples with lowest-level questionnaires
#' @description This is a subfunction of `cluster_gen` that performs cluster sampling where only the lowest-level individuals (e.g. students) fill out questionnaires.
#' @param n_levels number of cluster levels
#' @param n numeric vector with the number of clusters on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param N numeric vector with the population size of each level
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @export
cluster_gen_together <- function(n_levels, n, N, sampling_method,
                                 cluster_labels, resp_labels, collapse, n_X, n_W, c_mean, ...) {
	sample <- list()  # will store all BG questionnaires

  # Generating level label combinations
  id_combos <- labelRespondents(n, cluster_labels)

  # Generating questionnaire data for lowest level
  num_questionnaires <- nrow(id_combos)
    for (l in seq(num_questionnaires)) {
      respondents <- ifelse(test = class(n) == "list",
                            yes  = n[[n_levels]][l],
                            no   = n[n_levels])
      cluster_bg <- questionnaire_gen(respondents,
                                        n_X = n_X, n_W = n_W,
                                        c_mean = c_mean, verbose = FALSE,...)

      # Adding weights
      if (sampling_method == "SRS") {
        weight_name <- paste0(resp_labels[n_levels], ".weight")
        cluster_bg[weight_name] <-  N[n_levels] / n_obs[n_levels]
      } else if (sampling_method == "PPS") {
        stop("PPS sampling method not yet implemented")
      }
      # TODO: conditional probabilities must be considered

      # Creating new ID variable
      studentID <- paste0("student", seq(nrow(cluster_bg)))
      clusterID <- paste(rev(id_combos[l, ]), collapse = "_")
      cluster_bg$uniqueID <- paste(studentID, clusterID, sep = "_")

      # Saving the questionnaire to the final list (sample)
      cluster_bg -> sample[[l]]
    }

    if (collapse == "none") {
      names(sample) <- paste0(cluster_labels[n_levels - 1], seq(length(sample)))
    } else {
      sample <- do.call(rbind, sample)
      sample$subject <- seq(nrow(sample))
    }
  return(sample)
}