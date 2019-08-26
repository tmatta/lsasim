#' @title Generate cluster samples with lowest-level questionnaires
#' @description This is a subfunction of `cluster_gen` that performs cluster sampling where only the lowest-level individuals (e.g. students) fill out questionnaires.
#' @param n_levels number of cluster levels
#' @param n_obs numeric vector with the number of clusters on each level
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
cluster_gen_together <- function(n_levels, n_obs, N, sampling_method,
                                 cluster_labels, resp_labels, collapse, n_X, n_W, c_mean, ...) {
	sample <- list()  # will store all BG questionnaires
	level_combos <- list()  # will store ID combinations

  # Generating level label combinations
  if (class(n_obs) == "list") {
    n_combos <- sum(n_obs[[n_levels - 1]])
    second_last_level <- unlist(sapply(n_obs[[n_levels - 1]], seq))
    id_combos <- matrix(second_last_level, ncol = n_combos)
    for (row in 2:(n_levels - 1)) {
      id_level <- n_levels - row
      if (length(n_obs[[id_level]]) > 1 & all(n_obs[[id_level]] == 1)) {
        n_obs[[id_level]] <- sum(n_obs[[id_level]])
      }
      expanded_level <- as.vector(unlist(sapply(n_obs[[id_level]], seq)))
      expanded_level_col <- 1
      new_row <- matrix(ncol = n_combos)
      for (col in seq(n_combos)) {
        if (all(id_combos[, col] == 1)) {
          new_row[col] <- expanded_level[expanded_level_col]
          expanded_level_col <- expanded_level_col + 1
        } else {
          new_row[col] <- new_row[col - 1]
        }
      }
      id_combos <- rbind(id_combos, new_row)
    }
    id_combos <- t(id_combos)
  } else {
    for (l in 1:(n_levels - 1)) {
      level_combos[[n_levels - l]] <- seq(from = 1, to = n_obs[l])
    }
    id_combos <- expand.grid(level_combos)
  }
  id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
  colnames(id_combos) <- cluster_labels[-n_levels]
  for (l in seq(ncol(id_combos))) {
    id_combos[, l] <- paste0(cluster_labels[l], id_combos[, l])
  }
  num_questionnaires <- nrow(id_combos)
    for (l in seq(num_questionnaires)) {
      # Generating data
      if (class(n_obs) == "list") {
        cluster_bg <- questionnaire_gen(n_obs[[n_levels]][l],
                                        n_X = n_X, n_W = n_W,
                                        c_mean = c_mean, verbose = FALSE,...)
      } else {
         cluster_bg <- questionnaire_gen(n_obs[n_levels], n_X = n_X, n_W = n_W,
                                         c_mean = c_mean, verbose = FALSE,...)
      }

      # Adding weights
      # if (sampling_method == "SRS") {
      #   weight_name <- paste0(resp_labels[n_levels], ".weight")
      #   cluster_bg[weight_name] <-  N[n_levels] / n_obs[n_levels]
      # } else if (sampling_method == "PPS") {
      #   stop("PPS sampling method not yet implemented")
      # }
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