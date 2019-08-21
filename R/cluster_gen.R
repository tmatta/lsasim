#' @title Generate cluster sample
#' @param n_obs numeric vector with the number of observations (clusters or subjects) on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param N numeric vector with the population size of each level
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @param separate_questionnaires if `TRUE`, each level will have its own questionnaire
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param verbose if `TRUE`, prints output messages
#' @details This function relies heavily in two subfunctions---`cluster_gen_separate` and `cluster_gen_together`---which can be called independently. This does not make `cluster_gen` a simple wrapper function, as it performs several operations prior to calling its subfunctions, such as randomly generating `n_X` and `n_W` if they are not determined by user.
#'   `n_obs` can have unitary length, in which case all clusters will have the same size.
#'   Regarding the additional parameters to be passed to `questionnaire_gen()`, they can be passed either in the same format as `questionnaire_gen()` or as more complex objects that contain information for each cluster level.
#' @export
cluster_gen <- function(n_obs, # TODO: ranges for sizes (not just fixed values)
                        cluster_labels = c("country", "school", "class")[seq(length(n_obs)) - 1],
                        resp_labels = c("principal", "teacher", "student")[seq(length(n_obs)) - 1],
                        n_X = NULL,
                        n_W = NULL,
                        c_mean = NULL,
                        # TODO: allow independent c_mean for each cluster or only levels (implemented)? Idea for this: lists of lists (kinda messy)
                        separate_questionnaires = TRUE,
                        collapse = "none",
                        N = n_obs,
                        sampling_method = "SRS",
                        # TODO: SRS for schools and students? If schools differ in size, this will result in equal weights for each school and different weights for students. Alternatively, use PPS for schools. Why not offer both alternatives?
                        # TODO: Replicate weights
                        # TODO: Control over inter-class correlation (intra-class handled by quest_gen?). Add correlations (within, between)
                        verbose = TRUE,
                        ...) {
  # Validation
  check_condition(!separate_questionnaires & length(n_X) > 1,
                  "Unique questionnaire requested. n_X must therefore be a scalar.")
  check_condition(!separate_questionnaires & length(n_W) > 1,
                  "Unique questionnaire requested. n_W must therefore be a scalar or a list.")
  check_condition(length(n_obs) == 1, "n_obs must have length larger than 1")
  check_condition(class(c_mean) == "list" & !separate_questionnaires,
                  "For unique questionnaires, c_mean must not be a list.")
  check_condition(length(n_obs) > length(cluster_labels) + 1,
                  "cluster_labels has insufficient length")
  check_condition(!separate_questionnaires & collapse == "partial",
                  "Partial collapsing unavailable for unique questionnaires")

  # Calculating useful arguments                
  n_levels <- length(n_obs)

  # Adapting additional parameters to questionnaire_gen format
  ## n_X and n_W
  if (n_levels > 1 & separate_questionnaires) {
    if (length(n_X) == 1) n_X <- rep(n_X, n_levels)
    if (length(n_W) == 1 & class(n_W) == "numeric") {
      n_W <- rep(n_W, n_levels)
    } else if (length(n_W) > 1 & class(n_W) == "list") {
      n_W <- rep(list(n_W), n_levels)
    }
  }

  if (separate_questionnaires) {  # questionnaires administered at all levels
    # Generates unique questionnaires for each level
    if (is.null(n_X)) {
      n_X <- list()
      for (l in seq(n_levels)) {
        n_X[[l]] <- rzeropois(1.5)  # a positive number of Xs
      }
    }
    if (is.null(n_W)) {
      n_W <- list()
      for (l in seq(n_levels)) {
        n_W[[l]] <- as.list(replicate(rzeropois(5), 2))  # all Ws are binary
      }
    }

    # Message explaining cluster scheme
    if (verbose) clusterMessage(n_obs, resp_labels, cluster_labels, n_levels, 1)

    sample <- cluster_gen_separate(n_levels, n_obs, N,
                                   cluster_labels, resp_labels, collapse,
                                   n_X, n_W, c_mean, ...)
  } else {  # questionnaires administered only at the bottom level
    if (verbose) clusterMessage(n_obs, resp_labels, cluster_labels, n_levels, 2)

    if (is.null(n_X)) n_X <- rzeropois(1.5)  # a positive number of Xs
    if (is.null(n_W)) n_W <- as.list(replicate(rzeropois(5), 2))  # all binary
    sample <- cluster_gen_together(n_levels, n_obs, N,
                                   cluster_labels, resp_labels, collapse,
                                   n_X, n_W, c_mean, ...)
  }
  return(sample)
}