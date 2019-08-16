#' @title Generate cluster sample
#' @param clusters numeric vector with the number of clusters on each level
#' @param n_obs numeric vector with the number of observations in each cluster
#' @param labels character vector with the names of each cluster level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @param separate_questionnaires if `TRUE`, each level will have its own questionnaire
#' @details This function relies heavily in two subfunctions---`cluster_gen_separate` and `cluster_gen_together`---which can be called independently. This does not make `cluster_gen` a simple wrapper function, as it performs several operations prior to calling its subfunctions, such as randomly generating `n_X` and `n_W` if they are not determined by user.
#'   `n_obs` can have unitary length, in which case all clusters will have the same size.
#'   Regarding the additional parameters to be passed to `questionnaire_gen()`, they can be passed either in the same format as `questionnaire_gen()` or as more complex objects that contain information for each cluster level.
#' @examples
#' cluster_gen(c(1, 2), collapse = FALSE)
#' cluster_gen(c(4, 2), n_X = 1, n_W = list(2, 3), theta = TRUE,
#'             c_mean = list(0, c(0, 10)))
#' @export
cluster_gen <- function(clusters,
                        n_obs = 5,  # TODO: allow levels with different sizes.
                        # TODO: incorporate n_obs into clusters
                        # TODO: set ranges for class sizes (not just fized values)
                        labels = c("country", "school", "class"),
                        collapse = FALSE,
                        n_X = NULL,
                        n_W = NULL,
                        c_mean = 0,
                        separate_questionnaires = TRUE,
                        # TODO: add design weights. Have "pop_size" (N) or total clusters (N_clusters) as argument and calculate weights as a function of that. Otherwise, 1 (SRS).
                        # TODO: SRS for schools and students? If schools differ in size, this will result in equal weights for each school and different weights for students. Alternatively, use PPS for schools. Why not offer both alternatives?
                        # Non-response weights? (i.e., simulate non-response?) ..or would the weights be incorporated into questionnaire_gen (ex.: sample cat_prop = list(c(.4, 1)) where it should be c(.5, 1)? Leave this for later
                        # TODO: Replicate weights
                        # TODO: Control over inter-class correlation (intra-class is handled by quest_gen?). Add correlations (within, between)
                        ...) {

  n_levels <- length(clusters)
  # Adapting additional parameters to questionnaire_gen format
  if (n_levels > 1) {
    if (length(n_obs) == 1) n_obs <- c(clusters[-1], n_obs)
    if (length(n_X) == 1) n_X <- rep(n_X, n_levels)
    if (length(n_W) == 1) n_W <- rep(n_W, n_levels)
  }

  if (separate_questionnaires) {  # questionnaires administered at all levels
    # Generates unique questionnaires for each level
    if (is.null(n_X)) {
      # TODO: allow custom c_mean for each cluster or only levels (implemented)? Idea for this: lists of lists (ideally, something simpler, though)
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
    sample <- cluster_gen_separate(n_levels, clusters, n_obs,
                                   labels, collapse,
                                   n_X, n_W, c_mean, ...)
  } else {  # questionnaires administered only at the bottom level
    if (is.null(n_X)) n_X <- rzeropois(1.5)  # a positive number of Xs
    if (is.null(n_W)) n_W <- as.list(replicate(rzeropois(5), 2))  # all binary
    sample <- cluster_gen_together(n_levels, clusters, n_obs,
                                   labels, collapse,
                                   n_X, n_W, c_mean, ...)
  }
  return(sample)
}