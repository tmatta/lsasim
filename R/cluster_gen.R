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
cluster_gen <- function(clusters,  # TODO: allow levels with different sizes
                        n_obs = 5,
                        labels = c("country", "school", "class"),
                        collapse = FALSE,
                        n_X = NULL,
                        n_W = NULL,
                        c_mean = 0,
                        separate_questionnaires = TRUE,
                        # TODO: add weights
                        # TODO: add correlations (within, between)
                        ...) {

  n_levels <- length(clusters)
  # Adapting additional parameters to questionnaire_gen format
  if (n_levels > 1) {
    if (length(n_obs) == 1) n_obs <- c(clusters[-1], n_obs)
    if (length(n_X) == 1) n_X <- rep(n_X, n_levels)
    if (length(n_W) == 1) n_W <- rep(n_W, n_levels)
  }
  c_mean_list <- c_mean

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
    sample <- cluster_gen_separate(n_levels, c_mean_list, clusters, n_obs,
                                   labels, collapse,
                                   n_X, n_W, c_mean, ...)
  } else {  # questionnaires administered only at the bottom level
    if (is.null(n_X)) n_X <- rzeropois(1.5)  # a positive number of Xs
    if (is.null(n_W)) n_W <- as.list(replicate(rzeropois(5), 2))  # all binary
    sample <- cluster_gen_together(n_levels, c_mean_list, clusters, n_obs,
                                   labels, collapse,
                                   n_X, n_W, c_mean, ...)
  }
  return(sample)
}