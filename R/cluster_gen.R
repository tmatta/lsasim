#' @title Generate cluster sample
#' @param clusters numeric vector with the number of clusters on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the respondents on each level
#' @param n_obs numeric vector with the number of observations in each cluster
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @param separate_questionnaires if `TRUE`, each level will have its own questionnaire
#' @details `n_obs` can have unitary length, in which case all clusters will have the same size.
#'   Regarding the additional parameters to be passed to `questionnaire_gen()`, they can be passed either in the same format as `questionnaire_gen()` or as more complex objects that contain information for each cluster level.
#' @examples
#' cluster_gen(c(1, 2), collapse = FALSE)
#' cluster_gen(c(4, 2), n_X = 1, n_W = list(2, 3), theta = TRUE,
#'             c_mean = list(0, c(0, 10)))
#' @export
cluster_gen <- function(clusters,  # TODO: allow for levels with different sizes
                        n_obs = 5,
                        cluster_labels = c("country", "school", "class"),
                        resp_labels = c("principal", "teacher", "student"),
                        collapse = FALSE,
                        n_X = 2,
                        n_W = list(5, 5, 5),
                        c_mean = 0,
                        separate_questionnaires = TRUE,
                        # TODO: add weights
                        # TODO: add correlations (within, between)
                        ...) {

  # TODO: add combined questionnaires (id. of student in class, school, etc.)?
  # TODO: with combined IDs?

  n_levels <- length(clusters)
  # Adapting additional parameters to questionnaire_gen format
  if (n_levels > 1 & length(n_obs) == 1) n_obs <- c(clusters[-1], n_obs)
  c_mean_list <- c_mean

  if (separate_questionnaires) {  # questionnaires administered at all levels
    sample <- cluster_gen_separate(n_levels, c_mean_list, clusters, n_obs, cluster_labels, resp_labels, collapse, n_X, n_W, c_mean, ...)
  } else {  # questionnaires are administered only at the bottom level
    sample <- cluster_gen_together(n_levels, c_mean_list, clusters, n_obs, cluster_labels, resp_labels, collapse, n_X, n_W, c_mean, ...)
  }
  return(sample)
}