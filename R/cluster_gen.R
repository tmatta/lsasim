#' @title Generate cluster sample
#' @param levels numeric vector with the number of clusters on each level
#' @param labels character vector with the names of each level
#' @param n_obs numeric vector with the number of observations in each cluster
#' @param collapse if `TRUE`, 
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @details `n_obs` can have unitary length, in which case all clusters will have the same size.
#'   Regarding the additional parameters to be passed to `questionnaire_gen()`, they can be passed either in the same format as `questionnaire_gen()` or as more complex objects that contain information for each cluster level.
#' @examples
#' cluster_gen(levels = c(1, 2))
cluster_gen <- function(levels,
                        n_obs = 10,
                        labels = c("class",
                                   "school",
                                   "neighborhood",
                                   "city",
                                   "region",
                                   "country"),
                        collapse = TRUE,
                        c_mean = 0,
                        ...) {
  n_levels <- length(levels)
  sample <- list()  # will store all BG questionnaires

  if (length(n_obs) == 1) n_obs <- rep(n_obs, n_levels)
  c_mean_list <- c_mean
  for (level in seq(n_levels)) {
    # TODO: if n_X, n_W are not provided, use consistent values for each level?

    # Adapting additional parameters to questionnaire_gen format
    if (class(c_mean_list) == "list") {
      c_mean <- c_mean_list[[level]]
    }
    level_label <- labels[level]
    for (cluster in 1:levels[level]) {
      # Generating data
      cluster_bg <- suppressMessages(questionnaire_gen(n_obs[level],
                                                       c_mean = c_mean, ...))

      # Creating new ID variable
      cluster_bg$clusterID <- paste0(level_label, cluster)

      # Relabeling the subjects
      last_subject <- n_obs[level] * cluster
      first_subject <- last_subject - n_obs[level] + 1
      cluster_bg$subject <- first_subject:last_subject

      # Saving the questionnaire to the final list (sample)
      cluster_bg -> sample[[level_label]][[cluster]]
    }
    if (collapse) sample[[level_label]] <- do.call(rbind, sample[[level_label]])
  }

  # Renumbering subjects
  return(sample)
}