#' @title Generate cluster sample
#' @param clusters numeric vector with the number of clusters on each level
#' @param labels character vector with the names of each level
#' @param n_obs numeric vector with the number of observations in each cluster
#' @param collapse if `TRUE`,
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
cluster_gen <- function(levels,
                        n_obs = 10,
                        labels = c("class",
                                   "school",
                                   "city",
                                   "region",
                                   "country"),
                        collapse = FALSE,
                        c_mean = 0,
                        # TODO: add weights
                        ...) {
  n_levels <- length(clusters)
  sample <- list()  # will store all BG questionnaires

  # TODO: add combined questionnaires (id. of student in class, school, etc.)?
  # TODO: with combined IDs?

  # Adapting additional parameters to questionnaire_gen format
  if (length(n_obs) == 1) n_obs <- rep(n_obs, n_levels)
  c_mean_list <- c_mean

  for (level in seq(n_levels)) {
    # TODO: if n_X, n_W are not provided, use consistent values for each level?
    # TODO: allow custom c_mean for each cluster or only levels (implemented)?
    #   Idea for this: lists of lists (ideally, something simpler, though)

    # Adapting additional parameters to questionnaire_gen format
    if (class(c_mean_list) == "list") {
      c_mean <- c_mean_list[[level]]
    }

    level_label <- labels[level]
    for (cluster in 1:levels[level]) {
      # Generating data
      cluster_bg <- suppressMessages(questionnaire_gen(n_obs[level],
                                                       c_mean = c_mean, ...))

      # Adapting additional parameters to questionnaire_gen format
      if (class(c_mean_list) == "list") {
        c_mean <- c_mean_list[[l]]
      }

      level_label <- labels[l]
      if (l > 1) {
        clusters[l] <- clusters[l] * clusters[l - 1]
      } else {
        id_prefix <- ""
      }
      for (c in 1:clusters[l]) {
        # Generating data
        cluster_bg <- questionnaire_gen(n_obs[l], n_X = n_X, n_W = n_W,
                                        c_mean = c_mean, verbose = FALSE,...)

        # Creating new ID variable
        # if (l == 2 & c == 4) browser()

        cluster_bg$clusterID <- paste0(level_label, c)

        # Relabeling the subjects
        last_subject <- n_obs[l] * c
        first_subject <- last_subject - n_obs[l] + 1
        cluster_bg$subject <- first_subject:last_subject

        # Saving the questionnaire to the final list (sample)
        cluster_bg -> sample[[level_label]][[c]]
      }
    }
    if (collapse) sample[[level_label]] <- do.call(rbind, sample[[level_label]])
  } else {  # questionnaires are administered only at the bottom level
    level_combos <- list()  # will store ID combinations
    for (c in 1:n_levels) {
      level_combos[[n_levels - c + 1]] <- seq(from = 1, to = clusters[c])
    }
    id_combos <- expand.grid(level_combos)
    id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
    colnames(id_combos) <- labels

    for (c in seq(ncol(id_combos))) {
      id_combos[, c] <- paste0(labels[c], id_combos[, c])
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
        # if (l == 2 & c == 4) browser()

        cluster_bg$clusterID <- paste(id_combos[c, ], collapse = "_")

        # Relabeling the subjects
        # last_subject <- n_obs[l] * c
        # first_subject <- last_subject - n_obs[l] + 1
        # cluster_bg$subject <- first_subject:last_subject

        # Saving the questionnaire to the final list (sample)
        cluster_bg -> sample[[c]]
      }
      if (collapse) sample <- do.call(rbind, sample)
  }
  return(sample)
}