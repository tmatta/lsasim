#' @title Label respondents
#' @description This function nerated level label combinations for each respondent
#' @param n_obs list with the number of elements per level
#' @param cluster_labels character vector with the names of each cluster level
#' @param add_last_level if `TRUE` (not default), adds the last level to the output table
#' @param apply_labels if `TRUE`, applies labels (column names) to data cells
#' @return Data frame with the combinations of IDs from all levels
#' @export
label_respondents <- function (n_obs, cluster_labels = names(n_obs),
                               add_last_level = FALSE, apply_labels = TRUE)
{
  # Creating basic elements ====================================================
  n_levels <- length(n_obs)
	level_combos <- list()  # will store ID combinations
  n_combos <- sum(n_obs[[n_levels - 1]])
  second_last_level <- unlist(sapply(n_obs[[n_levels - 1]], seq))
  id_combos <- matrix(second_last_level, ncol = n_combos)

  # Create labels ==============================================================
  if (is.null(cluster_labels)) {
    cluster_labels <- attribute_cluster_labels(n_obs)$cl
    if (length(cluster_labels) == length(n_obs) - 1) {
      cluster_labels <- append(cluster_labels, "student") # hacky?
    }
  }

  # Assembling id_combos =======================================================
  if (n_levels - 1 >= 2) {
    for (row in 2:(n_levels - 1)) {
      id_level <- max(n_levels - row, 1)
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
  }
  id_combos <- t(id_combos)
  id_combos <- id_combos[, ncol(id_combos):1, drop = FALSE]  # so 1st lvl comes 1st
  id_combos <- as.data.frame(id_combos)  # prevents bug with list n. don't ask.
  names(id_combos) <- cluster_labels[-n_levels]

  # Adding bottom level ========================================================
  if (add_last_level) {
    id_combos <- data.frame(id_combos, n_obs[[n_levels]])
    names(id_combos) <- cluster_labels
  }

  # Pasting labels onto cells ==================================================
  if (apply_labels) {
    for (l in seq(ncol(id_combos))) {
      id_combos[, l] <- paste0(cluster_labels[l], id_combos[, l])
    }
  }
  return(id_combos)
}