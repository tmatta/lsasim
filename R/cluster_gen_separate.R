#' @title Generate cluster samples with individual questionnaires
#' @description This is a subfunction of `cluster_gen` that performs cluster sampling, with the twist that each cluster level has its own questionnaire.
#' @param n_levels number of cluster levels
#' @param n_obs numeric vector with the number of clusters on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @export
cluster_gen_separate <- function(n_levels, n_obs,
                                 cluster_labels, resp_labels, collapse, n_X,
                                 n_W, c_mean, ...) {
  out    <- list()
	sample <- list()  # will store all BG questionnaires
  for (l in seq(n_levels - 1)) {
    # Adapting additional parameters to questionnaire_gen format
    c_mean_list <- c_mean
    if (class(c_mean_list) == "list") c_mean <- c_mean_list[[l]]

    # Defining labels and IDs for this cluster and the next one
    level_label <- cluster_labels[l]
    next_level_label <- ifelse(l < n_levels - 1, cluster_labels[l + 1], resp_labels[l])

    if (l > 1) {  # Only applicable for sub-country levels
      n_obs[l] <- n_obs[l] * n_obs[l - 1]
      previousClusterID <- as.vector(sapply(sample[[l - 1]],
                                            function(x) x$clusterID))
    }

    for (c in seq(n_obs[l])) {
      # Generating data
      cluster_bg <- questionnaire_gen(n_obs[l + 1],
                                      n_X = n_X[[l]], n_W = n_W[[l]],
                                      c_mean = c_mean, verbose = FALSE,...)
      # Generating unique IDs
      respID <- paste0(next_level_label, seq(cluster_bg$subject))
      if (l > 1) {
        previous_c <- rep(seq(n_obs[l] / n_obs[l - 1]), n_obs[l])[c]
        cluster_bg$clusterID <- paste0(level_label, previous_c, "_",
                                        previousClusterID[c])
      } else {
        cluster_bg$clusterID <- paste0(level_label, c)
      }
      cluster_bg$uniqueID <- paste(respID, cluster_bg$clusterID, sep = "_")

      # Saving the questionnaire to the final list (sample)
      cluster_bg -> sample[[level_label]][[c]]
    }

    # Collapsing levels and removing clusterIDs -------------------------------
    if (collapse == "none") {
      out[[l]] <- sample[[l]]
      for (ll in seq(length(out[[l]]))) {
        out[[l]][[ll]]["clusterID"] <- NULL
      }
      if (l == n_levels - 1) names(out) <- cluster_labels[-(n_levels - 1)]
    } else {
      out[[level_label]] <- do.call(rbind, sample[[level_label]])
      if (collapse == "full") {
        if (l == 1) names(out[[l]]) <- paste0(names(out[[l]]), ".", cluster_labels[l])
        if (l > 1) {
          names(out[[l]]) <- paste0(names(out[[l]]), ".", cluster_labels[l])
          out[[l]] <- merge(x = out[[l]], y = out[[l - 1]][-1],
                            by.x = paste0("clusterID", ".", cluster_labels[l]),
                            by.y = paste0("uniqueID", ".", cluster_labels[l - 1]))
          out[[l]][paste0("clusterID.", level_label)] <- NULL
        }
        if (l == n_levels - 1) {
          out <- out[[l]]
          # Removing first and last clusterIDs
          out[paste0("clusterID.", cluster_labels[1])] <- NULL
          out[paste0("clusterID.", cluster_labels[l])] <- NULL
          # Renaming subjects (variable and values)
          names(out)[1] <- resp_labels[l]
          out$subject <- seq(nrow(out))
        }
      } else {
        out[[level_label]]["clusterID"] <- NULL
      }
    }
  }
  return(out)
}