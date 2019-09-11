#' @title Generate cluster samples with individual questionnaires
#' @description This is a subfunction of `cluster_gen` that performs cluster sampling, with the twist that each cluster level has its own questionnaire.
#' @param n_levels number of cluster levels
#' @param n numeric vector with the number of sampled observations (clusters or subjects) on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param N list of numeric vector with the population size of each *sampled* cluster element on each level
#' @param sum_pop total population at the lowest level (sampled or not)
#' @param calc_weights if `TRUE`, sampling weights are calculated
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size, "mixed" to use SRS for students and PPS otherwise or a vector with the sampling method for each level
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param verbose if `TRUE`, prints output messages
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @seealso cluster_gen cluster_gen_together
#' @export
cluster_gen_separate <- function(n_levels, n, N, sum_pop,  calc_weights, 
                                 sampling_method, cluster_labels, resp_labels,
                                 collapse, n_X, n_W, c_mean, verbose, ...)
{
  # Creating basic elements ----------------------------------------------------
  out    <- list()  # actual output (differs from sample if collapse)
	sample <- list()  # will store all BG questionnaires
  c_mean_list <- c_mean
  n_quest <- sapply(n, sum)
  id_combos <- labelRespondents(n, cluster_labels)

  # Generating data ------------------------------------------------------------
  for (l in seq(n_levels - 1)) {
    # Adapting additional parameters to questionnaire_gen format
    if (class(c_mean_list) == "list") c_mean <- c_mean_list[[l]]

    # Defining labels and IDs for this cluster and the next one
    level_label <- cluster_labels[l]
    next_level_label <- ifelse(
      test = l < n_levels - 1,
      yes  = cluster_labels[l + 1],
      no   = resp_labels[l]
    )
    previous_clusterID <- NULL
    previous_sublvl <- 0
    if (l > 1) {
      # Only applicable for sub-country levels and when next nevel is an
      # indicator of "X per Y" (instead of "X across Y")
      if (class(n) != "list") n[l] <- n[l] * n[l - 1]
      previous_clusterID <- as.vector(unlist(sapply(sample[[l - 1]],
                                            function(x) x$clusterID)))

      # Remove letters from label (useful for calculating weights)
      previous_sublvl <- gsub("[A-Za-z]", "", previous_clusterID)
      previous_sublvl <- as.numeric(gsub("\\_.", "", previous_sublvl))
    }
    n_groups <- sapply(n, sum)[l]
    
    # Generating questionnaires for each cluster element of that level
    for (lvl in seq(n_groups)) {
      # Creating basic elements
      n_resp <- n[[l + 1]][lvl]
      mu <- NULL
      if (!is.null(c_mean) & class(c_mean) == "list") {
        mu <- c_mean[[lvl]]
      } else {
        mu <- c_mean
      }

      # Generating data
      cluster_bg <- questionnaire_gen(
        n_resp, n_X = n_X[[l]], n_W = n_W[[l]], c_mean = mu, verbose = FALSE,
        ...
      )

      # Adding weights
      if (calc_weights) {
        cluster_bg <- weightResponses(
          cluster_bg, n, N, l + 1, lvl, previous_sublvl[lvl], sampling_method,
          cluster_labels, resp_labels, sum_pop, verbose
        )
      }

      # Generating unique IDs
      respID <- paste0(next_level_label, seq(cluster_bg$subject))
      if (l > 1) {
        previous_lvl <- as.vector(unlist(sapply(n[[l]], seq)))[lvl]
        cluster_bg$clusterID <- paste0(level_label, previous_lvl, "_",
                                        previous_clusterID[lvl])
      } else {
        cluster_bg$clusterID <- paste0(level_label, lvl)
      }
      cluster_bg$uniqueID <- paste(respID, cluster_bg$clusterID, sep = "_")

      # Saving the questionnaire to the final list (sample)
      cluster_bg -> sample[[level_label]][[lvl]]
    }

    # Collapsing levels and removing clusterIDs -------------------------------
    if (collapse == "none") {
      out[[l]] <- sample[[l]]
      for (ll in seq(length(out[[l]]))) {
        out[[l]][[ll]]["clusterID"] <- NULL
      }
      names(out)[[l]] <- cluster_labels[l]
    } else {
      out[[level_label]] <- do.call(rbind, sample[[level_label]])
      if (collapse == "full") {
        if (l == 1) {
          names(out[[l]]) <- paste0(names(out[[l]]), ".", resp_labels[l])
        }
        if (l > 1) {
          names(out[[l]]) <- paste0(names(out[[l]]), ".", resp_labels[l])
          out[[l]] <- merge(x = out[[l]], y = out[[l - 1]][-1],
                            by.x = paste0("clusterID", ".", resp_labels[l]),
                            by.y = paste0("uniqueID", ".", resp_labels[l - 1]))
          out[[l]][paste0("clusterID.", level_label)] <- NULL
        }
        if (l == n_levels - 1) {
          out <- out[[l]]
          # Removing first and last clusterIDs
          names(out[[l]]) <- paste0(names(out[[l]]), ".", resp_labels[l])
          out[paste0("clusterID.", resp_labels[1])] <- NULL
          out[paste0("clusterID.", resp_labels[l])] <- NULL
          # Renaming subjects (variable and values)
          names(out)[1] <- "subject"
          out$subject <- seq(nrow(out))
        }
      } else {
        out[[level_label]]["clusterID"] <- NULL
        out[[level_label]]$subject <- seq(nrow(out[[level_label]]))
      }
    }
  }
  
  return(out)
}