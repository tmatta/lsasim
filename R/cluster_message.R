#' @title Print messages about clusters
#' @description Prints messages about the cluster scheme before generating questionnaire responses.
#' @param n_obs list with the number of elements per level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param n_levels number of cluster levels
#' @param separate_questionnaires if `TRUE`, each level will have its own questionnaire
#' @param type Type of top-level message
#' @param detail if `TRUE`, prints further details about each level composition
#' @return Messages.
cluster_message <- function(n_obs, resp_labels, cluster_labels, n_levels,
                           separate_questionnaires, type, detail = FALSE)
{
  # Printing first-row message =================================================
  if (type == 1) {
    # Comma-separated multiple questionnaires
    message("Generating questionnaires for ",
            paste(pluralize(cluster_labels), collapse = ", "))
  } else {
    # Questionnaires only for the lowest level
    message("Generating questionnaires for ",
            pluralize(resp_labels[n_levels - 1]))
  }

  # Printing second until second-to-last messages ==============================
  tot_resp <- 0
  operands <- NULL
  for (l in seq(length(n_obs) - 1)) { # Final row of messages is different
    n_obs_print <- n_obs

    # Printing top level -------------------------------------------------------
    if (l == 1 & detail) {
      message("Top level: ", pluralize(cluster_labels[l]),
              " (", n_obs_print[l], ")")
    }

    # Printing second to second-to-last levels ---------------------------------
    if (is(n_obs, "list") & length(n_obs[[l + 1]]) > 1) {
      n_obs_print[[l]] <- paste0(paste(n_obs[[l]], collapse = " and "),
                                 ", respectively")
      n_obs_print[[l + 1]] <- paste0(paste(n_obs[[l + 1]], collapse = " and "),
                                     ", respectively")
    }
    if (detail & l < length(n_obs) - 1) {
      message("Each ", cluster_labels[l], " sampled ",
              pluralize(cluster_labels[l + 1]),
              " (", n_obs_print[l + 1], ")")
    }
    if (l > 1 & !is(n_obs, "list") & separate_questionnaires) {
      tot_resp <- tot_resp + prod(n_obs[1:l])
      operands <- c(operands, prod(n_obs[1:l]))
    }
  }

  # Final level ================================================================
  if (detail) {
    message("Each ", cluster_labels[n_levels - 1], " sampled ",
            pluralize(resp_labels[n_levels - 1]),
            " (", n_obs_print[n_levels], ")")
  }

  # Total respondents ==========================================================
  if (separate_questionnaires) {
    tot_resp <- sum(unlist(n_obs)[-1])
    operands <- unlist(n_obs[-1])
    operator <- " + "
  } else {
    tot_resp <- sum(unlist(n_obs[[n_levels]]))
    operands <- unlist(n_obs[[n_levels]])
    operator <- " + "
  }
  message("Total respondents: ", paste0(tot_resp, " (",
            paste(operands, collapse = operator), ")"))
}
