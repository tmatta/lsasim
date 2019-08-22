clusterMessage <- function(n_obs, resp_labels, cluster_labels, n_levels,
                           separate_questionnaires, type) {
  # This function prints messages about the cluster scheme before generating questionnaire responses. All arguments are from cluster_gen except for "type", which is numeric and changes the way the first line is printed.
  if (type == 1) {
    # Comma-separated multiple questionnaires
    message("Generating questionnaires for ",
            paste(cluster_labels, collapse = ", "))
  } else {
    # Questionnaires only for the lowest level
    message("Generating questionnaires for ", resp_labels[n_levels - 1])  
  }

  tot_resp <- 0
  operands <- NULL
  for (l in seq(length(n_obs) - 1)) { # Final row of messages is different
    n_obs_print <- n_obs

    # Printing top level
    if (l == 1) {
      message("Top level: ", cluster_labels[l], " (", n_obs_print[l], ")")
    }

    # Printing second to second-to-last levels
    if (class(n_obs) == "list" & length(n_obs[[l + 1]]) > 1) {
      n_obs_print[[l]] <- paste0(paste(n_obs[[l]], collapse = " and "),
                                 ", respectively")
      n_obs_print[[l + 1]] <- paste0(paste(n_obs[[l + 1]], collapse = " and "),
                                     ", respectively")
    }
    if (l < length(n_obs) - 1) {
      message("Each ", cluster_labels[l], " sampled ",  cluster_labels[l + 1],
              " (", n_obs_print[l + 1], ")")
    }
    if (l > 1 & class(n_obs) != "list" & separate_questionnaires) {
      tot_resp <- tot_resp + prod(n_obs[1:l])
      operands <- c(operands, prod(n_obs[1:l]))
    }
  }

  # Final row of messages
  message("Each ", cluster_labels[n_levels - 1], " sampled ",
          resp_labels[n_levels - 1], " (", n_obs_print[n_levels], ")")

  if (class(n_obs) == "list") {
    if (separate_questionnaires) {
      tot_resp <- sum(unlist(n_obs))
      operands <- unlist(n_obs)
      operator <- " + "
    } else {
      tot_resp <- sum(unlist(n_obs[[n_levels]]))
      operands <- unlist(n_obs[[n_levels]])
      operator <- " + "
    }
  } else {
    if (separate_questionnaires) {
      tot_resp <- tot_resp + prod(n_obs)
      operands <- c(operands, prod(n_obs))
      operator <-  " + "
    } else {
      tot_resp <- prod(n_obs)
      operands <- n_obs
      operator <- " * "
    }
  }
  message("Total respondents: ", paste0(tot_resp, " (",
            paste(operands, collapse = operator), ")"))
}