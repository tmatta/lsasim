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

  # Final level
  message("Each ", cluster_labels[n_levels - 1], " sampled ",
          resp_labels[n_levels - 1], " (", n_obs_print[n_levels], ")")

  # Total respondents
  if (class(n_obs) == "list") {
    if (separate_questionnaires) {
      tot_resp <- sum(unlist(n_obs)[-1])
      operands <- unlist(n_obs[-1])
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

labelRespondents <- function (n_obs, cluster_labels) {
  # This function nerated level label combinations for each respondent

  n_levels <- length(n_obs)
	level_combos <- list()  # will store ID combinations

  if (class(n_obs) == "list") {
    n_combos <- sum(n_obs[[n_levels - 1]])
    second_last_level <- unlist(sapply(n_obs[[n_levels - 1]], seq))
    id_combos <- matrix(second_last_level, ncol = n_combos)
    for (row in 2:(n_levels - 1)) {
      id_level <- n_levels - row
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
    id_combos <- t(id_combos)
  } else {
    for (l in 1:(n_levels - 1)) {
      level_combos[[n_levels - l]] <- seq(from = 1, to = n_obs[l])
    }
    id_combos <- expand.grid(level_combos)
  }
  id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
  colnames(id_combos) <- cluster_labels[-n_levels]
  for (l in seq(ncol(id_combos))) {
    id_combos[, l] <- paste0(cluster_labels[l], id_combos[, l])
  }
  return(id_combos)
}

weightResponses <- function(cluster_bg, n_obs, N, lvl, sublvl, sampling_method,
                            cluster_labels, resp_labels) {
  # This function calculates weight responses
  if (sampling_method == "mixed")
  {
    warning("Mixed sampling method not yet implemented")
  }
  else if (sampling_method == "SRS")
  {
    if (class(n_obs) == "list")
    {
      # Probabilities
      p_1_i <- n_obs[[lvl - 1]] / N[[lvl - 1]]
      p_2_ij <- n_obs[[lvl]] / N[[lvl]]
      if (length(p_1_i) > 1) p_1_i <- p_1_i[sublvl]
      if (length(p_2_ij) > 1) p_2_ij <- p_2_ij[sublvl]
      p_ij <- p_1_i * p_2_ij
      # Variable names
      label_1_i <- paste0(cluster_labels[lvl - 1], ".weight")
      label_2_ij <- paste0("within.", cluster_labels[lvl - 1], ".weight")
      label_ij <- paste0("final.", resp_labels[lvl], ".weight")

    }
    else
    {
      # Probabilities
      p_1_i <- n_obs[lvl - 1] / N[lvl - 1]
      p_2_ij <- n_obs[lvl] / N[lvl]
      p_ij <- p_1_i * p_2_ij
      # Variable names
      label_1_i <- paste0(cluster_labels[lvl - 1], ".weight")
      label_2_ij <- paste0("within.", cluster_labels[lvl - 1], ".weight")
      label_ij <- paste0("final.", resp_labels[lvl], ".weight")
    }
      # Weights
      w_1_i <- 1 / p_1_i  # school weight
      w_2_ij <- 1 / p_2_ij  # within-school weight
      w_ij <- 1 / p_ij  # final student weight
      # Final assignments
      cluster_bg[label_1_i] <- w_1_i
      cluster_bg[label_2_ij] <- w_2_ij
      cluster_bg[label_ij] <- w_ij
  }
    else if (sampling_method == "PPS")
  {
    message("PPS not yet implemented")
  }
  return(cluster_bg)
}