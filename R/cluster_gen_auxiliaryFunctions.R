#' @title Convert Vector to Expanded List
#' @description Converts a vector to list where each element is replicated a certain number of times depending on the previous vector
#' @param x vector to ve converted
#' @return expanded/replicated version of x
convertVectorToList <- function(x)
{
  x_list <- as.list(x)
  for (lvl in 2:(length(x))) {
    x_list[[lvl]] <- rep(x[[lvl]],
                         prod(seq(from = x[[1]],
                                  to   = x[[lvl - 1]],
                                  length.out = lvl - 1)))
  }
  x <- x_list
  return(x)
}

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
clusterMessage <- function(n_obs, resp_labels, cluster_labels, n_levels,
                           separate_questionnaires, type, detail = FALSE)
{
  # Printing first-row message =================================================
  if (type == 1) {
    # Comma-separated multiple questionnaires
    message("Generating questionnaires for ",
            paste(cluster_labels, collapse = ", "))
  } else {
    # Questionnaires only for the lowest level
    message("Generating questionnaires for ", resp_labels[n_levels - 1])  
  }

  # Printing second until second-to-last messages ==============================
  tot_resp <- 0
  operands <- NULL
  for (l in seq(length(n_obs) - 1)) { # Final row of messages is different
    n_obs_print <- n_obs

    # Printing top level -------------------------------------------------------
    if (l == 1 & detail) {
      message("Top level: ", cluster_labels[l], " (", n_obs_print[l], ")")
    }

    # Printing second to second-to-last levels ---------------------------------
    if (class(n_obs) == "list" & length(n_obs[[l + 1]]) > 1) {
      n_obs_print[[l]] <- paste0(paste(n_obs[[l]], collapse = " and "),
                                 ", respectively")
      n_obs_print[[l + 1]] <- paste0(paste(n_obs[[l + 1]], collapse = " and "),
                                     ", respectively")
    }
    if (detail & l < length(n_obs) - 1) {
      message("Each ", cluster_labels[l], " sampled ",  cluster_labels[l + 1],
              " (", n_obs_print[l + 1], ")")
    }
    if (l > 1 & class(n_obs) != "list" & separate_questionnaires) {
      tot_resp <- tot_resp + prod(n_obs[1:l])
      operands <- c(operands, prod(n_obs[1:l]))
    }
  }

  # Final level ================================================================
  if (detail) {
    message("Each ", cluster_labels[n_levels - 1], " sampled ",
            resp_labels[n_levels - 1], " (", n_obs_print[n_levels], ")")
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

#' @title Label respondents
#' @description This function nerated level label combinations for each respondent
#' @param n_obs list with the number of elements per level
#' @param cluster_labels character vector with the names of each cluster level
#' @return Data frame with the combinations of IDs from all levels
labelRespondents <- function (n_obs, cluster_labels)
{
  # Creating basic elements ====================================================
  n_levels <- length(n_obs)
	level_combos <- list()  # will store ID combinations
  n_combos <- sum(n_obs[[n_levels - 1]])
  second_last_level <- unlist(sapply(n_obs[[n_levels - 1]], seq))
  id_combos <- matrix(second_last_level, ncol = n_combos)

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
  id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
  id_combos <- as.data.frame(id_combos)  # prevents bug with list n. don't ask.
  names(id_combos) <- cluster_labels[-n_levels]
  for (l in seq(ncol(id_combos))) {
    id_combos[, l] <- paste0(cluster_labels[l], id_combos[, l])
  }
  return(id_combos)
}

#' @title Weight responses
#' @description calculates sampling weights for the questionnaire responses
#' @param cluster_bg dataset with background questionnaire
#' @param n_obs list with the number of elements per level
#' @param N list of numeric vector with the population size of each *sampled* cluster element on each level
#' @param lvl number of the current level
#' @param sublvl number of the current sublevel (element within level)
#' @param previous_sublvl number of the sublevel of the parent level
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param sum_pop total population at each level (sampled or not)
#' @param verbose if `TRUE`, prints output messages
#' @return Input data frame (`cluster_bg`) with three new columns for the sampling weights.
weightResponses <- function(cluster_bg, n_obs, N, lvl, sublvl, previous_sublvl, 
                            sampling_method, cluster_labels, resp_labels,
                            sum_pop, verbose)
{
  # Determining sampling method ================================================
  if (length(sampling_method) > 1) {
    sampling_method <- sampling_method[lvl - 1]
  } else if (sampling_method == "mixed") {
    # Reassigns sampling method. PPS for schools, SRS for otherwise
    sampling_method <- ifelse(test = cluster_labels[lvl - 1] == "school",
                              yes  = "PPS",
                              no   = "SRS")
  }

  # Determining output variable names ==========================================
  label_1_i <- paste0(cluster_labels[lvl - 1], ".weight")
  label_2_ij <- paste0("within.", cluster_labels[lvl - 1], ".weight")
  label_ij <- paste0("final.", resp_labels[lvl - 1], ".weight")

  # Messages to user ===========================================================
  if (verbose) {
    if (sublvl == 1) {
      message("- Calculating ", sampling_method, " weights at the ",
              cluster_labels[lvl - 1], " level")
      if (sampling_method == "SRS") {
        message("  ", label_1_i, " should add up to the number of ",
          cluster_labels[lvl - 1], " in the population (",
                sum_pop[lvl - 1], ", repeated measures excluded)")
      } else {
        message("  ", label_ij, " should add up to the number of ",
          cluster_labels[lvl - 1], " in the population (",
                sum_pop[lvl] * length(N[[lvl - 1]]), ")")
      }
    }
  }

  # Probabilities (previous_lvl and within previous_lvl) =======================
  if (sampling_method == "SRS") {
    p_1_i <- n_obs[[lvl - 1]] / N[[lvl - 1]]
    p_2_ij <- n_obs[[lvl]] / N[[lvl]]
    if (length(p_1_i) > 1) p_1_i <- p_1_i[previous_sublvl]
    if (length(p_2_ij) > 1) p_2_ij <- p_2_ij[sublvl]
  } else if (sampling_method == "PPS") {
    p_1_i <- n_obs[[lvl - 1]] * N[[lvl]][sublvl] / sum_pop[lvl]
    p_2_ij <- n_obs[[lvl]] / N[[lvl]]
    if (length(p_1_i) > 1) p_1_i <- p_1_i[previous_sublvl]
    if (length(p_2_ij) > 1) p_2_ij <- p_2_ij[sublvl]
  }

  # Final lvl probabilities ====================================================
  p_ij <- p_1_i * p_2_ij

  # Weights ====================================================================
  w_1_i <- 1 / p_1_i  # school weight
  w_2_ij <- 1 / p_2_ij  # within-school weight
  w_ij <- 1 / p_ij  # final student weight

  # Final assignments ==========================================================
  cluster_bg[label_1_i] <- w_1_i
  cluster_bg[label_2_ij] <- w_2_ij
  cluster_bg[label_ij] <- w_ij

  return(cluster_bg)
}

#' @title Transform regular vector into selection vector
#' @description Attaches a "select" class to a vector
#' @param x vector
#' @return same as `x`, but with a class attribute that classifies `x` as "select"
#' @export
select <- function(x, ...)
{
  out <- c(x, ...)
  class(out) <- "select"
  return(out)
}

sampleFrom <- function (N, n, labels = names(N))
{
  # Creating basic elements ====================================================
  if (class(N) != "list") N <- convertVectorToList(N)
  unit_labels <- labelRespondents(N, labels)
  sampled_units <- unit_labels

  # Sampling elements until second-to-last level ===============================
  for (l in seq(from = 1, to = length(n) - 1)) {
    subunit_labels <- unique(unit_labels[, l])
    selected_subunit <- sample(x = subunit_labels, size = n[l])
    selected_rows <- sampled_units[, l] %in% selected_subunit
    sampled_units <- sampled_units[selected_rows, , drop = FALSE]
  }

  # Sampling from last level ===================================================
  browser()#TEMP
  # lapply(sampled_units, 1, function(x) sample(n[length(n))
  drawClusterStructure(N, labels)
  drawClusterStructure(N, labels, output = "text")
}