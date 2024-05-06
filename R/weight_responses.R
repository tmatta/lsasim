#' @title Weight responses
#' @description calculates sampling weights for the questionnaire responses
#' @param cluster_bg dataset with background questionnaire
#' @param n_obs list with the number of elements per level
#' @param N list of numeric vector with the population size of each *sampled* cluster element on each level
#' @param lvl number of the current level
#' @param sublvl number of the current sub-level (element within level)
#' @param previous_sublvl number of the sub-level of the parent level
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param sum_pop total population at each level (sampled or not)
#' @param verbose if `TRUE`, prints output messages
#' @return Input data frame (`cluster_bg`) with three new columns for the sampling weights.
weight_responses <- function(cluster_bg, n_obs, N, lvl, sublvl, previous_sublvl,
                             sampling_method, cluster_labels, resp_labels,
                             sum_pop, verbose)
{
  # Determining sampling method ================================================
  if (length(sampling_method) > 1) {
    sampling_method <- sampling_method[lvl - 1]
  } else if (sampling_method == "mixed") {
    # Reassigns sampling method. PPS for schools, SRS otherwise ----------------
    sampling_method <- ifelse(test = cluster_labels[lvl - 1] == "school",
                              yes  = "PPS",
                              no   = "SRS")
  }

  # Determining output variable names ==========================================
  label_1_i <- paste0(cluster_labels[lvl - 1], ".weight")
  label_2_ij <- paste0("within.", cluster_labels[lvl - 1], ".weight")
  label_ij <- paste0("final.", resp_labels[lvl - 1], ".weight")

  # Messages to user ===========================================================
  if (verbose & sublvl == 1) {
    message("- Calculating ", sampling_method, " weights at the ",
            cluster_labels[lvl - 1], " level")
    if (sampling_method == "SRS") {
      message("  ", label_1_i, " should add up to the number of ",
        pluralize(cluster_labels[lvl - 1]), " in the population (",
                  sum_pop[lvl - 1], ", counting once per ",
                  cluster_labels[lvl - 1], ")")
    } else {
      message("  ", label_ij, " should add up to the number of ",
        pluralize(resp_labels[lvl - 1]), " in the population (",
              # sum_pop[lvl - 1] * length(N[[lvl - 1]]), ")")
              sum_pop[lvl], ")")
    }
  }

  # Defining cluster structures ------------------------------------------------
  cluster_structure <- list(n = label_respondents(n_obs, cluster_labels),
                            N = label_respondents(N, cluster_labels))

  # Adding final levels --------------------------------------------------------
  final_level <- length(n_obs)
  cluster_structure$n <- data.frame(cluster_structure$n, n_obs[[final_level]])
  cluster_structure$N <- data.frame(cluster_structure$N, N[[final_level]])
  names(cluster_structure$N) <- names(cluster_structure$n) <- cluster_labels

  # Defining substructures =====================================================
  parent_index <- unlist(sapply(n_obs[[lvl - 1]], seq))[sublvl]
  parent_label <- paste0(cluster_labels[lvl - 1], parent_index)
  cluster_substructure <- list(
      n = cluster_structure$n[cluster_structure$n[, lvl - 1] == parent_label, ],
      N = cluster_structure$N[cluster_structure$N[, lvl - 1] == parent_label, ]
    )
  if (lvl >= 3) {
    for (l in 2:max(lvl - 1, 2)) {
      parent_label <- label_respondents(n_obs)[sublvl, l - 1]
      cluster_substructure <- list(
        n = cluster_substructure$n[cluster_substructure$n[, l - 1] == parent_label, ],
        N = cluster_substructure$N[cluster_substructure$N[, l - 1] == parent_label, ]
      )
    }
  }

  # Calculating the cluster weights ============================================
  retrieve_unique_labels <- function(x, lvl) {
    lbl <- apply(x[, 1:lvl, drop = FALSE], 1,
                 function (x) paste(x, collapse = ""))
    return(length(unique(lbl)))
  }
  n_sc <- retrieve_unique_labels(cluster_structure$n, lvl - 1)
  N_sc <- retrieve_unique_labels(cluster_structure$N, lvl - 1)
  n_i <- ifelse(test = lvl == final_level,
                yes  = cluster_substructure$n[, lvl],
                no   = retrieve_unique_labels(cluster_substructure$n, lvl))
  N_i <- ifelse(test = lvl == final_level,
                yes  = cluster_substructure$N[, lvl],
                no   = retrieve_unique_labels(cluster_substructure$N, lvl))
  if (sampling_method == "SRS") {
    p_1_i <- n_sc / N_sc
  } else if (sampling_method == "PPS") {
    N_total <- sum_pop[lvl]
    p_1_i <- N_i * n_sc / N_total
  }

  # Calculating the within cluster weights =====================================
  p_2_ij <- n_i / N_i

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
