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
weight_responses <- function(
  cluster_bg, n_obs, N, lvl, sublvl, previous_sublvl, sampling_method, 
  cluster_labels, resp_labels, sum_pop, verbose
)
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
          pluralize(cluster_labels[lvl - 1]), " in the population (",
                    sum_pop[lvl - 1], ", counting once per ",
                    cluster_labels[lvl - 1], ")")
      } else {
        message("  ", label_ij, " should add up to the number of ",
          pluralize(resp_labels[lvl - 1]), " in the population (",
                sum_pop[lvl] * length(N[[lvl - 1]]), ")")
      }
    }
  }

  # Probabilities (previous_lvl and within previous_lvl) =======================
  # TODO: overhaul and calculate p_1_i and p_2_ij from label_respondents(n_obs, cluster_labels)
  if (sampling_method == "SRS") {
    p_1_i <- n_obs[[lvl - 1]] / N[[lvl - 1]][seq_along(n_obs[[lvl - 1]])]
    p_2_ij <- n_obs[[lvl]] / N[[lvl]][seq_along(n_obs[[lvl]])]
    if (length(p_1_i) > 1) p_1_i <- p_1_i[previous_sublvl]
    if (length(p_2_ij) > 1) p_2_ij <- p_2_ij[sublvl]
  } else if (sampling_method == "PPS") {
    # browser()#TEMP
    p_1_i <- n_obs[[lvl - 1]] * N[[lvl]][seq_along(n_obs[[lvl - 1]])] / sum_pop[lvl]
    p_2_ij <- n_obs[[lvl]] / N[[lvl]][seq_along(n_obs[[lvl]])]
    if (length(p_1_i) > 1) p_1_i <- p_1_i[sublvl]  # was previous_sublvl
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