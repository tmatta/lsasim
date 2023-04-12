#' @title Generate cluster samples with lowest-level questionnaires
#' @description This is a subfunction of `cluster_gen` that performs cluster sampling where only the lowest-level individuals (e.g. students) fill out questionnaires.
#' @param n_levels number of cluster levels
#' @param n numeric vector with the number of sampled observations (clusters or subjects) on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param N list of numeric vector with the population size of each *sampled* cluster element on each level
#' @param sum_pop total population at the lowest level (sampled or not)
#' @param calc_weights if `TRUE`, sampling weights are calculated
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param cat_prop list of cumulative proportions for each item. If \code{theta
#'   = TRUE}, the first element of \code{cat_prop} must be a scalar 1, which
#'   corresponds to the \code{theta}.
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param sigma vector of standard deviations for the continuous variables or list of vectors for the continuous variables for each level
#' @param cor_matrix correlation matrix or list of correlation matrices per PSU
#' @param rho intraclass correlation (scalar, vector or list)
#' @seealso cluster_gen cluster_gen_separate cluster_gen_together
#' @param verbose if `TRUE`, prints output messages
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @export
cluster_gen_together <- function(
  n_levels, n, N, sum_pop, calc_weights, sampling_method, cluster_labels,
  resp_labels, collapse, n_X, n_W, cat_prop, c_mean, sigma, cor_matrix, rho, verbose, ...
) {
  # Creating basic elements ----------------------------------------------------
	sample <- list()  # will store all BG questionnaires
  c_mean_list <- c_mean
  if (is(c_mean_list, "list")) {
    c_mean_list <- c_mean_list[[n_levels - 1]]
  }
  missing_sigma2 <- is.null(sigma)
  sigma_list <- sigma
  if (is(sigma_list, "list")) {
    sigma_list <- sigma_list[[n_levels - 1]]
  }
  id_combos <- label_respondents(n, cluster_labels)  # level label combinations
  num_questionnaires <- nrow(id_combos)
  if (!is(cor_matrix[1], "list")) {
    cor_matrix <- replicate(n_levels - 1, list(cor_matrix))
  }
  cor_matrix_list <- cor_matrix
  if (any(sapply(cat_prop, class) == "list")) {
    cat_prop <- cat_prop[[n_levels - 1]]
  }

  ## Defining parameters for intraclass correlations -------------------------
  if (!is.null(rho)) {

    ### Expanding rho to n_level width .......................................
    if (length(rho) == 1) rho <- rep(rho, n_X)

    ### Defining sigma2 and tau2 .............................................
    if (missing_sigma2) {
      sigma2 <- rchisq(n_X, 2)
    } else {
      sigma2 <- sigma ^ 2
    }
    tau2 <- rho * sigma2 / (1 - rho)

    ### Defining the group correlations (s2_j == s2 for all j) ...............
    n_j <- n[[n_levels]]
    M <- sum(n_j)
    Nn <- length(n_j)
    s2 <- sigma2 * (M - Nn) / sum(n_j - 1)
  }

  ### Generating questionnaire data for lowest level ...........................
  for (l in seq(num_questionnaires)) {

    #### Filtering elements pertaining to that specific PSU ("l")
    if (any(sapply(cat_prop, class) == "list")) {
      cat_prop_lvl <- cat_prop[[l]]
    } else {
      cat_prop_lvl <- cat_prop
    }
    respondents <- n[[n_levels]][l]
    if (!is.null(c_mean_list) & is(c_mean_list, "list")) {
      mu_mu <- c_mean_list[[l]]
    } else {
      mu_mu <- c_mean_list
    }
    if (!is.null(cor_matrix) & is(cor_matrix, "list")) {
      cor_mx <- cor_matrix[[1]]
      if (is(cor_mx[1], "list"))  cor_mx <- cor_matrix[[1]][[l]]
    }

    if (!is.null(rho)) {
      sd_X <- sqrt(s2)  # same sd for all PSUs if rho is present
    } else if (!is.null(sigma_list) & is(sigma_list, "list")) {
      sd_X <- sigma_list
    } else {
      sd_X <- sigma_list
    }

    ## Recalculating mu to fit rho ---------------------------------------------
    if (all(!is.null(rho))) {
      sd_mu <- sqrt(tau2 + sigma2 / n_j[l])
      if (is.null(mu_mu)) mu_mu <- rep(0, length(sd_mu))
      mu <- NULL
      for (s in seq_along(sd_mu)) {
        mu <- append(mu, rnorm(1, mu_mu[s], sd_mu[s]))
      }
    } else {
      mu <- mu_mu
    }

    ## Generating data ---------------------------------------------------------
    cluster_bg <- questionnaire_gen(
      respondents, n_X = n_X, n_W = n_W, cat_prop = cat_prop_lvl,
      c_mean = mu, c_sd = sd_X, cor_matrix = cor_mx, verbose = FALSE,...
    )

    # Adding weights
    if (calc_weights) {
      cluster_bg <- weight_responses(
        cluster_bg, n, N, n_levels, l, previous_sublvl = 0,
        sampling_method, cluster_labels, resp_labels, sum_pop, verbose
      )
    }

    # Creating new ID variable
    studentID <- paste0("student", seq(nrow(cluster_bg)))
    clusterID <- paste(rev(id_combos[l, ]), collapse = "_")
    cluster_bg$uniqueID <- paste(studentID, clusterID, sep = "_")

    # Saving the questionnaire to the final list (sample)
    cluster_bg -> sample[[l]]
  }

  # Collapsing data and creating output ----------------------------------------
  if (collapse == "none") {
    names(sample) <- paste0(cluster_labels[n_levels - 1], seq(length(sample)))
  } else {
    sample <- do.call(rbind, sample)
    sample$subject <- seq(nrow(sample))
  }

  return(sample)
}
