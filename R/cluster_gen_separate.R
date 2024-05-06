#' @title Generate cluster samples with individual questionnaires
#' @description This is a sub-function of `cluster_gen` that performs cluster sampling, with the twist that each cluster level has its own questionnaire.
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
#' @param cat_prop list of cumulative proportions for each item. If \code{theta
#'   = TRUE}, the first element of \code{cat_prop} must be a scalar 1, which
#'   corresponds to the \code{theta}.
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param sigma vector of standard deviations for the continuous variables or list of vectors for the continuous variables for each level
#' @param cor_matrix Correlation matrix between all variables (except weights)
#' @param verbose if `TRUE`, prints output messages
#' @param rho estimated intraclass correlation
#' @param theta if \code{TRUE}, the first continuous variable will be labeled
#'   'theta'. Otherwise, it will be labeled 'q1'.
#' @param whitelist used when `n = select(...)`, determines which PSUs get to generate questionnaires
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @seealso cluster_gen cluster_gen_together
#' @importFrom stats rchisq
#' @importFrom methods is
#' @export
cluster_gen_separate <- function(
  n_levels, n, N, sum_pop, calc_weights, sampling_method, cluster_labels, resp_labels, collapse, n_X, n_W, cat_prop, c_mean, sigma, cor_matrix,
  rho, theta, whitelist, verbose, ...
) {
  # Creating basic elements ====================================================
  out    <- list()  # actual output (differs from sample if collapse)
	sample <- list()  # will store all BG questionnaires
  c_mean_list <- c_mean
  sigma_list <- sigma
  n_quest <- sapply(n, sum)
  id_combos <- label_respondents(n, cluster_labels)
  missing_sigma2 <- is.null(sigma)
  if (!is(cor_matrix[1], "list")) {
    cor_matrix <- replicate(n_levels - 1, list(cor_matrix))
  }
  cor_matrix_list <- cor_matrix
  cat_prop_orig <- cat_prop

  # Generating data ============================================================
  for (l in seq(n_levels - 1)) {
    ## Adapting additional parameters to questionnaire_gen format --------------
    if (is(c_mean_list, "list")) c_mean <- c_mean_list[[l]]
    if (is(sigma_list, "list")) sigma <- sigma_list[[l]]
    if (any(sapply(cat_prop_orig, class) == "list")) {
      cat_prop <- cat_prop_orig[[l]]
    }

    ## Defining labels and IDs for this cluster and the next one ---------------
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
      if (!is(n, "list")) n[l] <- n[l] * n[l - 1]
      previous_clusterID <- as.vector(unlist(sapply(sample[[l - 1]],
                                            function(x) x$clusterID)))

      # Remove letters from label (useful for calculating weights)
      previous_sublvl <- gsub("[A-Za-z]", "", previous_clusterID)
      previous_sublvl <- as.numeric(gsub("\\_.", "", previous_sublvl))
    }
    n_groups <- sapply(n, sum)[l]

    ## Defining parameters for intraclass correlations -------------------------
    if (!is.null(rho)) {

      ### Expanding rho to n_level width .......................................
      if (!is(rho, "list")) rho <- replicate(n_levels, list(rho))
      if (length(rho[[l]]) == 1) rho[[l]] <- rep(rho[[l]], n_X[[l]] + theta)

      ### Defining sigma2 and tau2 .............................................
      n_j <- n[[l + 1]]
      M <- sum(n_j)
      if (missing_sigma2) {
        if (is.null(c_mean) | is.null(rho) | length(c_mean) == 1) {
          sigma2 <- rchisq(n_X[[l]] + theta, 2)
        } else {
          n_tilde <- calc_n_tilde(M, N[[l]], n_j)
          mean_j <- unlist(c_mean)
          overall_mean <- sum(mean_j * n_j) / M
          s2btw <- calc_var_between(n_j, mean_j, overall_mean, n_tilde, N[[l]])
          tau2 <- s2btw * n_tilde
          sigma2 <- tau2 * (1 - rho[[l]]) / rho[[l]]
        }
      } else {
        if (is(sigma, "list")) {
          sigma2 <- sigma[[l]] ^ 2
        } else {
          sigma2 <- sigma ^ 2
        }
      }
      tau2 <- rho[[l]] * sigma2 / (1 - rho[[l]])

      ### Defining the group correlations (s2_j == s2 for all j) ...............
      Nn <- length(n_j)
      s2 <- sigma2 * (M - Nn) / sum(n_j - 1)
    }

    ## Generating questionnaires for each cluster element of that level --------
    for (lvl in seq(n_groups)) {

      ### Creating basic elements ..............................................
      n_resp <- n[[l + 1]][lvl]
      if (any(sapply(cat_prop, class) == "list")) {
        cat_prop_lvl <- cat_prop[[lvl]]
      } else {
        cat_prop_lvl <- cat_prop
      }
      if (!is.null(c_mean) & is(c_mean, "list")) {
        mu_mu <- c_mean[[lvl]]
      } else {
        mu_mu <- c_mean
      }
      if (!is.null(cor_matrix) & class(cor_matrix)[1] == "list") {
        cor_mx <- cor_matrix[[l]]
        if (is(cor_mx[1], "list"))  cor_mx <- cor_matrix[[l]][[lvl]]
      }
      if (!is.null(rho[[l]])) {
        sd_X <- sqrt(s2)  # same sd for all PSUs if rho is present
      } else if (!is.null(sigma) & is(sigma, "list")) {
        sd_X <- sigma[[lvl]]
      } else {
        sd_X <- sigma
      }

      ### Recalculating mu to fit rho ..........................................
      if (all(!is.null(rho[[l]]))) {
        sd_mu <- sqrt(tau2 + sigma2 / n_j[lvl])  # from Snijders p. 20
        mu_mu <- ifelse(is.null(mu_mu), 0, mu_mu)
        mu <- sapply(sd_mu, function(s) rnorm(1, mu_mu, s))
      } else {
        mu <- mu_mu
      }

      ### Generating data ......................................................
      # Parsing lists of lists
      if (any(sapply(n_W[[l]], class) == "list")) {
        n_W_used <- n_W[[l]][[lvl]]
      } else {
        n_W_used <- n_W[[l]]
      }
      if (is(n_W_used, "list") & any(sapply(n_W_used, length) > 1)) {
        n_W_used <- n_W_used[[l]][lvl]
      }
      cluster_bg <- questionnaire_gen(
        n_resp, n_X = n_X[[l]], n_W = n_W_used, cat_prop = cat_prop_lvl,
        c_mean = mu, verbose = FALSE, c_sd = sd_X, cor_matrix = cor_mx,
        theta = theta, ...
      )

      ### Adding weights .....................................................
      if (calc_weights) {
        cluster_bg <- weight_responses(
          cluster_bg, n, N, l + 1, lvl, previous_sublvl[lvl], sampling_method,
          cluster_labels, resp_labels, sum_pop, verbose
        )
      }

      ### Generating unique IDs ..............................................
      respID <- paste0(next_level_label, seq(cluster_bg$subject))
      if (l > 1) {
        previous_lvl <- as.vector(unlist(sapply(n[[l]], seq)))[lvl]
        cluster_bg$clusterID <- paste0(level_label, previous_lvl, "_",
                                        previous_clusterID[lvl])
      } else {
        cluster_bg$clusterID <- paste0(level_label, lvl)
      }
      cluster_bg$uniqueID <- paste(respID, cluster_bg$clusterID, sep = "_")

      # Drop the whole thing if data is not on the whitelist .................
      if (!is.null(whitelist)) {
        if (l == 1) {
          clusterID_extracted <- gsub(
            pattern = "\\D", # anything that is not a digit
            replacement = "",
            cluster_bg[, "clusterID"][1]
          )
          clusterID_extracted <- as.numeric(clusterID_extracted)
          whitelist_extracted <- whitelist[, l]
          is_whitelisted <- (lvl %in% whitelist[, 1:l])
        } else {
          clusterID_extracted <- gsub(
            pattern = "\\D", # anything that is not a digit
            replacement = "",
            cluster_bg[, "clusterID"][1]
          )
          clusterID_extracted <- as.numeric(clusterID_extracted)
          whitelist_extracted <- apply(
            whitelist[, rev(seq_len(l))],
            1,
            function(x) paste(x, collapse = "")
          )
          is_whitelisted <- match(clusterID_extracted, whitelist_extracted)
          is_whitelisted <- !is.na(is_whitelisted)
        }
        # if (!(lvl %in% whitelist[, 1:l])) {
        if (!is_whitelisted) {
          cluster_bg[, 2:(ncol(cluster_bg) - 2)] <- NA
        }
        # Dropping indivial elements that should not have been sampled .......
        if (all(!is.na(cluster_bg))) {
          if (l < n_levels - 1) {
            keep_rows <- whitelist[whitelist[, l] == lvl, l + 1]
            blacklisted_rows <- which(
              is.na(match(rownames(cluster_bg), keep_rows))
            )
            cluster_bg[blacklisted_rows, 2:(ncol(cluster_bg) - 2)] <- NA
          } else if (l == n_levels - 1) {
            rowmatched <- match(clusterID_extracted, whitelist_extracted)
            limit <- whitelist[rowmatched, n_levels]
            if (limit < nrow(cluster_bg))
            cluster_bg <- cluster_bg[seq_len(limit), ]
          }
        }
      }

      ### Saving the questionnaire to the final list (sample) ................
      sample[[level_label]][[lvl]] <- cluster_bg
    }

    ## Collapsing levels and removing clusterIDs -------------------------------
    if (collapse == "none") {
      out[[l]] <- sample[[l]]
      for (ll in seq_along(out[[l]])) {
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
          non_weight_cols <- grep("weight", names(out[[l]]), invert = TRUE)
          names(out[[l]])[non_weight_cols] <-
            paste0(names(out[[l]])[non_weight_cols], ".", resp_labels[l])
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

  # Returning datasets =========================================================
  return(out)
}
