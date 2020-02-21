#' @title Generate cluster sample
#' @param n numeric vector with the number of sampled observations (clusters or subjects) on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param separate_questionnaires if `TRUE`, each level will have its own questionnaire
#' @param N list of numeric vector with the population size of each *sampled* cluster element on each level
#' @param calc_weights if `TRUE`, sampling weights are calculated
#' @param sum_pop total population at each level (sampled or not)
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param cat_prop list of cumulative proportions for each item. If \code{theta
#'   = TRUE}, the first element of \code{cat_prop} must be a scalar 1, which
#'   corresponds to the \code{theta}.
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param sigma vector of standard deviations for the continuous variables or list of vectors for the continuous variables for each level
#' @param cor_matrix Correlation matrix between all variables (except weights)
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param rho estimated intraclass correlation
#' @param verbose if `TRUE`, prints output messages
#' @param print_pop_structure if `TRUE`, prints the population hierarchical structure (as long as it differs from the sample structure)
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @return list with background questionnaire data, grouped by level or not
#' @details This function relies heavily in two subfunctions---`cluster_gen_separate` and `cluster_gen_together`---which can be called independently. This does not make `cluster_gen` a simple wrapper function, as it performs several operations prior to calling its subfunctions, such as randomly generating `n_X` and `n_W` if they are not determined by user.
#' `n` can have unitary length, in which case all clusters will have the same size.
#' `N` is *not* the population size across all elements of a level, but the population size for each element of one level.
#' Regarding the additional parameters to be passed to `questionnaire_gen()`, they can be passed either in the same format as `questionnaire_gen()` or as more complex objects that contain information for each cluster level.
#' @note For the purpose of this function, levels are counted starting from the top nesting/clustering level. This means that, for example, schools are the first cluster level, classes are the second, and students are the third and final level. This behavior can be customized by naming the `n` argument or providing custom labels in `cluster_labels` and `resp_labels`.
#' #TODO: explain the danger of setting c_mean and rho (sigma2 may get really high)
#'
#' labeling c_mean has no effect, it's for the user.
#' @seealso cluster_estimates cluster_gen_separate cluster_gen_together questionnaire_gen
#' @export
cluster_gen <- function(
  n,
  N = 1,
  cluster_labels = NULL,
  resp_labels = NULL,
  cat_prop = NULL,
  n_X = NULL,
  n_W = NULL,
  c_mean = NULL, # TODO: document about this being the grand mean if it's scalar
  sigma = NULL,
  # TODO: documet change in meaning from within-class variance to grand var
  cor_matrix = NULL,
  separate_questionnaires = TRUE,
  collapse = "none",
  sum_pop = sapply(N, sum),
  calc_weights = TRUE,
  sampling_method = "mixed",
  rho = NULL,
  verbose = TRUE,
  print_pop_structure = verbose,
  ...
)
{
  # Validating ============================================================
  validate_cluster_gen(
    n, N, cluster_labels, resp_labels, n_X, n_W, rho, sigma, c_mean,
    separate_questionnaires, collapse, sampling_method
  )

  # Attributing labels ====================================================
  if (is.null(cluster_labels)) cluster_labels <- attribute_cluster_labels(n)$cl
  if (is.null(resp_labels)) resp_labels <- attribute_cluster_labels(n)$resp

  # Checking valid n-N combinations and reformatting them if necessary ====
  class_n <- check_n_N_class(n)
  class_N <- check_n_N_class(N)

  census <- TRUE  # used to decide whether to print the pop structure or not
  if (class_N != "multiplier") {
    census <- FALSE
  } else {
    census <- (N == 1)
  }
  if (class_n == "select") {
    check_condition(class_N == "select", "N can't be select")
    n <- sample_from(N, n)
  } else if (class_n == "list with ranges") {
    if (class_N == "multiplier") {
      check_condition(N != 1,
                      "If n is a list, N must be 1 or explicitly defined")
      n <- convert_vector_to_list(n)
      N <- n
      sum_pop <- sapply(N, sum)
    } else if (class_N %in% c("vector", "list with ranges")) {
      N <- convert_vector_to_list(N)
      sum_pop <- sapply(N, sum)
      n <- convert_vector_to_list(n, N)
    } else if (class_N %in% "list without ranges") {
      n <- convert_vector_to_list(n, N)
    }
  } else if (class_n == "list without ranges") {
    if (class_N == "multiplier") {
      if (N == 1) {
        N <- n
        sum_pop <- sapply(N, sum)
      } else {
        stop("If n is a list, N must be 1 or explicitly defined")
      }
    } else if (class_N %in% c("vector", "list with ranges")) {
        N <- convert_vector_to_list(N)
    }
    n <- convert_vector_to_list(n, N)
  } else if (class_n == "vector") {
    if (class_N == "multiplier") N <- N * n
    class_N <- check_n_N_class(N)
    if (class_N %in% c("vector", "list with ranges")) {
      N <- convert_vector_to_list(N)
      sum_pop <- sapply(N, sum)
    }
    n <- convert_vector_to_list(n, N)
  }

  # Calculating useful arguments ==========================================
  n_levels <- length(n)
  if (!is.null(names(n))) {
    if (is.null(cluster_labels)) cluster_labels <- names(n)
    if (is.null(resp_labels)) resp_labels <- c(names(n)[-1], "respondent")
  }

  # Treating NAs in labels ================================================
  if (length(cluster_labels[!is.na(cluster_labels)]) < n_levels - 1) {
    cluster_labels[is.na(cluster_labels)] <- "unknowncluster"
  }
  if (length(resp_labels[!is.na(resp_labels)]) < n_levels) {
    resp_labels[is.na(resp_labels)] <- "unknownrespondent"
  }

  # Removing accents ======================================================
  if (!is.null(names(n))) names(n) <- iconv(names(n), to = "ASCII//TRANSLIT")
  cluster_labels <- iconv(cluster_labels, to = "ASCII//TRANSLIT")
  resp_labels    <- iconv(resp_labels, to = "ASCII//TRANSLIT")

  # Defining n_X and n_W ==================================================
  if (is.null(cat_prop)) {
    if (n_levels > 1 & separate_questionnaires) {
      if (length(n_X) == 1) n_X <- rep(n_X, n_levels)
      if (length(n_W) == 1 & class(n_W) == "numeric") {
        n_W <- rep(n_W, n_levels)
      } else if (length(n_W) > 1 & class(n_W) == "list") {
        if (any(sapply(n_W, class) == "list")) {
          n_W <- n_W
        } else {
          n_W <- rep(list(n_W), n_levels)
        }
      }
    }
    if (!is.null(cor_matrix) & is.null(n_X) & is.null(n_W)) {
      n_X <- sample(x = seq(0, ncol(cor_matrix)), size = 1)
      n_W <- ncol(cor_matrix) - n_X
    } else {
      if (is.null(n_X)) {
        if (length(rho) > 1) {
          n_X <- as.list(rep(length(rho), n_levels))
        } else {
          n_X <- gen_X_W_cluster(n_levels, separate_questionnaires,
                                class_cor = NULL)$n_X
        }
      }
      if (is.null(n_W)) {
        n_W <- gen_X_W_cluster(n_levels, separate_questionnaires, class_cor = NULL)$n_W
      }
    }
  }

  # Generating messages and data ==========================================
  if (separate_questionnaires) { # unique questionnaires at each level

    # Message explaining cluster scheme -----------------------------------
    if (verbose) {
      print(cli::rule(left = cli::col_blue("Hierarchical structure")))
      if (!census & print_pop_structure) {
        message("Population structure")
        draw_cluster_structure(N, cluster_labels, resp_labels)
        message("Sampled structure")
      }
      cluster_message(n, resp_labels, cluster_labels, n_levels,
                     separate_questionnaires, 1)
      draw_cluster_structure(n, cluster_labels, resp_labels)
    }

    # Questionnaire generation --------------------------------------------
    if (verbose & calc_weights) {
      print(cli::rule(left = cli::col_blue("Information on sampling weights")))
    }
    sample <- cluster_gen_separate(
      n_levels, n, N, sum_pop, calc_weights, sampling_method,
      cluster_labels, resp_labels, collapse,
      n_X, n_W, cat_prop, c_mean, sigma, cor_matrix, rho, verbose, ...
    )
  } else { # questionnaires administered only at the bottom level

    # Message explaining cluster scheme -----------------------------------
    if (verbose) {
      print(cli::rule(left = cli::col_blue("Hierarchical structure")))
      if (!census & print_pop_structure) {
        message("Population structure")
        draw_cluster_structure(N, cluster_labels, resp_labels)
        message("Sampled structure")
      }
      cluster_message(n, resp_labels, cluster_labels, n_levels,
                    separate_questionnaires, 2)
      draw_cluster_structure(n, cluster_labels, resp_labels)
    }

    # Questionnaire generation --------------------------------------------
    if (verbose & calc_weights) {
      print(cli::rule(left = cli::col_blue("Information on sampling weights")))
    }
    sample <- cluster_gen_together(
      n_levels, n, N, sum_pop, calc_weights, sampling_method,
      cluster_labels, resp_labels, collapse,
      n_X, n_W, cat_prop, c_mean, sigma, cor_matrix, rho, verbose, ...
    )
  }

  return(sample)
}