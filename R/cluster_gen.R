#' @title Generate cluster sample
#' @param n numeric vector with the number of sampled observations (clusters or subjects) on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers. It can also be "none", "partial" and "full" for finer control on 3+ levels
#' @param separate_questionnaires if `TRUE`, each level will have its own questionnaire
#' @param N list of numeric vector with the population size of each *sampled* cluster element on each level
#' @param calc_weights if `TRUE`, sampling weights are calculated
#' @param sum_pop total population at each level (sampled or not)
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param cat_prop list of cumulative proportions for each item. If \code{theta
#'   = TRUE}, the first element of \code{cat_prop} must be a scalar 1, which
#'   corresponds to the \code{theta}.
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level. Defaults to 0, but can change if `rho` is set.
#' @param sigma vector of standard deviations for the continuous variables or list of vectors for the continuous variables for each level. Defaults to 1, but can change if `rho` is set.
#' @param cor_matrix Correlation matrix between all variables (except weights). By default, correlations are randomly generated.
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param rho estimated intraclass correlation
#' @param theta if \code{TRUE}, the first continuous variable will be labeled
#'   'theta'. Otherwise, it will be labeled 'q1'.
#' @param verbose if `TRUE`, prints output messages
#' @param print_pop_structure if `TRUE`, prints the population hierarchical structure (as long as it differs from the sample structure)
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @return list with background questionnaire data, grouped by level or not
#' @details This function relies heavily in two subfunctions---`cluster_gen_separate` and `cluster_gen_together`---which can be called independently. This does not make `cluster_gen` a simple wrapper function, as it performs several operations prior to calling its subfunctions, such as randomly generating `n_X` and `n_W` if they are not determined by user.
#' `n` can have unitary length, in which case all clusters will have the same size.
#' `N` is *not* the population size across all elements of a level, but the population size for each element of one level.
#' Regarding the additional parameters to be passed to `questionnaire_gen()`, they can be passed either in the same format as `questionnaire_gen()` or as more complex objects that contain information for each cluster level.
#'
#' @note For the purpose of this function, levels are counted starting from the top nesting/clustering level. This means that, for example, schools are the first cluster level, classes are the second, and students are the third and final level. This behavior can be customized by naming the `n` argument or providing custom labels in `cluster_labels` and `resp_labels`.
#'
#' @note Manually setting both `c_mean` and `rho`, while possible, may yield unexpected results due to how those parameters work together. A high intraclass correlation (`rho`) teoretically means that each group will end up with different means so they can be better separated. If `c_mean` is left untouched (i.e., at the default value of zero), then `c_mean` will freely change between clusters in order to result in the expected intraclass correlation. For large samples, `c_mean` will in practice correspond to the grand mean across that level, as the means of each element will be different no matter the sample size.
#'
#' Moreover, if `c_mean`, `sigma` and `rho` are passed to the function, the means will be recalculated as a function of the other two parameters. The three are interdependent and cannot be passed simultaneously.
#'
#' If in addition to `rho` the user also determine different means for each level, the only way the math can check out is if the variance in each group becomes very high. For examples of this scenario and the one described in the previous paragraph, check out the final section of this page.
#'
#' @note The `ranges()` function should always be put inside a `list()`,as putting it inside a vector (`c()`) will cancel its effect. For more details, please read the documentation of the `ranges()` function.
#'
#' @note The only arguments that can be used to label each level are `n`, `N`, `cluster_labels` and `resp_labels`. Labeling other arguments such as `c_mean` and `cat_prop` has no effect on the final results, but it is a recommended way for users to keep track of which value corresponds to which element in a complex hierarchical structure.
#'
#' One of the extra arguments that can be passed by this function is `family`.
#' If \code{family == "gaussian"}, the questionnaire will be generated
#'   assuming that all the variables are jointly-distributed as a multivariate
#'   normal. The default behavior is \code{family == NULL}, where the data is
#'   generated using the polychoric correlation matrix, with no distributional
#'   assumptions.
#'
#' @seealso cluster_estimates cluster_gen_separate cluster_gen_together questionnaire_gen
#' @export
#' @examples
#' # Simple structure of 3 schools with 5 students each
#' cluster_gen(c(3, 5))
#'
#' # Complex structure of 2 schools with different number of students,
#' # sampling weights and custom number of questions
#' n <- list(3, c(20, 15, 25))
#' N <- list(5, c(200, 500, 400, 100, 100))
#' cluster_gen(n, N, n_X = 5, n_W = 2)
#'
#' # Condensing the output
#' set.seed(0); cluster_gen(c(2, 4))
#' set.seed(0); cluster_gen(c(2, 4), collapse=TRUE) # same, but in one dataset
#'
#' # Condensing the output: 3 levels
#' str(cluster_gen(c(2, 2, 1), collapse="none"))
#' str(cluster_gen(c(2, 2, 1), collapse="partial"))
#' str(cluster_gen(c(2, 2, 1), collapse="full"))
#'
#' # Controlling the intra-class correlation and the grand mean
#' x <- cluster_gen(c(5, 1000), rho = .9, n_X = 2, n_W = 0, c_mean = 10)
#' sapply(1:5, function(s) mean(x$school[[s]]$q1))  # means per school != 10
#' mean(sapply(1:5, function(s) mean(x$school[[s]]$q1))) # closer to c_mean
#'
#' # Making the intraclass variance explode by forcing "incompatible" rho and c_mean
#' x <- cluster_gen(c(5, 1000), rho = .5, n_X = 2, n_W = 0, c_mean = 1:5)
#' anova(x)
cluster_gen <- function(
  n,
  N = 1,
  cluster_labels = NULL,
  resp_labels = NULL,
  cat_prop = NULL,
  n_X = NULL,
  n_W = NULL,
  c_mean = NULL,
  sigma = NULL,
  cor_matrix = NULL,
  separate_questionnaires = TRUE,
  collapse = "none",
  sum_pop = sapply(N, sum),
  calc_weights = TRUE,
  sampling_method = "mixed",
  rho = NULL,
  theta = FALSE,
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

  whitelist <- NULL
  if (class_n == "select") {
    check_condition(class_N == "select", "N can't be select")
    check_condition(
      class_N == "multiplier",
      "if n is select, N must be a vector or a list"
      )
    if (is(N, "numeric")) N <- convert_vector_to_list(N)
    whitelist <- sample_from(N, n)
    n <- N
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
      # Repeating a non-null n_X and n_W to all levels
      n_X <- repeatXW(n_X, n_W, n_levels)$n_X
      n_W <- repeatXW(n_X, n_W, n_levels)$n_W
    }
    if (!is.null(cor_matrix) & is.null(n_X) & is.null(n_W)) {
      # Generating n_X and n_W
      if (is.null(c_mean) & is.null(sigma)) {
        n_X <- sample(x = seq(0, ncol(cor_matrix)), size = 1) - theta
      } else {
        n_X <- ifelse(
          is.null(sigma), length(c_mean) - theta, length(sigma) - theta
        )
      }
      n_W <- ncol(cor_matrix) - n_X
      # Repeating n_X and n_W across multiple levels
      n_X <- repeatXW(n_X, n_W, n_levels)$n_X
      n_W <- repeatXW(n_X, n_W, n_levels)$n_W
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
        n_W <- gen_X_W_cluster(
          n_levels, separate_questionnaires, class_cor = NULL
        )$n_W
        # Dropping levels of n_W to adjust them to the limits of c_mean
        # or cor_ matrix
        if (!is.null(c_mean)) {
          n_W <- sapply(
            seq_along(n_W),
            function(x) if (x > length(c_mean)) n_W[[x]] <- NULL
          )
        } else if (!is.null(cor_matrix)) {
          n_W <- sapply(
            seq_along(n_W),
            function(x) if (x > length(c_mean)) n_W[[x]] <- NULL
          )
        }
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
      }
      if (is.null(whitelist)) {
        message("Sampled structure")
        cluster_message(n, resp_labels, cluster_labels, n_levels,
                      separate_questionnaires, 1)
        draw_cluster_structure(n, cluster_labels, resp_labels)
      } else {
        whitelist_message(whitelist)
      }
    }

    # Questionnaire generation --------------------------------------------
    if (verbose & calc_weights) {
      print(cli::rule(left = cli::col_blue("Information on sampling weights")))
    }
    sample <- cluster_gen_separate(
      n_levels, n, N, sum_pop, calc_weights, sampling_method,
      cluster_labels, resp_labels, collapse,
      n_X, n_W, cat_prop, c_mean, sigma, cor_matrix, rho, theta,
      whitelist, verbose,
      ...
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
      if (is.null(whitelist)) {
        cluster_message(n, resp_labels, cluster_labels, n_levels,
                      separate_questionnaires, 2)
        draw_cluster_structure(n, cluster_labels, resp_labels)
      } else {
        whitelist_message(whitelist)
      }
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

  # Reclassifying object and returning it ---------------------------------
  if (is(sample, "list")) {
    class(sample) <- c("lsasimcluster", "list")
  } else {
    class(sample) <- c("lsasimcluster", "data.frame")
  }
  return(sample)
}

repeatXW <- function(n_X, n_W, n_levels) {
  # Repeats a non-null n_X and n_W to all levels
  if (length(n_X) == 1) n_X <- rep(n_X, n_levels)
  if (length(n_W) == 1 & is(n_W, "numeric")) {
    n_W <- rep(n_W, n_levels)
  } else if (length(n_W) > 1 & is(n_W, "list")) {
    if (any(sapply(n_W, class) == "list")) {
      n_W <- n_W
    } else {
      n_W <- rep(list(n_W), n_levels)
    }
  }
  return(list(n_X = n_X, n_W = n_W))
}
