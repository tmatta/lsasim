#' Wrapper-functions for check_condition
#' @description functions to save space in ther parent functions by moving the validation checks here
#' @param n_cats vector with number of categories for each categorical variable
#'   (W)
#' @param n_vars number of variables (Y, X and W)
#' @param n_X number of continuous background variables (X)
#' @param n_W number of categorical variables (W)
#' @param theta is there a latent variable (Y)?
#' @param cat_prop list of vectors with the cumulative proportions of the
#'   background variables
#' @param cor_matrix correlation matrix of YXW
#' @param cov_matrix covariance matrix of YXW
#' @param c_mean vector of means of all variables (YXW)
#' @param c_sd vector of standard deviations of all variables (YXW)
#'
validate_questionnaire_gen <- function(
  n_cats, n_vars, n_X, n_W, theta, cat_prop, cor_matrix, cov_matrix,
  c_mean, c_sd
) {

  # Conditions involving number and quality of covariates -------------
  check_condition(any(n_cats == 1),
                  "the number of categories in n_W must all be greater than 1")
  check_condition(n_vars != n_X + n_W + theta,
                  "n_vars must equal n_X + n_W + theta")
  check_condition(n_X == 0 & n_W == 0,
                  "At least one background variable must be generated")
  if (!is.null(n_X) & !is.null(n_W)) {
    if (n_W > 1) {
      check_condition(
        n_X + n_W + theta != ncol(cov_matrix),
        paste0(
          "\nn_X + n_W + theta must not be different from ncol(cov_matrix).",
          " The former add up to",
          n_X + n_W + theta,
          ", whereas the latter equals ",
          ncol(cov_matrix), ".",
          "\n\nIf you are passing a matrix, try manually setting n_X, n_W\n",
          "and theta so that the number of variables match the matrix rank."
        )
      )
      check_condition(
        n_X + n_W + theta != ncol(cor_matrix),
        paste0("n_X + n_W + theta must not be different from ncol(cor_matrix).",
          "\nThe former add up to ",
          n_X + n_W + theta,
          ", whereas the latter equals ",
          ncol(cor_matrix), ".",
          "\n\nIf you are passing a matrix, try manually setting n_X, n_W\n",
          "and theta so that the number of variables match the matrix rank."
        )
      )
    }
  }
  check_condition(n_vars > ncol(cov_matrix),
                  "n_vars must not exceed ncol(cov_matrix)")
  check_condition(n_vars > ncol(cor_matrix),
                  "n_vars must not exceed ncol(cor_matrix)")
  check_condition(!is.null(cat_prop) & !is.null(n_vars) &
                    length(cat_prop) != n_vars,
                  "n_vars must be NULL or equal to length(cat_prop)")
  check_condition(any(sapply(lapply(cat_prop, diff), function(x) any(x < 0))),
                  "The elements of cat_prop must be non-decreasing")
  check_condition(any(sapply(cat_prop, function(x) any(x > 1))),
                  "cat_prop must not contain values above 1")
  check_condition(any(sapply(cat_prop, max) != 1),
                  "last value of each element of cat_prop must be 1")
  check_condition(length(c_mean) > n_X + theta,
                  "length(c_mean) cannot be larger than n_X + theta")
  check_condition(length(c_sd) > n_X + theta,
                  "length(c_sd) cannot be larger than n_X + theta")
  check_condition(length(c_mean) > 1 & length(c_mean) != n_X + theta,
                  "c_mean recycled to fit all continuous variables", FALSE)
  check_condition(length(c_sd) > 1 & length(c_sd) != n_X + theta,
                  "c_sd recycled to fit all continuous variables", FALSE)

  # Conditions involving the covariance or correlation matrices -------
  check_condition(ncol(cov_matrix) > 0 & ncol(cor_matrix) > 0,
                  "Only one matrix (cov_matrix or cor_matrix) may be provided")
  check_condition(any(cor_matrix > 1), "Improper correlation matrix")
  if (!is.null(cor_matrix)) {
    check_condition(!isSymmetric(cor_matrix), "cor_matrix is not symmetric")
    check_condition(
      any(eigen(cor_matrix)$values < 0),
      paste(
        "Improper correlation matrix passed.",
        "Make sure all eigenvalues are non-negative.\n",
        "The eigenvalues of the matrix passed are as follows:\n",
        paste0(eigen(cor_matrix)$values, collapse=", "),
        "\n\nFor more information, use the eigen() function.",
        "\n\nThis can usually be fixed by adjusting the matrix correlations",
        "to avoid inconsistencies."
      )
    )
  }
  if (!is.null(cov_matrix)) {
    check_condition(!isSymmetric(cov_matrix), "cov_matrix is not symmetric")
    check_condition(min(diag(cov_matrix)) < 0,
                    "cov_matrix cannot contain negative values in its diagonal.")
    check_condition(abs(max(cov2cor(cov_matrix))) > 1,
                    "cov_matrix is possibly invalid, because it generates correlations above 1", FALSE)
    check_condition(!is.null(c_sd),
                    "cov_matrix was provided, so c_sd was ignored", FALSE)
  } else {
    check_condition(any(c_sd < 0), "c_sd may not contain negative elements")
  }
  if (!is.null(cat_prop)) {
    check_condition(theta & (cat_prop[[1]][1] != 1),
                    "theta == TRUE, so the first element of cat_prop must be 1")
    check_condition(identical(cat_prop, list(1)) & theta,
                    "At least one background variable must be generated")
    check_condition(length(cat_prop) != ncol(cor_matrix),
                    "length(cat_prop) cannot be different from ncol(cor_matrix)")
    check_condition(length(cat_prop) != ncol(cov_matrix),
                    "length(cat_prop) cannot be different from ncol(cov_matrix)")
    check_condition(!is.null(n_X) | !is.null(n_W),
                    "cat_prop was provided, so n_X and n_W were ignored", FALSE)
    numYX <- sum(sapply(cat_prop, function(x) x[1] == 1))
    check_condition(length(c_mean) > numYX,
                    "length(c_mean) cannot be larger than the number of continuous variables")
    check_condition(length(c_sd) > numYX,
                    "length(c_sd) cannot be larger than the number of continuous variables")
    check_condition(length(c_mean) > numYX,
                    "c_mean recycled to fit all continuous variables", FALSE)
    check_condition(length(c_sd) > numYX,
                    "c_sd recycled to fit all continuous variables", FALSE)
  }
}

validate_cluster_gen <- function(
  n, N, cluster_labels, resp_labels, n_X, n_W, rho, sigma, c_mean,
  separate_questionnaires, collapse, sampling_method) {
  check_condition(
    (!is.null(names(n)) | !is.null(names(N))) &
    (!is.null(cluster_labels) | !is.null(resp_labels)),
    "If n or N are labeled, cluster_labels and resp_labels must be left NULL."
  )
  check_condition(
    !separate_questionnaires & length(n_X) > 1,
    "Unique questionnaire requested. n_X must therefore be a scalar."
  )
  check_condition(
    !separate_questionnaires & length(n_W) > 1,
    "Unique questionnaire requested. n_W must therefore be a scalar or a list."
  )
  check_condition(
    !separate_questionnaires &
      (class(rho) == "list" | class(c_mean) == "list" | class(sigma) == "list"),
    "Unique questionnaire requested. rho, c_mean and c_sd must not be lists."
  )
  check_condition(
    !is.null(rho) & !is.null(sigma) & !is.null(c_mean),
    paste(
      "rho and sigma were defined. The three parameters are interdependent",
      "and should not be passed simultaneously.",
      "By design, c_mean will be ignored and recalculated."
    ),
    FALSE
  )
  check_condition(
    is(c_mean, "list") & (length(c_mean) == length(n)),
    paste(
      "c_mean must have one fewer levels than n.",
      "Please keep in mind that the first (top_) level of n is a nesting level"
    )
  )
  check_condition(
    is(sigma, "list") & (length(c_mean) == length(n)),
    paste(
      "sigma must have one fewer levels than n.",
      "Please keep in mind that the first (top_) level of n is a nesting level"
    )
  )
  check_condition(length(n) == 1, "n must have length longer than 1")
  check_condition(
    length(n) > length(cluster_labels) + 1 & !is.null(cluster_labels),
    "cluster_labels has insufficient length."
  )
  check_condition(
    !separate_questionnaires & collapse == "partial",
    paste("Partial collapsing unavailable for unique questionnaires.",
          "Treated as 'full'."), FALSE
  )
  check_condition(
    !(all(sampling_method %in% c("SRS", "PPS", "mixed"))),
    "Invalid sampling method."
  )
  check_condition(
    class(n) == "select" & class(N) == "select",
    "If n is select, N must be explicitly defined."
  )
  check_condition(
    !is.null(rho) & any(sapply(sigma, class) == "list"),
    paste("If rho is provided, sigma must be the same for all PSUs in a level",
          "(i.e., sigma must not contain a list within a list)")
  )
  check_condition(
    !is.null(n_W) & (length(n_W) > length(n) - 1) & class(n_W) != "list",
    "length(n_W) cannot be larger than length(n) - 1"
  )
}