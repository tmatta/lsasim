#' Generate analytical parameters
#'
#' @param data output from the \code{questionnaire_gen} function for
#'   \code{full.output = TRUE}
#' @param vcov_yfz covariance matrix between Y, F and Z
#' @param Phi latent regression correlation matrix
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param prop_groups_1 vector with the proportions of the first (base) category
#'   of each categorical variable
#' @param MC if \code{TRUE}, perform Monte Carlo simulation to estimate
#'   regression coefficients
#' @param replications for \code{MC = TRUE}, this represents the number of Monte
#'   Carlo subsamples calculated.
#' @importFrom stats lm model.matrix quantile
#' @details The covariance matrix provided must have Y in the first row/column.
#' @export
#' @examples
#'
#' # Data containing only continuous variables
#' data1 <- questionnaire_gen(1000, family="gaussian", theta = TRUE,
#'                            full_output = TRUE, n_X = 3, n_W = 0)
#' beta_gen(data1, MC = TRUE)
#'
#' # Data containing only dichotomous variables
#' data2 <- questionnaire_gen(1000, family="gaussian", theta = TRUE,
#'                            full_output = TRUE, n_X = 0, n_W = list(2))
#' beta_gen(data2, MC = TRUE)
#'
#' # Data containing polychotomous variables
#' data3 <- questionnaire_gen(1000, family="gaussian", theta = TRUE,
#'                            full_output = TRUE, n_X = 0, n_W = list(2, 3))
#' \donttest{beta_gen(data3, MC = TRUE)}
beta_gen <- function(data, vcov_yfz, Phi, wcol_Phi, prop_groups_1, MC = FALSE,
                     replications = 100) {
  if (!data$theta) stop("Data must include theta")

  YXW <- data$bg[-1]  # remove "subject"
  XW <- YXW[-1]  # remove "theta" (and "subject", from before)
  if (is.null(data$c_mean)) {
    Y_mu <- 0
    XW_mu <- rep(0, length(XW))
  } else {
    Y_mu <- data$c_mean[1]
    XW_mu <- data$c_mean[-1]
  }

  if (data$n_W == 0) {
    # This is the easy case: all BG variables are continuous
    vcov <- data$cov_matrix
    calc_intercept <- function(Y, X, b) return(NULL)
  } else if (all(data$n_cats == 2)) {
    # This is the not so easy case: n_W > 0, but all Ws are binary. The W
    # variables will be dummy-coded. In this case, vcov doesn't change because
    # the base categories will be dropped anyway, and the covariances of the
    # second category are the same as
    vcov <- data$cov_matrix
    calc_intercept <- function(Y, X, b, pr1) return(Y - b %*% (1 - pr1))  # TODO: adapt to script
  } else {
    # Most complex case: n_W > 0 and n_W is polytomous
    stop("beta_gen for polytomous variables not yet implemented")
  }
  vcov_XW <- vcov[-1, -1]
  cov_YXW <- vcov[-1, 1, drop = FALSE]  # drop = FALSE keeps class as "matrix"
  beta_hat <- solve(vcov_XW, cov_YXW) # no intercept
  intercept <- calc_intercept(Y_mu, XW_mu, beta_hat, data$cat_prop[[2]][1])

  output <- c(intercept, beta_hat)

  if (MC) {
    message("Generating Monte Carlo coefficient estimates. Please wait...")
    boot_data <- list()
    for (r in seq(replications)) {
      unique_lvl <- TRUE
      while (unique_lvl) {
        boot_obs <- sample(rownames(data$bg), replace = TRUE)
        boot_data[[r]] <- YXW[boot_obs, ]
        unique_lvl <- any(sapply(boot_data[[r]], function(x) all(duplicated(x)[-1L])))
      }
    }
    boot_coef <- sapply(boot_data, function(x) lm(theta ~ ., x)$coefficients)
    boot_avg_coef <- apply(boot_coef, 1, mean)
    boot_CI <- apply(boot_coef, 1, function(x) quantile(x, c(.025, .975)))

    # Checking if cov_matrix estimates is contained in MC confidence interval
    cov_in_CI <- output > boot_CI[1, ] & output < boot_CI[2, ]

    output <- rbind(cov_matrix = output, MC = boot_avg_coef, boot_CI,
                    cov_in_CI = as.logical(cov_in_CI))
  }
  return(t(output))
}
