#' Generate analytical parameters
#'
#' @param data output from the \code{questionnaire_gen} function for
#'   \code{full_output = TRUE}
#' @param vcov_yfz covariance matrix between Y, F and Z
#' @param MC if \code{TRUE}, perform Monte Carlo simulation to estimate
#'   regression coefficients
#' @param replications for \code{MC = TRUE}, this represents the number of Monte
#'   Carlo subsamples calculated.
#' @param analytical if \code{TRUE}, an analytical solution using the covariance
#'   matrix will be calculated.
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
#' # Data containing only polychotomous variables
#' data3 <- questionnaire_gen(1000, family="gaussian", theta = TRUE,
#'                            full_output = TRUE, n_X = 0, n_W = list(3, 5))
#' \donttest{beta_gen(data3, MC = TRUE)}
beta_gen <- function(data, vcov_yfz, Phi, wcol_Phi, prop_groups_1, MC = FALSE,
                     replications = 100, analytical = TRUE) {
  if (!missing(vcov_yfz)) {
    vcov <- vcov_yfz
    calc_intercept <- function(Y, X, b, pr1) return(Y - (b %*% 1 - pr1))  # TODO: from script. Parenthesis OK?
    Y_mu <- XW_mu <- 0
  } else {
    if (!data$theta) stop("Data must include theta")

    YXW <- data$bg[-1]  # remove "subject"
    XW <- YXW[-1]  # remove "theta" (and "subject", from before)


    # Checking for means --------------------------------------------------
    if (is.null(data$c_mean)) {
      Y_mu <- 0
      XW_mu <- rep(0, length(XW))
    } else {
      Y_mu <- data$c_mean[1]
      X_mu <- data$c_mean[-1]
      W_mu <- sapply(data$cat_prop_W_p, function(x) 1 - x[1])
      XW_mu <- unlist(c(X_mu, W_mu))
    }

    # Retrieving covariance matrix ----------------------------------------
    model_mx <- model.matrix(theta ~ ., data = YXW)
    cov_YXW <- cov(model_mx, YXW$theta)[-1]
    vcov_XW <- cov(model_mx)[-1, -1]
  }
  if (analytical) {
    beta_hat <- solve(vcov_XW, cov_YXW) # no intercept
    intercept <- Y_mu - crossprod(beta_hat, XW_mu)
    output <- c(intercept, beta_hat)
  } else {
    output <- NA
  }
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
    boot_CI <- apply(boot_coef, 1, function(x) quantile(x, c(.005, .995)))

    # Checking if cov_matrix estimates is contained in MC confidence interval
    cov_in_CI <- output > boot_CI[1, ] & output < boot_CI[2, ]

    output <- rbind(cov_matrix = output, MC = boot_avg_coef, boot_CI,
                    cov_in_CI = as.logical(cov_in_CI))
  }
  return(t(output))
}
