#' Generate analytical parameters
#'
#' @param data output from the \code{questionnaire_gen} function for
#'   \code{full_output = TRUE}
#' @param MC if \code{TRUE}, perform Monte Carlo simulation to estimate
#'   regression coefficients
#' @param replications for \code{MC = TRUE}, this represents the number of Monte
#'   Carlo subsamples calculated.
#' @importFrom stats lm model.matrix quantile cov
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
beta_gen <- function(data, MC = FALSE, replications = 100, analytical = TRUE) {
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
    W_mu <- data$cat_prop_W_p
    Z_mu <- 0
    XW_mu <- unlist(c(X_mu, W_mu))

    Y_var <- data$c_sd[1] ^ 2
    if (data$n_W > 0) {
      W_var <- sapply(W_mu, function(p) p * (1 - p))
      W_sd <- sapply(W_var, function(x) sqrt(x)[1])
      Z_var <- Z_sd <- 1
      cov_YZ <- data$cov_matrix[1, 2]
    }
  }
  if (data$n_W > 0) {
    # Assembling covariance matrix YXW (provided was XYZ) ---------------
    # REMEMBER CONVERSION: (test code -> production):
    # X -> Y
    # Y -> Z
    # Z -> W
    q_Z <- lapply(data$cat_prop_W, function(w) qnorm(c(0, w), 0, 1))
    exp_YW <- 0
    for (i in seq_along(W_mu[[1]])) {
      # TODO: generalize from [[1]] to as many W as there are
      exp_YW[i] <- W_mu[[1]][i] * exp_A_given_B(q_Z[i], q_Z[i + 1],
                                                Y_mu, 0, 1, cov_YZ)
    }
    cov_YW <- exp_YW - Y_mu * W_mu[[1]]

    # Covariance matrix of the categories of Z
    vcov_W <- tcrossprod(W_mu[[1]], -W_mu[[1]])
    diag(vcov_W) <- W_var
  }

  # Final assembly of true covariance matrix
  vcov_YW <- matrix(nrow = length(W_mu[[1]]) + 1, ncol = length(W_mu[[1]]) + 1)
  vcov_YW[1, ] <- vcov_YW[, 1] <- c(Y_var, cov_YW)
  vcov_YW[-1, -1] <- vcov_W
  vcov_YW <- vcov_YW[-2, -2]  # remove category one to avoid collinearity

  # Calculating regression parameters -----------------------------------
  beta_hat <- solve(vcov_YW[-1, -1], vcov_YW[1, -1])
  intercept <- Y_mu - crossprod(beta_hat, W_mu[[1]][-1])
  output <- c("(Intercept)" = intercept, beta_hat)
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
