#' Generate analytical parameters
#'
#' @param data output from the \code{questionnaire_gen} function for
#'   \code{full.output = TRUE}
#' @param vcov_yfz covariance matrix between Y, F and Z
#' @param Phi latent regression correlation matrix
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param MC if \code{TRUE}, perform Monte Carlo simulation to estimate
#'   regression coefficients
#' @param replications for \code{MC = TRUE}, this represents the number of
#'   Monte Carlo subsamples calculated.
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
#' beta_gen(data3, MC = TRUE)
beta_gen <- function(data, vcov_yfz, Phi, wcol_Phi, MC = FALSE,
                     replications = 1000) {
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
    calc_intercept <- function(Y_mu, XW_mu, beta_hat) return(NULL)
  } else if (all(data$n_cats == 2)) {
    # This is the not so easy case: n_W > 0, but all Ws are binary. The W
    # variables will be dummy-coded

    # XW_split <- lapply(XW, function(x) model.matrix(~ x, XW))
    # XW_mu_split <- lapply(XW_split, function(x) colMeans(x)[-1])  # no intercept
    # XW_mu <- unlist(XW_mu_split)
    # In this case, vcov doesn't change because the base categories will be
    # dropped anyway, and the covariances of the second category are the same as
    vcov <- data$cov_matrix

    # Group differences
    # cols_W <- which(substr(rownames(vcov), 1, 1) == "W")
    # # beta_z <- solve(vcov[cols_W, cols_W], vcov[1, cols_W])  # identical to beta_hat
    # # prop_cat_1 <- sapply(data$cat_prop[cols_W], function(x) x[1])
    # # # beta_c <- as.numeric(0 - (beta_z %*% 1 - prop_cat_1))
    # # beta_c <- as.numeric(0 - (1 %*% beta_z - prop_cat_1))
    # # #TODO: check if beta_c is equal to -boot_avg_coef
    # # extra_parms <- c(Z0 = beta_c, Z1 = beta_z)
    # vcov_XW <- vcov[-1, -1]
    # cov_YXW <- vcov[1, -1]
    # beta_hat <- solve(vcov_XW, cov_YXW) # no intercept
    # prop_cat_1 <- sapply(data$cat_prop[cols_W], function(x) x[1])
    # beta_c <- -(1 %*% beta_hat - prop_cat_1)  # adjusts beta for cat_prop
    # #TODO: check if beta_c is equal to -boot_avg_coef
    # beta_hat <- beta_c
    calc_intercept <- function(Y_mu, XW_mu, beta_hat) {
      return(Y_mu - crossprod(beta_hat, XW_mu))
    }
  } else {
    # Most complex case: n_W > 0 and n_W is polytomous
    stop("beta_gen for polytomous variables not yet implemented")
    # cols_W_expanded <- seq(unlist(sapply(data$n_cats, seq))) + cols_W - 1
    # cols_vcov_expanded <- cols_W_expanded[length(cols_W_expanded)]
    # # expanding the covariance matrix
    # # Duplicate rows/cols -> singular matrix
    # # TODO: use cat_prop as weight to adjust covariances?
    # # TODO: Drop base level of W.
    # # TODO: generalize for any(n_cats != 2)
    # vcov <- data$cov_matrix
    # vcov_expanded <- matrix(nrow = cols_vcov_expanded, ncol = cols_vcov_expanded)
    # rownames(vcov) <- c("Y", paste0("X", seq(data$n_X)), paste0("W", seq(data$n_W)))
    # colnames(vcov) <- rownames(vcov)
    # rownames(vcov_expanded) <- c("Y", paste0("X", seq(data$n_X)),
    #                              paste0("W", seq(data$n_W), seq(cols_W_expanded)))
    # colnames(vcov_expanded) <- rownames(vcov_expanded)
    #
    # # diagonal elements of the variance-covariance matrix
    # vars_X <- diag(data$cov_matrix)[cols_X]
    # vars_W_expanded <- unlist(sapply(data$n_cats, function(x) rep(diag(data$cov_matrix)[cols_W], x)))
    # vcov_expanded[1, 1] <- data$cov_matrix[1, 1]
    # diag(vcov_expanded[-1, -1]) <- c(vars_X, vars_W_expanded)
    #
    # # Off-diagonal elements involving Y and X
    # cols_YX <- c(col_Y, cols_X)
    # vcov_expanded[cols_YX, cols_YX] <- vcov[cols_YX, cols_YX]
    #
    # # Off-diagonal elements involving W
    # vcov_W_NA <- is.na(vcov_expanded[cols_W_expanded, cols_W_expanded])
    # vcov_expanded[cols_W_expanded, cols_W_expanded][vcov_W_NA] <- -vcov_expanded[cols_W_expanded, cols_W_expanded][!vcov_W_NA]
    # vcov[c(cols_X, cols_W), c(cols_X, cols_W)]
    #
    # vcov_expanded[-1, 1] <- vcov_expanded[1, -1]  # TODO: generalize for n_W > 1
    # vcov_expanded[2, 3] <- -vcov_expanded[2, 2]  # TODO: generalize for n_W > 1
    # vcov_expanded[3, 2] <- vcov_expanded[2, 3]  # TODO: generalize for n_W > 1
    # vcov_expanded <- vcov_expanded[c(1, 3), c(1, 3)]  # TODO: generalize for n_W > 1
  }
  vcov_XW <- vcov[-1, -1]
  cov_YXW <- vcov[-1, 1, drop = FALSE]  # drop = FALSE keeps class as "matrix"
  beta_hat <- solve(vcov_XW, cov_YXW) # no intercept
  intercept <- calc_intercept(Y_mu, XW_mu, beta_hat)

  output <- c(intercept, beta_hat)

  if (MC) {
    message("Generating Monte Carlo coefficient estimates. Please wait...")
    reps <- replications
    boot_obs <- replicate(reps, sample(rownames(data$bg), replace = TRUE))
    boot_data <- list()
    for (r in seq(reps)) boot_data[[r]] <- YXW[boot_obs[, r], ]
    boot_coef <- sapply(boot_data, function(x) lm(theta ~ .- 1, x)$coefficients)
    boot_avg_coef <- apply(boot_coef, 1, mean)
    boot_CI <- apply(boot_coef, 1, function(x) quantile(x, c(.025, .975)))

    # Checking if cov_matrix estimates is contained in MC confidence interval
    cov_in_CI <- output > boot_CI[1, ] & output < boot_CI[2, ]

    output <- rbind(cov_matrix = output, MC = boot_avg_coef, boot_CI,
                    cov_in_CI = as.logical(cov_in_CI))
  }
  return(t(output))
}
