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
  if (class(data) != "list") {
    stop("Data must be generated with full_output = TRUE")
  }

  # Basic data subsetting -------------------------------------------------
  YXW <- data$bg[-1]  # remove "subject"
  XW <- YXW[-1]  # remove "theta" (and "subject", from before)

  # Identifying and labeling elements -------------------------------------
  names_X <- names_W <- names_Z <- NULL
  if (data$n_X > 0) names_X <- paste0("x", 1:data$n_X)
  if (data$n_W > 0) {
    names_W <- paste0("w", 1:data$n_W)
    names_Z <- paste0("z", 1:data$n_W)
  }
  names_YXZ <- c("theta", names_X, names_Z)
  dimnames(data$cov_matrix) <- list(names_YXZ, names_YXZ)
  names(data$cat_prop_W_p) <- names(data$cat_prop_W) <- names_W
  names(data$c_sd) <- names(data$c_mean) <- names(data$cat_prop_YX) <- c("theta", names_X)


  # Defining parameters for Y, X, Z and W ---------------------------------
  if (is.null(data$c_mean)) {
    Y_mu <- 0
    XW_mu <- rep(0, length(XW))
  } else {
    Y_mu <- data$c_mean[1]
    X_mu <- data$c_mean[-1]
    W_mu <- data$cat_prop_W_p
    W_mu_minus_1 <- unlist(sapply(W_mu, function(w) w[-1]))
    XW_mu <- unlist(c(X_mu, W_mu))
    XW_mu_minus_1 <- unlist(c(X_mu, W_mu_minus_1))
  }

  Z_mu <- 0
  # Y_var <- data$c_sd[1] ^ 2
  # Y_sd <- sqrt(Y_var)
  X_sd <- data$c_sd[-1]

  if (data$n_W > 0) {
    W_var <- lapply(W_mu, function(p) p * (1 - p))
    # W_sd <- sapply(W_var, function(x) sqrt(x)[1])  # not used!?
    Z_sd <- 1
    cov_YZ <- data$cov_matrix["theta", names_Z, drop = FALSE]
  }
  # cov_YX <- data$cov_matrix["theta", names_X, drop = FALSE]
  cov_XZ <- data$cov_matrix[names_X, names_Z, drop = FALSE]

  # Calculating elements for YXW covariance matrix (provided was XYZ) -----
  if (data$n_W > 0) {
    q_Z <- lapply(data$cat_prop_W, function(w) qnorm(c(0, w), Z_mu, Z_sd))

    exp_YW <- exp_AB(names_W, Y_mu, W_mu, cov_YZ, q_Z)
    cov_YW <- cov_AB(names_W, exp_YW, Y_mu, W_mu)

    cov_XW <- exp_XW <- list()
    for (x in names_X) {
      exp_XW[[x]] <- exp_AB(names_W, Z_mu, W_mu, cov_XZ[x, ], q_Z, X_mu[[x]], X_sd[x])
      cov_XW[[x]] <- cov_AB(names_W, exp_XW[[x]], X_mu[[x]], W_mu)
    }

    # Covariance matrix of the categories of Z
    vcov_W <- sapply(names_W, function(w) create_vcov_w(W_mu[[w]], W_var[[w]]))
  }

  # Final assembly of YXW covariance matrix -------------------------------
  vcov_order <- length(data$c_mean) + sum(sapply(W_mu, function(w) length(w) - 1))
  vcov_YXW <- matrix(nrow = vcov_order, ncol = vcov_order)
  names_W_extended <- names_W  #TODO: expand with category numbers
  names_YXW_extended <- c("theta", names_X, names_W_extended)
  dimnames(vcov_YXW) <- list(names_YXW_extended, names_YXW_extended)

  # Everything that doesn't involve W
  z_cols <- match(names_Z, rownames(data$cov_matrix))
  vcov_YXW[c("theta", names_X), c("theta", names_X)] <- data$cov_matrix[-z_cols, -z_cols]

  # Cov(Y, W)
  vcov_YXW["theta", names_W_extended] <- vcov_YXW[names_W_extended, "theta"] <- cov_YW

  # Cov(X, W)
  vcov_YXW[names_X, names_W_extended] <- vcov_YXW[names_W_extended, names_X] <- unlist(cov_XW)
  # Cov (Wi, Wi)
  vcov_YXW[names_W_extended, names_W_extended] <- vcov_W  # Cov(Wi, Wi)
  # vcov_YXW <- vcov_YXW[-2, -2]  # remove category one to avoid collinearity

  # Calculating regression parameters -----------------------------------
  beta_hat <- solve(vcov_YXW[-1, -1], vcov_YXW[1, -1])

  intercept <- Y_mu - crossprod(beta_hat, XW_mu_minus_1)
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
