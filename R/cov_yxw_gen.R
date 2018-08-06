#' Setup full YXW covariance matrix
#'
#' @param n_ind number of indicator variables
#' @param n_fac number of factor variables
#' @param Phi latent regression correlation matrix
#' @param n_z number of background variables
#' @param sd_z standard deviation of background variables
#' @param x_names names of X variables
#' @param w_names names of W variables
#' @param pr_grp_1 scalar or list of proportions of the first group
#' @param Lambda matrix containing the factor loadings
#'
cov_yxw_gen <- function(n_ind, n_z, x_names, w_names, Phi, n_fac, Lambda) {

  l_start <- cumsum(n_ind) - (n_ind - 1)
  l_end <- l_start + n_ind - 1

  n_yxw <- 1 + sum(n_ind) + n_z
  vcov_yxw <- matrix(NA, nrow = n_yxw, ncol = n_yxw)
  rownames(vcov_yxw) <- colnames(vcov_yxw) <- c("y", x_names, w_names)

  # Compute covariance matrix for factor indicators (X)
  # TODO: why are these called _x and not _f?
  Phi_x <- Phi[2:(n_fac + 1), 2:(n_fac + 1)]
  cov_x <- Lambda %*% Phi_x %*% t(Lambda)
  var_x <- Lambda ^ 2 %*% Phi_x + (1 - Lambda ^ 2)

  # Compute variances for factor indicators
  indicator_vars <- list()
  for (i in seq(n_ind)) {
    indicator_vars[[i]] <- var_x[l_start[i]:l_end[i], i]
  }
  diag(cov_x) <- unlist(indicator_vars)

  # Inserting the covariance matrix into vcov_yxw (Y and W cols remain NA)
  x_indices <- 2:(length(diag(cov_x)) + 1)
  vcov_yxw[x_indices, x_indices] <- cov_x

  # Compute covariance between Y and factor indicators (X)
  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[1, l_seq + 1] <- Lambda[l_seq, i] * Phi[1, (i + 1)]
    vcov_yxw[l_seq + 1, 1] <- t(vcov_yxw[1, l_seq + 1])
  }

  # Compute covariance between W and factor indicators (X)
  wcol_yxw <- match(w_names, colnames(vcov_yxw))
  wcol_Phi <- match(w_names, colnames(Phi))
  vcov_yxw[wcol_yxw, wcol_yxw] <- .5  # TODO: implement correl between Ws
  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[wcol_yxw, l_seq + 1] <- Lambda[l_seq, i] * Phi[wcol_Phi, (i + 1)]
    vcov_yxw[l_seq + 1, wcol_yxw] <- t(vcov_yxw[wcol_yxw, l_seq + 1])
  }

  # Compute covariance between Y and W
  vcov_yxw[1, wcol_yxw] <- vcov_yxw[wcol_yxw, 1] <- Phi[1, wcol_Phi]
  diag(vcov_yxw) <- 1

  return(vcov_yxw)
}
