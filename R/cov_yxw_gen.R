#' Setup full YXW covariance matrix
#'
#' @param n_ind number of indicator variables
#' @param n_fac number of factor variables
#' @param Phi latent regression correlation matrix
#' @param n_z number of background variables
#' @param Lambda matrix containing the factor loadings
#'
cov_yxw_gen <- function(n_ind, n_z, Phi, n_fac, Lambda) {
  n_ind_rep <- rep(n_ind, n_fac)  # number of indicators for each factor

  x_names <- paste0("x", 1:sum(n_ind_rep))
  f_names <- paste0("f", 1:length(n_ind_rep))
  w_names <- paste0("w", 1:n_z)

  rownames(Phi) <- colnames(Phi) <- c("y", f_names, w_names)

  l_start <- cumsum(n_ind_rep) - (n_ind_rep - 1)
  l_end <- l_start + n_ind_rep - 1

  n_yxw <- 1 + sum(n_ind_rep) + n_z
  vcov_yxw <- matrix(NA, nrow = n_yxw, ncol = n_yxw)
  rownames(vcov_yxw) <- colnames(vcov_yxw) <- c("y", x_names, w_names)

  # Generating or formatting factor-loading matrix (Lambda) ---------------
  if (class(Lambda) %in% c("numeric", "integer")) {
    # "Lambda" parameter was provided as limits for random genration
    Lambda <- lambda_gen(n_ind, n_fac, Lambda, x_names, f_names)
  } else {
    # "Lambda" parameter was provided as the actual matrix.
    dimnames(Lambda) <- list(x_names, f_names)
  }

  # Compute covariance matrix for factor indicators (X)
  Phi_f <- Phi[2:(n_fac + 1), 2:(n_fac + 1)]
  cov_x <- Lambda %*% Phi_f %*% t(Lambda)
  var_xf <- Lambda ^ 2 %*% Phi_f + (1 - Lambda ^ 2)

  # Compute variances for factor indicators
  indicator_vars <- list()
  for (i in seq(n_ind_rep)) {
    indicator_vars[[i]] <- var_xf[l_start[i]:l_end[i], i]
  }
  diag(cov_x) <- unlist(indicator_vars)

  # Inserting the covariance matrix into vcov_yxw (Y and W cols remain NA)
  x_indices <- 2:(length(diag(cov_x)) + 1)
  vcov_yxw[x_indices, x_indices] <- cov_x

  # Compute covariance between Y and factor indicators (X)
  for (i in seq(n_ind_rep)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[1, l_seq + 1] <- Lambda[l_seq, i] * Phi[1, (i + 1)]
    vcov_yxw[l_seq + 1, 1] <- t(vcov_yxw[1, l_seq + 1])
  }

  # Compute covariance between W and factor indicators (X)
  wcol_yxw <- match(w_names, colnames(vcov_yxw))
  wcol_Phi <- match(w_names, colnames(Phi))
  for (i in seq(n_ind_rep)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[wcol_yxw, l_seq + 1] <- Lambda[l_seq, i] * Phi[wcol_Phi, (i + 1)]
    vcov_yxw[l_seq + 1, wcol_yxw] <- t(vcov_yxw[wcol_yxw, l_seq + 1])
  }

  # Compute covariance between Y and W
  vcov_yxw[1, wcol_yxw] <- vcov_yxw[wcol_yxw, 1] <- Phi[1, wcol_Phi]
  diag(vcov_yxw) <- 1

  return(vcov_yxw)
}
