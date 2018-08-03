#' Generation of covariance matrices
#'
#' Construct covariance matrices for the generation of simulated test data.
#'
#' @param pr_grp_1 proportion of observations in group 1. Can be a scalar or a
#'   vector
#' @param n_fac number of factors
#' @param n_ind number of indicators per factor
#' @param Lambda either a matrix containing the factor loadings or a vector
#'   containing the lower and upper limits for a randomly-generated Lambda
#'   matrix
#' @return A list containing three covariance matrices: vcov_yxw, vcov_yxz and
#'   vcov_yfz
#' @export
#' @examples
#'  vcov <- cov_gen(pr_grp_1 = .5, n_fac = 3, n_ind = 2)
#'  str(vcov)
cov_gen <- function(pr_grp_1, n_fac, n_ind, Lambda = 0:1) {

  # TODO: implement cat_prop to define number of Ws and Xs.
  # TODO: split cov_gen? vcov_yxw_gen, vcov_yxz_gen, vcov_yfz_gen, beta_gen.

  # General parameters ----------------------------------------------------
  pr_grp_1 <- sapply(pr_grp_1, function(x) x[1])  # TODO: workaround. REMOVE
  pr_grp_2 <- 1 - pr_grp_1  # proportion of observations in group 2
  # TODO: generalize as list with any number of categories
  var_z <- pr_grp_1 * pr_grp_2  # variance of dichotomous variable z
  sd_z  <- sqrt(var_z)
  n_z   <- length(sd_z)  # number of background variables
  n_ind <- rep(n_ind, n_fac)  # number of indicators for each factor
  f_names <- paste0("f", 1:length(n_ind))
  x_names <- paste0("x", 1:sum(n_ind))
  w_names <- paste0("w", 1:n_z)

  # Location markers for the lambda matrix
  n_ind_minus1 <- n_ind - 1
  l_start <- cumsum(n_ind) - n_ind_minus1
  l_end <- l_start + n_ind - 1

  # Factor loading matrix (loadings generated randomly) -------------------
  if (class(Lambda) %in%  c("numeric", "integer")) {
    Lambda_mx <- matrix(0, ncol = length(n_ind), nrow = sum(n_ind))
    for (i in seq(n_ind)) {
      Lambda_mx[l_start[i]:l_end[i], i] <- runif(n_ind[i], Lambda[1],
                                                 Lambda[2])
    }
    Lambda <- Lambda_mx
  }
  colnames(Lambda) <- f_names
  rownames(Lambda) <- x_names

  # Generation covariance matrix between y, x1, ..., x36, w ---------------
  # w ~ N(0, 1) is the latent representation of dicotomous z
  # TODO: are W and Z switched from paper!
  # converts pt. biserial correlations to biserial correlations
  n_yfw <- 1 + n_fac + n_z  # 1 for y
  Phi <- cor_gen(n_yfw)  # latent regression correlation matrix
  # TODO: check if names below are correct
  rownames(Phi) <- colnames(Phi) <- c("y", f_names, w_names)

  # Setup full YXW covariance matrix --------------------------------------
  # yxw is the covariance between y, x1, ..., x36; w, W ~ N(0, 1)
  n_yxw <- 1 + sum(n_ind) + n_z
  vcov_yxw <- matrix(NA, nrow = n_yxw, ncol = n_yxw)
  rownames(vcov_yxw) <- colnames(vcov_yxw) <- c("y", x_names, w_names)

  # Compute covariance matrix for factor indicators
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
  vcov_yxw[1, 1] <- 1  # covariance between Y and itself
  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[1, l_seq + 1] <- Lambda[l_seq, i] * Phi[1, (i + 1)]
    vcov_yxw[l_seq + 1, 1] <- t(vcov_yxw[1, l_seq + 1])
  }

  # Compute covariance between W and factor indicators (X)
  wcol_yxw <- match(w_names, colnames(vcov_yxw))
  wcol_Phi <- match(w_names, colnames(Phi))
  vcov_yxw[wcol_yxw, wcol_yxw] <- 1
  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[wcol_yxw, l_seq + 1] <- Lambda[l_seq, i] * Phi[wcol_Phi, (i + 1)]
    vcov_yxw[l_seq + 1, wcol_yxw] <- t(vcov_yxw[wcol_yxw, l_seq + 1])
  }

  # Compute covariance between y and w
  vcov_yxw[1, wcol_yxw] <- vcov_yxw[wcol_yxw, 1] <- Phi[1, wcol_Phi]

  # Analytical covariance matrix ------------------------------------------
  # yxz is the covariance between y, x1, ..., x36; z
  # using var(z) = pq and point biserial correlations
  vcov_yxz <- matrix(NA, nrow = n_yxw, ncol = n_yxw)
  wcol_yxz <- wcol_yxw
  yfcol_yxz <- colnames(vcov_yxw)[-match(w_names, colnames(vcov_yxw))]

  # Filling vcov_yxz with values from vcov_yxw for Y and F
  dimnames(vcov_yxz) <- dimnames(vcov_yxw)
  vcov_yxz[yfcol_yxz, yfcol_yxz] <- vcov_yxw[yfcol_yxz, yfcol_yxz]

  # Adding Z to the covariance matrix
  ## Between Zs
  vcov_yxz[wcol_yxz, wcol_yxz] <- var_z
  ## Between Zs and Y
  cor_ptbis <- list()
  for (i in seq(length(n_ind) + 1)) {
    cor_ptbis[[i]] <- pt_bis_conversion(Phi[wcol_Phi, i], pr_grp_1)
  }
  cor_ptbis <- unlist(cor_ptbis)
  vcov_yxz[1, wcol_yxz] <- vcov_yxz[wcol_yxz, 1] <- cor_ptbis[1] * sd_z
  ## Between Zs and Xs
  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxz[wcol_yxz, l_seq + 1] <- Lambda[l_seq, i] * cor_ptbis[(i + 1)] * sd_z
    vcov_yxz[l_seq + 1, wcol_yxz] <- Lambda[l_seq, i] * cor_ptbis[(i + 1)] * sd_z
  }

  # Latent regression covariance matrix -----------------------------------
  # yfz is the covariance between y, f1, ..., f9, z; where f is a factor
  sd_vec <- c(rep(1, max(wcol_Phi) - n_z), sd_z)
  yfcol_yfz <- colnames(Phi)[-match(w_names, colnames(Phi))]
  Phi_pb <- Phi
  Phi_pb[wcol_Phi, yfcol_yfz] <- cor_ptbis  # Y and Fs vs. Ws
  Phi_pb[yfcol_yfz, wcol_Phi] <- t(Phi_pb[wcol_Phi, yfcol_yfz])

  vcov_yfz <- diag(sd_vec) %*% Phi_pb %*% diag(sd_vec)

  # Analytical parameters -------------------------------------------------

  # Latent regression parameters
  ncol_Phi <- ncol(Phi)
  beta_hat <- solve(vcov_yfz[2:ncol_Phi, 2:ncol_Phi], vcov_yfz[1, 2:ncol_Phi])

  # Group differences for Z
  beta_z <- solve(vcov_yfz[wcol_Phi, wcol_Phi], vcov_yfz[1, wcol_Phi])
  beta_c <- as.numeric(0 - (beta_z %*% t(1 - pr_grp_1)))

  out <- list(vcov_yxw = vcov_yxw, vcov_yxz = vcov_yxz, vcov_yfz = vcov_yfz,
              beta_hat = beta_hat, Z0 = beta_c, Z1 = beta_z)
  return(out)
}
