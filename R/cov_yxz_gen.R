#' Generate analytical covariance matrix
#'
#  Generate covariance matrix between Y, X and Z
#'
#' @param vcov_yxw covariance matrix between Y, X and W
#' @param w_names name of the W variables
#' @param Phi latent regression correlation matrix
#' @param pr_grp_1 scalar or list of proportions of the first group
#' @param n_ind number of indicator variables
#' @param n_fac number of factors
#' @param Lambda matrix containing the factor loadings
#' @param var_z vector of variances of the background variables
#'
cov_yxz_gen <- function(vcov_yxw, w_names, Phi,
                        pr_grp_1, n_ind, n_fac, Lambda, var_z) {
  n_ind_rep <- rep(n_ind, n_fac)  # number of indicators for each factor
  l_start <- cumsum(n_ind_rep) - (n_ind_rep - 1)
  l_end <- l_start + n_ind_rep - 1
  wcol_Phi <- match(w_names, colnames(Phi))
  n_z <- length(var_z)
  n_yxw <- 1 + sum(n_ind_rep) + n_z

  sd_z <- sqrt(var_z)
  vcov_yxz <- matrix(NA, nrow = n_yxw, ncol = n_yxw)
  yfcol_yxz <- colnames(vcov_yxw)[-match(w_names, colnames(vcov_yxw))]

  # Filling vcov_yxz with values from vcov_yxw for Y and F
  dimnames(vcov_yxz) <- dimnames(vcov_yxw)
  wcol_yxz <- match(w_names, colnames(vcov_yxz))
  vcov_yxz[yfcol_yxz, yfcol_yxz] <- vcov_yxw[yfcol_yxz, yfcol_yxz]

  # Adding Z to the covariance matrix
  ## Between Zs
  vcov_yxz[wcol_yxz, wcol_yxz] <- var_z
  ## Between Zs and Y
  cor_ptbis <- list()
  for (i in seq(length(n_ind_rep) + 1)) {
    cor_ptbis[[i]] <- pt_bis_conversion(Phi[wcol_Phi, i], pr_grp_1)
  }
  cor_ptbis <- unlist(cor_ptbis)
  vcov_yxz[1, wcol_yxz] <- vcov_yxz[wcol_yxz, 1] <- cor_ptbis[1] * sd_z
  ## Between Zs and Xs
  for (i in seq(n_ind_rep)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxz[wcol_yxz, l_seq + 1] <- Lambda[l_seq, i] * cor_ptbis[(i + 1)] * sd_z
    vcov_yxz[l_seq + 1, wcol_yxz] <- Lambda[l_seq, i] * cor_ptbis[(i + 1)] * sd_z
  }

  return(vcov_yxz)
}
