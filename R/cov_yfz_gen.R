#' Generate latent regression covariance matrix
#'
#' Generates covariance matrix between Y, F and Z
#'
#' @param n_ind number of indicator variables
#' @param n_fac number of factors
#' @param Phi latent regression correlation matrix
#' @param n_z number of background variables
#' @param sd_z standard deviation of background variables
#' @param w_names names of W variables
#' @param pr_grp_1 scalar or list of proportions of the first group
#'
cov_yfz_gen <- function(n_ind, n_fac, Phi, n_z, sd_z, w_names, pr_grp_1) {
  n_ind_rep <- rep(n_ind, n_fac)  # number of indicators for each factor
  wcol_Phi <- match(w_names, colnames(Phi))
  sd_vec <- c(rep(1, max(wcol_Phi) - n_z), sd_z)
  yfcol_yfz <- colnames(Phi)[-match(w_names, colnames(Phi))]
  Phi_pb <- Phi

  ## Between Zs and Y
  cor_ptbis <- list()
  for (i in seq(length(n_ind_rep) + 1)) {
    cor_ptbis[[i]] <- pt_bis_conversion(Phi[wcol_Phi, i], pr_grp_1)
  }
  cor_ptbis <- unlist(cor_ptbis)

  Phi_pb[wcol_Phi, yfcol_yfz] <- cor_ptbis  # Y and Fs vs. Ws
  Phi_pb[yfcol_yfz, wcol_Phi] <- t(Phi_pb[wcol_Phi, yfcol_yfz])

  vcov_yfz <- diag(sd_vec) %*% Phi_pb %*% diag(sd_vec)
  return(vcov_yfz)
}
