#' Generate latent regression covariance matrix
#'
#' Generates covariance matrix between Y, F and Z
#'
#' @param n_ind number of indicator variables
#' @param Phi latent regression correlation matrix
#' @param n_z number of background variables
#' @param sd_z standard deviation of background variables
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param w_names names of W variables
#' @param pr_grp_1 scalar or list of proportions of the first group
#'
cov_yfz_gen <- function(n_ind, Phi, n_z, sd_z, wcol_Phi, w_names, pr_grp_1) {
  sd_vec <- c(rep(1, max(wcol_Phi) - n_z), sd_z)
  yfcol_yfz <- colnames(Phi)[-match(w_names, colnames(Phi))]
  Phi_pb <- Phi

  ## Between Zs and Y
  cor_ptbis <- list()
  for (i in seq(length(n_ind) + 1)) {
    cor_ptbis[[i]] <- pt_bis_conversion(Phi[wcol_Phi, i], pr_grp_1)
    # TODO: figure out how this works with polytomous W
  }
  cor_ptbis <- unlist(cor_ptbis)

  Phi_pb[wcol_Phi, yfcol_yfz] <- cor_ptbis  # Y and Fs vs. Ws
  Phi_pb[yfcol_yfz, wcol_Phi] <- t(Phi_pb[wcol_Phi, yfcol_yfz])

  vcov_yfz <- diag(sd_vec) %*% Phi_pb %*% diag(sd_vec)
  return(vcov_yfz)
}
