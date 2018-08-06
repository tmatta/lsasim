#' Generate latent regression covariance matrix
#'
#' Generates covariance matrix between Y, F and Z
#'
#' @param Phi latent regression correlation matrix
#' @param n_z number of background variables
#' @param sd_z standard deviation of background variables
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param w_names names of W variables
#' @param cor_ptbis vector of point-bisserial correlations between Y and F or W
#'
cov_yfz_gen <- function(Phi, n_z, sd_z, wcol_Phi, w_names, cor_ptbis) {
  sd_vec <- c(rep(1, max(wcol_Phi) - n_z), sd_z)
  yfcol_yfz <- colnames(Phi)[-match(w_names, colnames(Phi))]
  Phi_pb <- Phi
  Phi_pb[wcol_Phi, yfcol_yfz] <- cor_ptbis  # Y and Fs vs. Ws
  Phi_pb[yfcol_yfz, wcol_Phi] <- t(Phi_pb[wcol_Phi, yfcol_yfz])

  vcov_yfz <- diag(sd_vec) %*% Phi_pb %*% diag(sd_vec)
  return(vcov_yfz)
}
