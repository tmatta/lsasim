#' Generate analytical parameters
#'
#' @param vcov_yfz covariance matrix between Y, F and Z
#' @param Phi latent regression correlation matrix
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param pr_grp_1 scalar or list of proportions of the first group
#' @export
beta_gen <- function(vcov_yfz, Phi, wcol_Phi, pr_grp_1) {
  # TODO: change inputs so that background data can be provided
  ncol_Phi <- ncol(Phi)

  # Latent regression parameters
  beta_hat <- solve(vcov_yfz[2:ncol_Phi, 2:ncol_Phi], vcov_yfz[1, 2:ncol_Phi])

  # Group differences for Z
  beta_z <- solve(vcov_yfz[wcol_Phi, wcol_Phi], vcov_yfz[1, wcol_Phi])
  beta_c <- as.numeric(0 - (beta_z %*% t(1 - pr_grp_1)))
  return(list(beta_hat, Z0 = beta_c, Z1 = beta_z))
}
