#' Generate analytical parameters
#'
#' @param vcov_yfz covariance matrix between Y, F and Z
#' @param Phi latent regression correlation matrix
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param pr_grp_1 scalar or list of proportions of the first group
#' @export
#' @examples
#' bg <- questionnaire_gen(100, n_X = 2, n_W = c(2, 2), theta = TRUE,
#'                         full_output = TRUE)
#' fit.obs <- lm(theta ~ q1 + q2 + q3 + q4, data = test$bg)
#' prop.gr.1 <- apply(test$bg[c("q3", "q4")], 2, function(x) prop.table(table(x)))[1, ]
#' fit.exp <- beta_gen(vcov_yfz = test$cov_matrix, Phi = test$cor_matrix,
#'                     wcol_Phi = 4:5, pr_grp_1 = prop.gr.1)
#'
#' cbind(obs = fit.obs$coefficients[2:5], exp = fit.exp$beta_hat)
beta_gen <- function(vcov_yfz, Phi, wcol_Phi, pr_grp_1) {
  # TODO: change inputs so that background data can be provided
  ncol_Phi <- ncol(Phi)

  # Latent regression parameters
  beta_hat <- solve(vcov_yfz[2:ncol_Phi, 2:ncol_Phi], vcov_yfz[1, 2:ncol_Phi])

  # Group differences for Z
  beta_z <- solve(vcov_yfz[wcol_Phi, wcol_Phi], vcov_yfz[1, wcol_Phi])
  beta_c <- as.numeric(0 - (beta_z %*% t(1 - pr_grp_1)))
  return(list(beta_hat = beta_hat, Z0 = beta_c, Z1 = beta_z))
}
