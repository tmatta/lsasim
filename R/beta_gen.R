#' Generate analytical parameters
#'
#' @param data output from the \code{questionnaire_gen} function for
#'   \code{full.output = TRUE}
#' @param vcov_yfz covariance matrix between Y, F and Z
#' @param Phi latent regression correlation matrix
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param pr_grp_1 scalar or list of proportions of the first group
#' @param MC if \code{TRUE}, perform Monte Carlo simulation to estimate
#'   regression coefficients
#' @export
#' @examples
#' bg <- questionnaire_gen(1000, n_X = 2, n_W = list(2, 2), theta = TRUE,
#'                         full_output = TRUE)
#' fit.obs <- lm(theta ~ q1 + q2 + q3 + q4, data = bg$bg)
#' fit.exp <- beta_gen(bg)
#' summary(fit.obs)
#' cbind(obs = fit.obs$coefficients[2:5], beta_hat = fit.exp$beta_hat,
#'       beta_c = fit.exp$Z0)
beta_gen <- function(data, vcov_yfz, Phi, wcol_Phi, MC = FALSE) {
  if (missing(vcov_yfz)) vcov_yfz <- data$cov_matrix
  if (missing(Phi)) Phi <- data$cor_matrix
  wcol_data <- which(sapply(data$bg, class) == "factor")
  if (missing(wcol_Phi)) wcol_Phi <- wcol_data - data$n_X - data$theta
  ncol_Phi <- ncol(Phi)
  pr_grp_1 <- apply(data$bg[wcol_data], 2, function(x) prop.table(table(x)))[1, ]


  # Latent regression parameters
  B <- vcov_yfz[2:ncol_Phi, 2:ncol_Phi]
  Y <- vcov_yfz[1, 2:ncol_Phi]
  beta_hat <- solve(B, Y)

  # Group differences for Z
  beta_z <- solve(vcov_yfz[wcol_Phi, wcol_Phi], vcov_yfz[1, wcol_Phi])
  beta_c <- as.numeric(0 - (beta_z %*% t(1 - pr_grp_1)))
  output <- list(beta_hat = beta_hat, Z0 = beta_c, Z1 = beta_z)

  if (MC) {
    message("Generating Monte Carlo coefficient estimates. Please wait...")
    reps <- 1e3
    subsample_obs <- replicate(reps, sample(rownames(data$bg), replace = TRUE))
    subsample_data <- list()
    for (r in seq(reps)) {
      subsample_data[[r]] <- data$bg[subsample_obs[, r], ]
    }
    subsample_coef <- sapply(subsample_data,
                             function(x) lm(data = x[-1], formula = theta ~ .)$coefficients)
    subsample_avg_coef <- apply(subsample_coef, 1, mean)
    output <- c(output, MC_coef = list(subsample_avg_coef))
  }
  return(output)
}
