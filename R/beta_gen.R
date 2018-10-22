#' Generate analytical parameters
#'
#' @param data output from the \code{questionnaire_gen} function for
#'   \code{full.output = TRUE}
#' @param vcov_yfz covariance matrix between Y, F and Z
#' @param Phi latent regression correlation matrix
#' @param wcol_Phi vector with column numbers of the W variables in Phi
#' @param MC if \code{TRUE}, perform Monte Carlo simulation to estimate
#'   regression coefficients
#' @param replications for \code{MC = TRUE}, this represents the number of
#'   Monte Carlo subsamples calculated.
#' @importFrom stats lm model.matrix quantile
#' @details The covariance matrix provided must have Y in the first row/column.
#' @export
#' @examples
#'
#' # Data containing only continuous variables
#' data1 <- questionnaire_gen(100, family="gaussian", theta = TRUE,
#'                            full_output = TRUE, n_X = 3, n_W = 0)
#' beta_gen(data1, MC = TRUE)
#'
#' # Data containing only dichotomous variables
#' data2 <- questionnaire_gen(100, family="gaussian", theta = TRUE,
#'                            full_output = TRUE, n_X = 0, n_W = list(2))
#' beta_gen(data2, MC = TRUE)
#'
#' # Data containing polychotomous variables
#' data3 <- questionnaire_gen(100, family="gaussian", theta = TRUE,
#'                            full_output = TRUE, n_X = 0, n_W = list(2, 3))
#' #beta_gen(data3, MC = TRUE)
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
    reps <- replications
    boot_obs <- replicate(reps, sample(rownames(data$bg), replace = TRUE))
    boot_data <- list()
    for (r in seq(reps)) boot_data[[r]] <- YXW[boot_obs[, r], ]
    boot_coef <- sapply(boot_data, function(x) lm(theta ~ .- 1, x)$coefficients)
    boot_avg_coef <- apply(boot_coef, 1, mean)
    boot_CI <- apply(boot_coef, 1, function(x) quantile(x, c(.025, .975)))

    # Checking if cov_matrix estimates is contained in MC confidence interval
    cov_in_CI <- output > boot_CI[1, ] & output < boot_CI[2, ]

    output <- rbind(cov_matrix = output, MC = boot_avg_coef, boot_CI,
                    cov_in_CI = as.logical(cov_in_CI))
  }
  return(t(output))
}
