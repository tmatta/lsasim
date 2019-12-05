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

  # General parameters ----------------------------------------------------
  pr_grp_2 <- 1 - pr_grp_1  # proportion of observations in group 2
  var_z <- pr_grp_1 * pr_grp_2  # variance of dichotomous variable z
  sd_z  <- sqrt(var_z)
  n_z   <- length(sd_z)  # number of background variables
  n_ind_rep <- rep(n_ind, n_fac)  # number of indicators for each factor
  f_names <- paste0("f", 1:length(n_ind_rep))
  x_names <- paste0("x", 1:sum(n_ind_rep))
  w_names <- paste0("w", 1:n_z)

  # Generating or formatting factor-loading matrix (Lambda) ---------------
  if (class(Lambda)[1] %in% c("numeric", "integer")) {
    # "Lambda" parameter was provided as limits for random genration
    Lambda <- lambda_gen(n_ind, n_fac, Lambda, x_names, f_names)
  } else {
    # "Lambda" parameter was provided as the actual matrix.
    dimnames(Lambda) <- list(x_names, f_names)
  }

  # Generating covariance matrix between Y, X and Z -----------------------
  # Z ~ N(0, 1) is the latent representation of discrete W
  # converts pt. biserial correlations to biserial correlations
  n_yfw <- 1 + n_fac + n_z  # 1 for y
  Phi <- cor_gen(n_yfw)  # latent regression correlation matrix
  rownames(Phi) <- colnames(Phi) <- c("y", f_names, w_names)

  # Setup full YXW covariance matrix --------------------------------------
  # yxw is the covariance between y, x1, ..., x36; w, W ~ N(0, 1)
  vcov_yxw <- cov_yxw_gen(n_ind, n_z, Phi, n_fac, Lambda)

  # Analytical covariance matrix ------------------------------------------
  # yxz is the covariance between y, x1, ..., x36; z
  # using var(z) = pq and point biserial correlations
  vcov_yxz <- cov_yxz_gen(vcov_yxw, w_names, Phi, pr_grp_1, n_ind, n_fac, Lambda, var_z)

  # Latent regression covariance matrix -----------------------------------
  vcov_yfz <- cov_yfz_gen(n_ind, n_fac, Phi, n_z, sd_z, w_names, pr_grp_1)

  # Analytical parameters -------------------------------------------------
  out <- list(vcov_yxw = vcov_yxw, vcov_yxz = vcov_yxz, vcov_yfz = vcov_yfz)
  return(out)
}
