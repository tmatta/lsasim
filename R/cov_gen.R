#' Generation of covariance matrices
#'
#' Second version of the questionnaire_gen function, developed independently to
#' maintain functionality of the original function. This function could
#' eventually be integrated into questionnaire_gen.
#'
#' @param pr_grp_1 proportion of observations in group 1
#' @param n_fac number of factors
#' @param n_ind number of indicators per factor
#' @param Lambda_lims vector of lower and upper limits for the factor-loading
#'   matrix
#' @param seed sample seed number for the Random Number Generator
#' @return A list containing three covariance matrices: vcov_yxw, vcov_yxz and
#'   vcov_yfz
#' @export
#' @examples
#'  vcov <- questionnaire_gen_2()
#'  str(vcov)
cov_gen <- function(pr_grp_1 = .66, n_fac = 9, n_ind = 4,
                    Lambda_lims = c(.6, .9), seed = runif(n = 1, max = 1e8)) {
  # Construct covariance matrices for simulation --------------------------
  set.seed(seed)

  # Population parameters
  pr_grp_2 <- 1 - pr_grp_1  # proportion of observations in group 2
  var_z <- pr_grp_1 * pr_grp_2  # variance of dichotomous variable z
  sd_z  <- sqrt(var_z)

  # Background parameters
  n_ind <- rep(n_ind, n_fac)  # number of indicators for each factor

  # Location markers for the lambda matrix
  n_ind_minus1 <- n_ind - 1
  l_start <- cumsum(n_ind) - n_ind_mifznus1
  l_end <- l_start + n_ind - 1

  # Factor loading matrix (loadings generated randomly) -------------------
  Lambda <- matrix(0, ncol = length(n_ind), nrow = sum(n_ind))

  for (i in seq(n_ind)) {
    Lambda[l_start[i]:l_end[i], i] <- runif(n_ind[i],
                                            Lambda_lims[1], Lambda_lims[2])
  }

  colnames(Lambda) <- paste0("f", 1:length(n_ind))
  rownames(Lambda) <- paste0("x", 1:sum(n_ind))

  # Generation covariance matrix between y, x1, ..., x36, w ---------------
  # w ~ N(0, 1) is the latent representation of dicotomous z
  # converts pt. biserial correlations to biserial correlations

  # Latent regression correlation matrix
  Phi <- cor_gen(n_fac + 2)
  #TODO: check if names below are correct (probably not, given var_names)
  rownames(Phi) <- c("y", paste0("x", 1:n_fac), "w")
  colnames(Phi) <- rownames(Phi)

  # Setup full covariance matrix
  # yxw is the covariance between y, x1, ..., x36; w, w~ N(0, 1)
  vcov_yxw <- matrix(NA, nrow = sum(n_ind) + 2, ncol = sum(n_ind) + 2)

  # Compute covariance matrix for factor indicators
  Phi_x <- Phi[2:(n_fac + 1), 2:(n_fac + 1)]
  cov_x <- Lambda %*% Phi_x %*% t(Lambda)
  var_x <- Lambda ^ 2 %*% Phi_x + (1 - Lambda ^ 2)

  # Compute variances for factor indicators
  indicator_vars <- list()
  for (i in seq(n_ind)) {
    indicator_vars[[i]] <- var_x[l_start[i]:l_end[i], i]
  }
  diag(cov_x) <- unlist(indicator_vars)

  vcov_yxw[2:(length(diag(cov_x)) + 1), 2:(length(diag(cov_x)) + 1)] <- cov_x

  # Compute covariance between y and factor indicators
  vcov_yxw[1, 1] <- 1

  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[1, l_seq + 1] <- Lambda[l_seq, i] * Phi[1, (i + 1)]
    vcov_yxw[l_seq + 1, 1] <- Lambda[l_seq, i] * Phi[1, (i + 1)]
  }

  # Compute covariance between w and factor indicators
  ncol_yxw <- ncol(vcov_yxw)
  ncol_Phi <- ncol(Phi)
  vcov_yxw[ncol_yxw, ncol_yxw] <- 1
  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxw[ncol_yxw, l_seq + 1] <- Lambda[l_seq, i] * Phi[ncol_Phi, (i + 1)]
    vcov_yxw[l_seq + 1, ncol_yxw] <- Lambda[l_seq, i] * Phi[ncol_Phi, (i + 1)]
  }

  # Compute covariance between y and w
  vcov_yxw[1, ncol_yxw] <- vcov_yxw[ncol_yxw, 1] <- Phi[1, ncol_Phi]

  var_names <- c("y", paste0("x", 1:sum(n_ind)), "w")
  rownames(vcov_yxw) <- colnames(vcov_yxw) <- var_names

  # Analytical covariance matrix ------------------------------------------
  # yxz is the covariance between y, x1, ..., x36; z
  # using var(z) = pq and point biserial correlations
  vcov_yxz <- matrix(NA, nrow = sum(n_ind) + 2, ncol = sum(n_ind) + 2)
  ncol_yxz <- ncol(vcov_yxz)
  vcov_yxz[1:(ncol_yxz - 1), 1:(ncol_yxz - 1)] <-
    vcov_yxw[1:(ncol_yxz - 1), 1:(ncol_yxz - 1)]

  # Add z to the covariance matrix
  vcov_yxz[ncol_yxz, ncol_yxz] <- var_z

  cor_ptbis <- list()
  for (i in seq(length(n_ind) + 1)) {
    cor_ptbis[[i]] <- pt_bis_conversion(Phi[ncol_Phi, i], pr_grp_1)
  }

  cor_ptbis <- unlist(cor_ptbis)

  vcov_yxz[1, ncol_yxz] <- vcov_yxz[ncol_yxz, 1] <- cor_ptbis[1] * sd_z

  for (i in seq(n_ind)) {
    l_seq <- l_start[i]:l_end[i]
    vcov_yxz[ncol_yxz, l_seq + 1] <- Lambda[l_seq, i] * cor_ptbis[(i + 1)] * sd_z
    vcov_yxz[l_seq + 1, ncol_yxz] <- Lambda[l_seq, i] * cor_ptbis[(i + 1)] * sd_z
  }

  # Latent regression covariance matrix -----------------------------------
  # yfz is the covariance between y, f1, ..., f9, z; where f is a factor
  sd_vec <- c(rep(1, ncol_Phi - 1), sd_z)
  Phi_pb <- Phi
  Phi_pb[ncol_Phi, seq(ncol_Phi - 1)] <- cor_ptbis
  Phi_pb[seq(ncol_Phi - 1), ncol_Phi] <- cor_ptbis

  vcov_yfz <- diag(sd_vec) %*% Phi_pb %*% diag(sd_vec)  #TODO: should be yfz!

  # Analytical parameters -------------------------------------------------

  # Latent regression parameters
  beta_hat <- solve(vcov_yfz[2:ncol_Phi, 2:ncol_Phi], vcov_yfz[1, 2:ncol_Phi])

  # Group differences for Z
  beta_z <- solve(vcov_yfz[ncol_Phi, ncol_Phi], vcov_yfz[1, ncol_Phi])
  beta_c <- as.numeric(0 - (beta_z %*% 1 - pr_grp_1))

  out <- list(vcov_yxw = vcov_yxw, vcov_yxz = vcov_yxz, vcov_yfz = vcov_yfz,
              beta_hat = beta_hat, Z0 = beta_c, Z1 = beta_z)
  return(out) #TODO: what is the desired output?
}
