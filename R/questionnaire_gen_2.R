#' Generation of ordinal and continuous variables (temp)
#'
#' Second version of the questionnaire_gen function, developed independently to
#' maintain functionality of the original function. This function could
#' eventually be integrated into questionnaire_gen
#'
#' @param seed sample seed number for the Random Number Generator
#' @export
questionnaire_gen_2 <- function(seed = 674634) {
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  # Construct covariance matricies for simulation
  # yxz is the covariance between y, x1, ..., x36; z
  # yxw is the covariance between y, x1, ..., x36; w, w~ N(0, 1)
  # yfz is the covariance between y, f1, ..., f9, z; where f is a factor
  #~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~#
  set.seed(seed)

  #------------------------------------------------------------------------------#
  # Population parameters
  #------------------------------------------------------------------------------#

  #--- proportion of observations in group 1 and group 2
  pr_grp_1 <- .66
  pr_grp_2 <- 1 - pr_grp_1

  #--- variance and standard deviation for dichotomous variable z
  var_z <- pr_grp_1 * pr_grp_2
  sd_z  <- sqrt(var_z)


  #------------------------------------------------------------------------------#
  # Background parameters
  #------------------------------------------------------------------------------#

  #--- number of factors
  n_fac <- 9

  #--- number of indicators for each of the 9 factors
  n_ind <- rep(4, n_fac)

  #--- loction markers for lambda matrix
  n_ind_minus1 <- n_ind - 1
  l_start <- cumsum(n_ind) - n_ind_minus1
  l_end <- l_start + n_ind - 1


  #------------------------------------------------------------------------------#
  # Factor loading matrix (loadings generated randomly)
  #------------------------------------------------------------------------------#
  Lambda <- matrix(0, ncol = length(n_ind), nrow = sum(n_ind))

  for(i in 1:length(n_ind))  Lambda[l_start[i]:l_end[i], i] <- runif(n_ind[i], .6, .9)

  colnames(Lambda) <- paste0("f", 1:length(n_ind))
  rownames(Lambda) <- paste0("x", 1:sum(n_ind))
  round(Lambda, 3)

  #------------------------------------------------------------------------------#
  # Generation covariance matrix between y, x1, ..., x36, w
  # w ~ N(0, 1) is the latent representation of dicotomous z
  # converts pt. biserial correlations to biserial correlations
  #------------------------------------------------------------------------------#

  #--- Latent regression correlation matrix
  Phi <- lsasim::cor_gen(n_fac + 2)
  det(Phi) > 0
  round(Phi, 3)

  #--- setup full covariance matrix
  vcov_yxw <- matrix(NA, nrow = sum(n_ind) + 2, ncol = sum(n_ind) + 2)

  #--- compute covariance matrix for factor indicators
  cov_x <- Lambda %*% Phi[2:10, 2:10] %*% t(Lambda)
  var_x <- Lambda^2 %*% Phi[2:10, 2:10] + (1 - Lambda^2)

  #--- compute variances for factor indicators
  indicator_vars <- list()
  for(i in 1:length(n_ind)) indicator_vars[[i]] <- var_x[l_start[i]:l_end[i], i]
  diag(cov_x) <- unlist(indicator_vars)

  vcov_yxw[2:37, 2:37] <- cov_x

  #--- compute covariance between y and factor indicators
  vcov_yxw[1, 1] <- 1

  for(i in 1:length(n_ind))
    vcov_yxw[1, (l_start[i] + 1):(l_end[i] + 1)] <-
    vcov_yxw[(l_start[i] + 1):(l_end[i] + 1), 1] <-
    Lambda[l_start[i]:l_end[i], i] * Phi[1, (i + 1)]

  # round(vcov_yxw, 2)

  #--- compute covariance between w and factor indicators
  vcov_yxw[38, 38] <- 1
  for(i in 1:length(n_ind))
    vcov_yxw[38, (l_start[i] + 1):(l_end[i] + 1)] <-
    vcov_yxw[(l_start[i] + 1):(l_end[i] + 1), 38] <-
    Lambda[l_start[i]:l_end[i], i] * Phi[11, (i + 1)]

  # round(vcov_yxw, 2)

  #--- compute covariance between y and w
  vcov_yxw[1, 38] <- vcov_yxw[38, 1] <- Phi[1, 11]

  var_names <- c("y", paste0("x", 1:sum(n_ind)), "w")
  rownames(vcov_yxw) <- colnames(vcov_yxw) <- var_names

  det(vcov_yxw) > 0

  saveRDS(vcov_yxw, file = "vcov_yxw.rds")

  #------------------------------------------------------------------------------#
  # Analytical covariance matrix
  # using var(z) = pq and point biserial correlations
  #------------------------------------------------------------------------------#
  vcov_yxz <- matrix(NA, nrow = sum(n_ind) + 2, ncol = sum(n_ind) + 2)

  vcov_yxz[1:37, 1:37] <- vcov_yxw[1:37, 1:37]

  #--- add z to the covariance matrix
  vcov_yxz[38, 38] <- var_z

  cor_ptbis <- list()

  for(i in 1:(length(n_ind) + 1))
    cor_ptbis[[i]]  <- pt_bis_conversion(bis_cor = Phi[11, i], pr_group1 = pr_grp_1)

  cor_ptbis <- unlist(cor_ptbis)

  vcov_yxz[1, 38] <- vcov_yxz[38, 1] <- cor_ptbis[1] * sd_z

  for(i in 1:length(n_ind))
    vcov_yxz[38, (l_start[i] + 1):(l_end[i] + 1)] <-
    vcov_yxz[(l_start[i] + 1):(l_end[i] + 1), 38] <-
    Lambda[l_start[i]:l_end[i], i] * cor_ptbis[(i + 1)] * sd_z

  det(vcov_yxz) > 0

  saveRDS(vcov_yxz,  file = "vcov_yxz.rds")


  #------------------------------------------------------------------------------#
  # latent regression covariance matrix
  #------------------------------------------------------------------------------#
  sd_vec <- c(rep(1, 10), sd_z)
  Phi_pb <- Phi
  Phi_pb[11, 1:10] <- Phi_pb[1:10, 11] <- cor_ptbis

  vcov_xfz <- diag(sd_vec) %*% Phi_pb %*% diag(sd_vec)

  saveRDS(vcov_xfz, file = "vcov_xfz.rds")

  #------------------------------------------------------------------------------#
  # Analytical parameters
  #------------------------------------------------------------------------------#

  #--- latent regression parameters
  beta_hat <- solve(vcov_xfz[2:11, 2:11], vcov_xfz[1, 2:11])
  round(beta_hat, 3)
  # [1]  0.307 -0.091 -0.326  0.481  0.508 -0.053  0.790  0.106 -0.215 -0.622


  #--- group differences for Z
  beta_z <- solve(vcov_xfz[11, 11], vcov_xfz[1, 11])
  beta_c <- as.numeric(0 - (beta_z %*% 1 - pr_grp_1))
  round(cbind(Z0 = beta_c, Z1 = beta_z), 3)
  #        Z0   Z1
  # [1,] 0.14 0.52

}
