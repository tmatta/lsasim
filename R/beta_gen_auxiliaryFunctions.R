exp_A_given_B <- function(low, upp, muA, muB, sdA, covAB) {
  # Calculates E(AB | low < A < upp)
  std_low <- (low - muA) / sdA
  std_upp <- (upp - muA) / sdA
  ratio <- (dnorm(std_low) - dnorm(std_upp)) / (pnorm(std_upp) - pnorm(std_low))
  return(muB + (covAB / sdA) * ratio)
}

exp_AB <- function(names_b, mu_a, mu_b, cov_ab, q_z, mu_z = 0, sd_z = 1) {
  # Calculates E(AB)
  exp_ab <- list()
  for (b in names_b) {
    exp_ab[[b]] <- vector()
    b_num <- match(b, names_b)
    for (i in seq_along(mu_b[[b]])) {
      exp_ab[[b]][i] <- mu_b[[b]][i] *
        exp_A_given_B(q_z[[b]][i], q_z[[b]][i + 1], mu_a, mu_z, sd_z, cov_ab[b_num])
    }
  }
  return(exp_ab)
}

cov_AB <- function(names_b, exp_ab, mu_a, mu_b) {
  # Calculates Cov(AB) as E(AB) - E(A) * E(B)
  covar <- sapply(names_b, function(b) (exp_ab[[b]] - mu_a * mu_b[[b]])[-1])
  return(covar)
}

calc_p_mvn_trunc <- function(low, upp, sig, mu = c(0, 0)) {
  mvtnorm::pmvnorm(low, upp, mu, sig)[1] / (pnorm(upp[2]) - pnorm(low[2]))
}

create_vcov_w <- function(mu, var, remove_ref_cat = TRUE) {
  # Calculates the covariance matrix of the Ws given a list of means
  mx <- tcrossprod(mu, -mu)  # Covariances = - p_i * p_j
  diag(mx) <- var  # Variances equal p_i * (1 - p_i), not p_i ^ 2
  if (remove_ref_cat) mx <- mx[-1, -1]
  return(mx)
}
