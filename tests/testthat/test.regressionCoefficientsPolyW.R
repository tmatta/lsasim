#TODO: integrate into questionnaire_gen and beta_gen
context("True regression coefficients are sensible")
library(mvtnorm)

# Setup -------------------------------------------------------------------
rm(list = ls())
n <- 1e4

mu_x <- 0
var_x <- 1
sd_x <- sqrt(var_x)

mu_y <- .2
var_y <- .16
sd_y <- sqrt(var_y)

cum_prob <- c(0, .1, .3, .6, 1)
# cum_prob <- c(0, .3, .7, 1)
# cum_prob <- c(0, .4, 1)
mu_z <- diff(cum_prob)
var_z <- sapply(mu_z, function(z) z * (1 - z))
sd_z <- sqrt(var_z)

cov_xy <- 0.3

# Generating data ---------------------------------------------------------
vcov_xy <- matrix(c(var_x, cov_xy, cov_xy, var_y), 2)
xy <- data.frame(rmvnorm(n, mean = c(mu_x, mu_y), sigma = vcov_xy))
names(xy) <- c("x", "y")
q_y_cum <- qnorm(p = cum_prob, mean = mu_y, sd = sd_y)
xyz <- data.frame(xy, z = cut(xy$y, q_y_cum, labels = 1:length(mu_z)))

reg_xz <- lm(x ~ z, xyz)

# Calculating vcov_xyz -----------------------------------------------------
stnorm_y <- (q_y_cum - mu_y) / sd_y  # not used

exp_x_y_grls <- function(a, b, mu_x, mu_y, sd_y, cov) {
  z_a <- (a - mu_y) / sd_y
  z_b <- (b - mu_y) / sd_y
  return(mu_x + (cov / sd_y) * (dnorm(z_a) - dnorm(z_b)) / (pnorm(z_b) - pnorm(z_a)))
}

exp_xz <- 0
for (i in seq_along(mu_z)) {
  exp_xz[i] <- mu_z[i] * exp_x_y_grls(q_y_cum[i], q_y_cum[i + 1], mu_x, mu_y, sd_y, cov_xy)
}

cov_xz <- exp_xz - mu_x * mu_z

# Covariance matrix of the categories of Z
vcov_z <- tcrossprod(mu_z, -mu_z)
diag(vcov_z) <- var_z

# Final assembly of true covariance matrix
vcov_xz <- matrix(nrow = length(mu_z) + 1, ncol = length(mu_z) + 1)
vcov_xz[1, ] <- vcov_xz[, 1] <- c(var_x, cov_xz)
vcov_xz[-1, -1] <- vcov_z
vcov_xz <- vcov_xz[-2, -2]  # remove category one to avoid collinearity

# Checking regression coefficients ----------------------------------------
beta_xz <- solve(vcov_xz[-1, -1], vcov_xz[1, -1])
alpha_xz <- mu_x - crossprod(beta_xz, mu_z[-1])

diff <- coef(reg_xz) - c(alpha_xz, beta_xz)

test_that("Numerical and analytical solutions are close: polynomial W", {
  expect_lte(max(diff), 0.02)
})
