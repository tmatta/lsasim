#TODO: integrate into questionnaire_gen and beta_gen
context("True regression coefficients are sensible")
library(mvtnorm)

# Setup -------------------------------------------------------------------
rm(list = ls())
n <- 1e5

mu_yxz  <- list(y = 0, x1 = 1, x2 = 1, z1 = 0, z2 = 0)
var_yxz <- list(y = 1, x1 = 1, x2 = 1, z1 = 1, z2 = 1)
sd_yxz  <- lapply(var_yxz, sqrt)
num_yx <- 3
cols_z <- 4:5

# Discrete variables generated from Z
cum_prob <- list(w1 = c(0, .1, 1), w2 = c(0, .3, .8, 1))  # different from q_gen
names_w <- names(cum_prob)
mu_w <- lapply(cum_prob, diff)
mu_w_minus_1 <- unlist(sapply(mu_w, function(w) w[-1]))
var_w <- lapply(mu_w, function(p) p * (1 - p))
sd_w <- lapply(var_w, sqrt)

cov_yz <- cov_xz <- cov_yx <- cov_xx <- .5
cov_zz <- 0.5
vcov_yxz <- matrix(c(1, cov_yx, cov_yx, cov_yz, cov_yz,
                     cov_yx, 1, cov_xx, cov_xz, cov_xz,
                     cov_yx, cov_xx, 1, cov_xz, cov_xz,
                     cov_yz, cov_xz, cov_xz, 1, cov_zz,
                     cov_yz, cov_xz, cov_xz, cov_zz, 1), 5)
dimnames(vcov_yxz) <- list(names(mu_yxz), names(mu_yxz))
# Generating data ---------------------------------------------------------
yxz <- data.frame(rmvnorm(n, mean = unlist(mu_yxz), sigma = vcov_yxz))
q_z <- lapply(cum_prob, function(x) qnorm(x, mean = 0, sd = 1))  # Z ~ N(0, 1)
yxzw <- data.frame(yxz,
                   w1 = cut(yxz$z1, q_z$w1, labels = 0:(length(mu_w$w1) - 1)),
                   w2 = cut(yxz$z2, q_z$w2, labels = 1:length(mu_w$w2)))

reg_yxw  <- lm(y ~ x1 + x2 + w1 + w2, yxzw)
reg_yw1  <- lm(y ~ w1, yxzw)
reg_yxw1 <- lm(y ~ x1 + x2 + w1, yxzw)
reg_yx1w  <- lm(y ~ x1 + w1 + w2, yxzw)
reg_yw  <- lm(y ~ w1 + w2, yxzw)

# Calculating elements of vcov_yxw ----------------------------------------
# Missing pieces: all covariances involving W, i.e., Cov(Y, W), Cov(X, W) and
# Cov(W, W). Calculations below:
exp_X_given_Y <- function(lo_lim, up_lim, mu_X = 0, cov_XY = .5, mu_Y = 0, sd_Y = 1) {
  a <- (lo_lim - mu_Y) / sd_Y
  b <- (up_lim - mu_Y) / sd_Y
  return(mu_X + (cov_XY / sd_Y) * (dnorm(a) - dnorm(b)) / (pnorm(b) - pnorm(a)))
}

# Calculates E(XW) or E(YW) for covariance later on
exp_yw <- list()
for (w in names_w) {
  exp_yw[[w]] <- vector()
  for (i in seq_along(mu_w[[w]])) {
    exp_yw[[w]][i] <- mu_w[[w]][i] *
      exp_X_given_Y(q_z[[w]][i], q_z[[w]][i + 1], mu_yxz$y, cov_yz)
  }
}

exp_xw <- list()
for (w in names_w) {
  exp_xw[[w]] <- vector()
  for (i in seq_along(mu_w[[w]])) {
    exp_xw[[w]][i] <- mu_w[[w]][i] *
      exp_X_given_Y(q_z[[w]][i], q_z[[w]][i + 1], mu_yxz$x1, cov_xz)
  }
}

# [-1] below removes first category
# From Cov(X, W) = E(XW) - E(X)E(W)
cov_yw <- sapply(names_w, function(w) (exp_yw[[w]] - mu_yxz$y * mu_w[[w]])[-1])
cov_xw <- sapply(names_w, function(w) (exp_xw[[w]] - mu_yxz$x1 * mu_w[[w]])[-1])

# Covariance matrix of the categories of W
create_vcov_w <- function(mu, var, remove_ref_cat = TRUE) {
  mx <- tcrossprod(mu, -mu)  # Covariances = - p_i * p_j
  diag(mx) <- var  # Variances equal p_i * (1 - p_i), not p_i ^ 2
  if (remove_ref_cat) mx <- mx[-1, -1]
  return(mx)
}
vcov_w <- sapply(names_w, function(w) create_vcov_w(mu_w[[w]], var_w[[w]]))

# Final assembly of true covariance matrix --------------------------------
vcov_order <- num_yx + sum(sapply(mu_w, function(w) length(w) - 1))
vcov_yxw <- matrix(nrow = vcov_order, ncol = vcov_order)
col_names <- c(names(mu_yxz)[-c(4, 5)], names(mu_w_minus_1))
dimnames(vcov_yxw) <- list(col_names, col_names)

# Adding Cov(Y, W)
vcov_yxw[1, ] <- vcov_yxw[, 1] <- c(var_yxz$y, cov_yx, cov_yx, unlist(cov_yw))

# Adding Cov(Y, X)
vcov_yxw[1:num_yx, 1:num_yx] <- vcov_yxz[-cols_z, -cols_z]

# Adding Cov(X, W)
vcov_yxw["x1", c("w1", "w21", "w22")] <- vcov_yxw[c("w1", "w21", "w22"), "x1"] <- unlist(cov_xw)
vcov_yxw["x2", c("w1", "w21", "w22")] <- vcov_yxw[c("w1", "w21", "w22"), "x2"] <- unlist(cov_xw)

# Adding Cov(W, W) for the same Z
vcov_yxw[4, 4] <- vcov_w$w1 #TODO: generalize for any number and size of W
vcov_yxw[5:6, 5:6] <- vcov_w$w2 #TODO: generalize for any number and size of W

# Adding Cov(W, W) for different Zs
calc_p_mvn_trunc <- function(lo, up, sig, mu = c(0, 0)) {
  pmvnorm(lo, up, mu, sig)[1] / (pnorm(up[2]) - pnorm(lo[2]))
}
vcov_yxw["w1", c("w21", "w22")] <- vcov_yxw[c("w21", "w22"), "w1"] <-
  c(calc_p_mvn_trunc(c(q_z$w1[2], q_z$w2[2]), c(q_z$w1[3], q_z$w2[3]), sig = matrix(c(1, cov_zz, cov_zz, 1), 2)) * .5 - .9 * .5,
    calc_p_mvn_trunc(c(q_z$w1[2], q_z$w2[3]), c(q_z$w1[3], q_z$w2[4]), sig = matrix(c(1, cov_zz, cov_zz, 1), 2)) * .2 - .9 * .2)

# Creating submatrices
vcov_yw1 <- vcov_yxw[c("y", "w1"), c("y", "w1")]
vcov_yxw1 <- vcov_yxw[c("y", "x1", "x2", "w1"), c("y", "x1", "x2", "w1")]
vcov_yw <- vcov_yxw[c("y", "w1", "w21", "w22"), c("y", "w1", "w21", "w22")]
vcov_yx1w <- vcov_yxw[c("y", "x1", "w1", "w21", "w22"), c("y", "x1", "w1", "w21", "w22")]

# Calculating regression coefficients -------------------------------------
calcRegCoeff <- function(cov, mu_y, mu_x) {
  beta <- solve(cov[-1, -1], cov[1, -1])
  alpha <- mu_y - crossprod(beta, mu_x)
  return(unlist(list(alpha, beta)))
}
cov_reg_yxw <- calcRegCoeff(vcov_yxw, mu_yxz$y, c(mu_yxz$x1, mu_yxz$x2, mu_w_minus_1))
cov_reg_yw1 <- calcRegCoeff(vcov_yw1, mu_yxz$y, mu_w$w1[-1])
cov_reg_yxw1 <- calcRegCoeff(vcov_yxw1, mu_yxz$y, c(mu_yxz$x1, mu_yxz$x2, mu_w$w1[-1]))
cov_reg_yx1w <- calcRegCoeff(vcov_yx1w, mu_yxz$y, c(mu_yxz$x1, mu_w_minus_1))
cov_reg_yw <- calcRegCoeff(vcov_yw, mu_yxz$y, mu_w_minus_1)


# Benchmarking regression coefficients ------------------------------------
# print(rbind(reg = coef(reg_yxw1), cov = cov_reg_yxw1))
# print(rbind(reg = coef(reg_yw), cov = cov_reg_yw))
# print(rbind(reg = coef(reg_yx1w), cov = cov_reg_yx1w))
# print(rbind(reg = coef(reg_yxw), cov = cov_reg_yxw))

diff_yxw1 <- coef(reg_yxw1) - cov_reg_yxw1
diff_yw   <- coef(reg_yw) - cov_reg_yw
diff_yx1w <- coef(reg_yx1w) - cov_reg_yx1w
diff_yxw  <- coef(reg_yxw) - cov_reg_yxw

test_that("Numerical and analytical solutions are close", {
  expect_lte(max(diff_yxw1), 0.1)
  expect_lte(max(diff_yw), 0.1)
  expect_lte(max(diff_yx1w), 0.1)
  expect_lte(max(diff_yxw), 0.1)
})
