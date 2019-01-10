# Setup -------------------------------------------------------------------
rm(list = ls())
set.seed(2390487)
n <- 1e4
# cum_prob <- list(1, 1, 1, c(.4, 1), c(.3, .8, 1))
# cov_mx <- matrix(c(1, .1, .1, .2, .2,
#                    .1, 1, .3, .4, .4,
#                    .1, .3, 1, .4, .4,
#                    .2, .4, .4, 1, .5,
#                    .2, .4, .4, .5, 1), 5)
cum_prob <- list(1, 1, 1, c(.4, 1))
cov_mx <- matrix(c(1, .1, .1, .2,
                   .1, 4, .3, .4,
                   .1, .3, 9, .4,
                   .2, .4, .4, 1), 4)
mu_yx <- 0:2
sd_yx <- 1:3
# Generate data -----------------------------------------------------------
df <- questionnaire_gen(n, cum_prob, cov_matrix = cov_mx, theta = TRUE,
                        full_output = TRUE, c_mean = mu_yx, c_sd = sd_yx,
                        family = "gaussian")

# Test data (to be incorporated) ------------------------------------------
mu_yxz  <- list(y = 0, x1 = 1, x2 = 2, z1 = 0, z2 = 0)
var_yxz <- list(y = 1, x1 = 4, x2 = 9, z1 = 1, z2 = 1)
sd_yxz  <- lapply(var_yxz, sqrt)
num_yx <- 3
cols_z <- 4:5

# Discrete variables generated from Z
cum_prob <- list(w1 = c(0, .4, 1), w2 = c(0, .3, .8, 1))  # different format from q_gen
names_w <- names(cum_prob)
mu_w <- lapply(cum_prob, diff)
mu_w_minus_1 <- unlist(sapply(mu_w, function(w) w[-1]))
var_w <- lapply(mu_w, function(p) p * (1 - p))
sd_w <- lapply(var_w, sqrt)

cov_yx <- .1
cov_yz <- .2
cov_xx <- .3
cov_xz <- .4
cov_zz <- .5
vcov_yxz <- matrix(c(1, .1, cov_yx, cov_yz, cov_yz,
                     cov_yx, 4, cov_xx, cov_xz, cov_xz,
                     cov_yx, cov_xx, 9, cov_xz, cov_xz,
                     cov_yz, cov_xz, cov_xz, 1, cov_zz,
                     cov_yz, cov_xz, cov_xz, cov_zz, 1), 5)
dimnames(vcov_yxz) <- list(names(mu_yxz), names(mu_yxz))
yxzw <- df$bg
names(yxzw) <- c("subject", "y", "x1", "x2", "w1")
q_z <- lapply(cum_prob, function(x) qnorm(x, mean = 0, sd = 1))  # Z ~ N(0, 1)
reg_yxw1 <- lm(y ~ x1 + x2 + w1, yxzw)

# Calculating elements of vcov_yxw ----------------------------------------
# Missing pieces: all covariances involving W, i.e., Cov(Y, W), Cov(X, W) and
# Cov(W, W). Calculations below:
exp_X_given_Y <- function(lo_lim, up_lim, mu_X = 0, cov_XY = .5, mu_Y = 0, sd_Y = 1) {
  a <- (lo_lim - mu_Y) / sd_Y
  b <- (up_lim - mu_Y) / sd_Y
  return(mu_X + (cov_XY / sd_Y) * (dnorm(a) - dnorm(b)) / (pnorm(b) - pnorm(a)))
}

# Calculates E(XW) or E(YW) for covariance later on
exp_AB2 <- function(names_b, mu_a, mu_b, cov_ab, q, ...) {
  exp_ab <- list()
  for (b in names_b) {
    exp_ab[[b]] <- vector()
    for (i in seq_along(mu_b[[b]])) {
      exp_ab[[b]][i] <- mu_b[[b]][i] *
        exp_X_given_Y(q[[b]][i], q[[b]][i + 1], mu_a, cov_ab, ...)
    }
  }
  return(exp_ab)
}

exp_yw <- exp_AB2(names_w, mu_yxz$y, mu_w, cov_yz, q_z)
exp_x1w <- exp_AB2(names_w, mu_yxz$x1, mu_w, cov_xz, q_z, sd_Y = sd_yxz$x1)
exp_x2w <- exp_AB2(names_w, mu_yxz$x2, mu_w, cov_xz, q_z, sd_Y = sd_yxz$x2)

cov_AB <- function(names_b, exp_ab, mu_a, mu_b) {
  covar <- sapply(names_b, function(b) (exp_ab[[b]] - mu_a * mu_b[[b]])[-1])
  return(covar)
}
# [-1] below removes first category
# From Cov(X, W) = E(XW) - E(X)E(W)
cov_yw <- cov_AB(names_w, exp_yw, mu_yxz$y, mu_w)
cov_x1w <- cov_AB(names_w, exp_x1w, mu_yxz$x1, mu_w)
cov_x2w <- cov_AB(names_w, exp_x2w, mu_yxz$x2, mu_w)

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
vcov_yxw["x1", c("w1", "w21", "w22")] <- vcov_yxw[c("w1", "w21", "w22"), "x1"] <- unlist(cov_x1w)
vcov_yxw["x2", c("w1", "w21", "w22")] <- vcov_yxw[c("w1", "w21", "w22"), "x2"] <- unlist(cov_x2w)

# Adding Cov(W, W) for the same Z
vcov_yxw[4, 4] <- vcov_w$w1 #TODO: generalize for any number and size of W
vcov_yxw[5:6, 5:6] <- vcov_w$w2 #TODO: generalize for any number and size of W

# Adding Cov(W, W) for different Zs
calc_p_mvn_trunc <- function(lo, up, sig, mu = c(0, 0)) {
  mvtnorm::pmvnorm(lo, up, mu, sig)[1] / (pnorm(up[2]) - pnorm(lo[2]))
}

# TODO: flexibilize according to mu_w
vcov_yxw["w1", c("w21", "w22")] <- vcov_yxw[c("w21", "w22"), "w1"] <-
  c(calc_p_mvn_trunc(c(q_z$w1[2], q_z$w2[2]), c(q_z$w1[3], q_z$w2[3]), sig = matrix(c(1, cov_zz, cov_zz, 1), 2)) * .5 - .6 * .5,
    calc_p_mvn_trunc(c(q_z$w1[2], q_z$w2[3]), c(q_z$w1[3], q_z$w2[4]), sig = matrix(c(1, cov_zz, cov_zz, 1), 2)) * .2 - .6 * .2)

# Creating submatrices
# vcov_yw1 <- vcov_yxw[c("y", "w1"), c("y", "w1")]
vcov_yxw1 <- vcov_yxw[c("y", "x1", "x2", "w1"), c("y", "x1", "x2", "w1")]
# vcov_yw <- vcov_yxw[c("y", "w1", "w21", "w22"), c("y", "w1", "w21", "w22")]
# vcov_yx1w <- vcov_yxw[c("y", "x1", "w1", "w21", "w22"), c("y", "x1", "w1", "w21", "w22")]

# Calculating regression coefficients -------------------------------------
calcRegCoeff <- function(cov, mu_y, mu_x) {
  beta <- solve(cov[-1, -1], cov[1, -1])
  alpha <- mu_y - crossprod(beta, mu_x)
  return(unlist(list(alpha, beta)))
}
# cov_reg_yxw <- calcRegCoeff(vcov_yxw, mu_yxz$y, c(mu_yxz$x1, mu_yxz$x2, mu_w_minus_1))
# cov_reg_yw1 <- calcRegCoeff(vcov_yw1, mu_yxz$y, mu_w$w1[-1])
cov_reg_yxw1 <- calcRegCoeff(vcov_yxw1, mu_yxz$y, c(mu_yxz$x1, mu_yxz$x2, mu_w$w1[-1]))
# cov_reg_yx1w <- calcRegCoeff(vcov_yx1w, mu_yxz$y, c(mu_yxz$x1, mu_w_minus_1))
# cov_reg_yw <- calcRegCoeff(vcov_yw, mu_yxz$y, mu_w_minus_1)


# Benchmarking regression coefficients ------------------------------------
print(rbind(reg = coef(reg_yxw1), cov = cov_reg_yxw1))
# print(rbind(reg = coef(reg_yw), cov = cov_reg_yw))
# print(rbind(reg = coef(reg_yx1w), cov = cov_reg_yx1w))
# print(rbind(reg = coef(reg_yxw), cov = cov_reg_yxw))

diff_yxw1 <- coef(reg_yxw1) - cov_reg_yxw1
# diff_yw   <- coef(reg_yw) - cov_reg_yw
# diff_yx1w <- coef(reg_yx1w) - cov_reg_yx1w
# diff_yxw  <- coef(reg_yxw) - cov_reg_yxw

test_that("Numerical and analytical solutions are close", {
  expect_lte(max(diff_yxw1), 0.1)
  # expect_lte(max(diff_yw), 0.1)
  # expect_lte(max(diff_yx1w), 0.1)
  # expect_lte(max(diff_yxw), 0.1)
})
