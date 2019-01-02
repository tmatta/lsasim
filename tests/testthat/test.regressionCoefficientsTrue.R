#TODO: integrate into questionnaire_gen and beta_gen
context("True regression coefficients are sensible")

# Setup -------------------------------------------------------------------
rm(list = ls())
set.seed(2)
n <- 1e5

mu_x <- 0
var_x <- 1
sd_x <- sqrt(var_x)

mu_y <- .2
var_y <- .16
sd_y <- sqrt(var_y)

cum_prob <- c(.2, 1)
mu_z <- diff(cum_prob)
var_z <- sapply(mu_z, function(x) x * (1 - x))

cov_xy <- .05

# Generating data ---------------------------------------------------------
vcov_xy <- matrix(c(var_x, cov_xy, cov_xy, var_y), 2)
xy <- setNames(data.frame(mvtnorm::rmvnorm(n, mean = c(mu_x, mu_y), sigma = vcov_xy)),
               c("x", "y"))
q_y_cum <- c(-Inf, qnorm(p = cum_prob, mean = mu_y, sd = sd_y))
q_y_abs <- qnorm(p = mu_z, mean = mu_y, sd = sd_y)
xyz <- data.frame(xy, z = cut(xy$y, q_y_cum, labels = 0:1))
print(summary(xyz))
reg_xyz <- lm(x ~ y + z, xyz)

# Calculating vcov_xyz -----------------------------------------------------

# Numerical solution
xyz_given_z0 <- xyz[xyz$z == 0, ]
xyz_given_z1 <- xyz[xyz$z == 1, ]
print(identical(xyz[xyz$z == 0, ], xyz[xyz$y < q_y_cum[2], ]))
print(identical(xyz[xyz$z == 1, ], xyz[xyz$y > q_y_cum[2], ]))
cov_yz <- mean(xyz_given_z1$y) * mu_z - mu_z * mu_y
cov_xz <- mean(xyz_given_z1$x) * mu_z - mu_z * mu_x

# Analytical solution
stnorm_y <- (q_y_cum[2] - mu_y) / sd_y
inv_mills_ratio <- dnorm(stnorm_y) / (1 - pnorm(stnorm_y))

mu_y_given_y <- mu_y + sd_y * inv_mills_ratio  # truncated normal
mu_x_given_y <- mu_x + (cov_xy / sd_y) * inv_mills_ratio # mvn

cov_yz <- mu_y_given_y * mu_z - mu_y * mu_z
cov_xz <- mu_x_given_y * mu_z - mu_x * mu_z

# Final assembly
vcov_xyz <- matrix(c(var_x, cov_xy, cov_xz,
                     cov_xy, var_y, cov_yz,
                     cov_xz, cov_yz, var_z), 3)
print(vcov_xyz)

# Checking regression coefficients ----------------------------------------
beta_xyz <- solve(vcov_xyz[-1, -1], vcov_xyz[1, -1])
alpha_xyz <- mu_x - crossprod(beta_xyz, c(mu_y, mu_z))

cat("\nObserved and expected regression coefficients\n")
print(rbind(obs = coef(reg_xyz), exp = c(alpha_xyz, beta_xyz)))

cat("\nStandardized differences:\n")
std_diff <- (coef(reg_xyz) - c(alpha_xyz, beta_xyz)) / c(alpha_xyz, beta_xyz)

test_that("Numerical and analytical solutions are close: binary W", {
  expect_lte(max(std_diff), 0.01)
})
