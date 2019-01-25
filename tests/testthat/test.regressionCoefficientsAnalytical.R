# Setup -------------------------------------------------------------------
context("Script vs. beta_gen-calculated regression coefficients")

n <- 1e4
cum_prob_xw    <- list(1, 1, 1, c(.4, 1), c(.3, .8, 1))
cum_prob_yx1   <- list(1, 1)
cum_prob_yx    <- list(1, 1, 1)
cum_prob_yw1   <- list(1, c(.4, 1))
cum_prob_yw2   <- list(1, c(.3, .8, 1))
cum_prob_yw    <- list(1, c(.4, 1), c(.3, .8, 1))
cum_prob_yx1w1 <- list(1, 1, c(.4, 1))
cum_prob_yx1w2 <- list(1, 1, c(.3, .8, 1))
cum_prob_yx1w  <- list(1, 1, c(.4, 1), c(.3, .8, 1))
cum_prob_yxw1  <- list(1, 1, 1, c(.4, 1))
cum_prob_yxw2  <- list(1, 1, 1, c(.3, .8, 1))
cum_prob_yxw   <- list(1, 1, 1, c(.4, 1), c(.3, .8, 1))
cov_mx_yxw <- matrix(c(1, .1, .1, .2, .2,
                      .1, 1, .3, .4, .4,
                      .1, .3, 1, .4, .4,
                      .2, .4, .4, 1, .5,
                      .2, .4, .4, .5, 1), 5)
cov_mx_yx1   <- cov_mx_yxw[c(1, 2), c(1, 2)]
cov_mx_yx    <- cov_mx_yxw[c(1, 2, 3), c(1, 2, 3)]
cov_mx_yw1   <- cov_mx_yxw[c(1, 4), c(1, 4)]
cov_mx_yw2   <- cov_mx_yxw[c(1, 5), c(1, 5)]
cov_mx_yw    <- cov_mx_yxw[c(1, 4, 5), c(1, 4, 5)]
cov_mx_yx1w1 <- cov_mx_yxw[c(1, 2, 4), c(1, 2, 4)]
cov_mx_yx1w2 <- cov_mx_yxw[c(1, 2, 5), c(1, 2, 5)]
cov_mx_yx1w  <- cov_mx_yxw[c(1, 2, 4, 5), c(1, 2, 4, 5)]
cov_mx_yxw1  <- cov_mx_yxw[c(1, 2, 3, 4), c(1, 2, 3, 4)]
cov_mx_yxw2  <- cov_mx_yxw[c(1, 2, 3, 5), c(1, 2, 3, 5)]
mu_yx <- 0:2
sd_yx <- 1:3

# Generate data -----------------------------------------------------------
q_gen <- function(data, cov_mx, mean) {
  suppressMessages(questionnaire_gen(n, data, cov_matrix = cov_mx, theta = TRUE,
                                     family = "gaussian", full_output = TRUE,
                                     c_mean = mean))
}

df_yx1   <- q_gen(cum_prob_yx1, cov_mx_yx1, mu_yx[c(1, 2)])
df_yx    <- q_gen(cum_prob_yx, cov_mx_yx, mu_yx[c(1, 2, 3)])
df_yw1   <- q_gen(cum_prob_yw1, cov_mx_yw1, mu_yx[c(1)])
df_yw2   <- q_gen(cum_prob_yw2, cov_mx_yw2, mu_yx[c(1)])
df_yw    <- q_gen(cum_prob_yw, cov_mx_yw, mu_yx[c(1)])
df_yx1w1 <- q_gen(cum_prob_yx1w1, cov_mx_yx1w1, mu_yx[c(1, 2)])
df_yx1w2 <- q_gen(cum_prob_yx1w2, cov_mx_yx1w2, mu_yx[c(1, 2)])
df_yx1w  <- q_gen(cum_prob_yx1w, cov_mx_yx1w, mu_yx[c(1:2)])
df_yxw1  <- q_gen(cum_prob_yxw1, cov_mx_yxw1, mu_yx[c(1:3)])
df_yxw2  <- q_gen(cum_prob_yxw2, cov_mx_yxw2, mu_yx[c(1:3)])
df_yxw   <- q_gen(cum_prob_yxw, cov_mx_yxw, mu_yx[c(1:3)])

yx1   <- setNames(df_yx1$bg, c("subject", "y", "x1"))
yx    <- setNames(df_yx$bg, c("subject", "y", "x1", "x2"))
yw1   <- setNames(df_yw1$bg, c("subject", "y", "w1"))
yw2   <- setNames(df_yw2$bg, c("subject", "y", "w2"))
yw    <- setNames(df_yw$bg, c("subject", "y", "w1", "w2"))
yx1w1 <- setNames(df_yx1w1$bg, c("subject", "y", "x1", "w1"))
yx1w2 <- setNames(df_yx1w2$bg, c("subject", "y", "x1","w2"))
yx1w  <- setNames(df_yx1w$bg, c("subject", "y", "x1", "w1", "w2"))
yxw1  <- setNames(df_yxw1$bg, c("subject", "y", "x1", "x2", "w1"))
yxw2  <- setNames(df_yxw2$bg, c("subject", "y", "x1", "x2", "w2"))
yxw   <- setNames(df_yxw$bg, c("subject", "y", "x1", "x2", "w1", "w2"))

# Test data (to be removed) ------------------------------------------
mu_yxz  <- list(y = mu_yx[1], x1 = mu_yx[2], x2 = mu_yx[3], z1 = 0, z2 = 0)
var_yxz <- list(y = sd_yx[1] ^ 2, x1 = sd_yx[2] ^ 2, x2 = sd_yx[3] ^ 2, z1 = 1, z2 = 1)
sd_yxz  <- lapply(var_yxz, sqrt)
num_yx <- 3
cols_z <- 4:5

# Discrete variables generated from Z
cum_prob <- list(w1 = c(0, .4, 1), w2 = c(0, .3, .8, 1))  # != format from q_gen
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
vcov_yxz <- cov_mx_yxw
dimnames(vcov_yxz) <- list(names(mu_yxz), names(mu_yxz))
q_z <- lapply(cum_prob, function(x) qnorm(x, mean = 0, sd = 1))  # Z ~ N(0, 1)

# Regressions -------------------------------------------------------------
reg_yx1   <- lm(y ~ x1               , yx1)
reg_yx    <- lm(y ~ x1 + x2          , yx)
reg_yw1   <- lm(y ~           w1     , yw1)
reg_yw2   <- lm(y ~                w2, yw2)
reg_yw    <- lm(y ~           w1 + w2, yw)
reg_yx1w1 <- lm(y ~ x1 +      w1     , yx1w1)
reg_yx1w2 <- lm(y ~ x1 +           w2, yx1w2)
reg_yx1w  <- lm(y ~ x1 +      w1 + w2, yx1w)
reg_yxw1  <- lm(y ~ x1 + x2 + w1     , yxw1)
reg_yxw2  <- lm(y ~ x1 + x2 +      w2, yxw2)
reg_yxw   <- lm(y ~ x1 + x2 + w1 + w2, yxw)

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
exp_x1w <- exp_AB2(names_w, mu_yxz$x1, mu_w, cov_xz, q_z, sd_Y = sd_yxz$y)
exp_x2w <- exp_AB2(names_w, mu_yxz$x2, mu_w, cov_xz, q_z, sd_Y = sd_yxz$y)

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
vcov_yxw[4, 4] <- vcov_w$w1
vcov_yxw[5:6, 5:6] <- vcov_w$w2

# Adding Cov(W, W) for different Zs
calc_p_mvn_trunc <- function(lo, up, sig, mu = c(0, 0)) {
  mvtnorm::pmvnorm(lo, up, mu, sig)[1] / (pnorm(up[2]) - pnorm(lo[2]))
}

vcov_yxw["w1", c("w21", "w22")] <- vcov_yxw[c("w21", "w22"), "w1"] <-
  c(calc_p_mvn_trunc(c(q_z$w1[2], q_z$w2[2]), c(q_z$w1[3], q_z$w2[3]), sig = matrix(c(1, cov_zz, cov_zz, 1), 2)) * .5 - .6 * .5,
    calc_p_mvn_trunc(c(q_z$w1[2], q_z$w2[3]), c(q_z$w1[3], q_z$w2[4]), sig = matrix(c(1, cov_zz, cov_zz, 1), 2)) * .2 - .6 * .2)

# Creating submatrices
vcov_yx1   <- vcov_yxw[c("y", "x1"), c("y", "x1")]
vcov_yx    <- vcov_yxw[c("y", "x1", "x2"), c("y", "x1", "x2")]
vcov_yw1   <- vcov_yxw[c("y", "w1"), c("y", "w1")]
vcov_yw2   <- vcov_yxw[c("y", "w21", "w22"), c("y", "w21", "w22")]
vcov_yx1w1 <- vcov_yxw[c("y", "x1", "w1"), c("y", "x1", "w1")]
vcov_yx1w2 <- vcov_yxw[c("y", "x1", "w21", "w22"), c("y", "x1", "w21", "w22")]
vcov_yw    <- vcov_yxw[c("y", "w1", "w21", "w22"), c("y", "w1", "w21", "w22")]
vcov_yx1w  <- vcov_yxw[c("y", "x1", "w1", "w21", "w22"), c("y", "x1", "w1", "w21", "w22")]
vcov_yxw1  <- vcov_yxw[c("y", "x1", "x2", "w1"), c("y", "x1", "x2", "w1")]
vcov_yxw2  <- vcov_yxw[c("y", "x1", "x2", "w21", "w22"), c("y", "x1", "x2", "w21", "w22")]

# Calculating regression coefficients -------------------------------------
mu_xw <- c("x1" = mu_yxz$x1, "x2" = mu_yxz$x2, mu_w_minus_1)

calcRegCoeff <- function(cov, mu_y, mu_x) {
  beta <- solve(cov[-1, -1], cov[1, -1])
  alpha <- mu_y - crossprod(beta, mu_x)
  names_coeff <- c("theta", names(mu_x))
  out <- setNames(unlist(list(alpha, beta)), names_coeff)
  return(out)
}

cov_reg_yx1   <- calcRegCoeff(vcov_yx1, mu_yxz$y, mu_xw[c(1)])
cov_reg_yx    <- calcRegCoeff(vcov_yx, mu_yxz$y, mu_xw[c(1, 2)])
cov_reg_yw1   <- calcRegCoeff(vcov_yw1, mu_yxz$y, mu_xw[c(3)])
cov_reg_yw2   <- calcRegCoeff(vcov_yw2, mu_yxz$y, mu_xw[c(4, 5)])
cov_reg_yx1w1 <- calcRegCoeff(vcov_yx1w1, mu_yxz$y, mu_xw[c(1, 3)])
cov_reg_yx1w2 <- calcRegCoeff(vcov_yx1w2, mu_yxz$y, mu_xw[c(1, 4, 5)])
cov_reg_yw    <- calcRegCoeff(vcov_yw, mu_yxz$y, mu_xw[c(3, 4, 5)])
cov_reg_yx1w  <- calcRegCoeff(vcov_yx1w, mu_yxz$y, mu_xw[c(1, 3, 4, 5)])
cov_reg_yxw1  <- calcRegCoeff(vcov_yxw1, mu_yxz$y, mu_xw[c(1, 2, 3)])
cov_reg_yxw2  <- calcRegCoeff(vcov_yxw2, mu_yxz$y, mu_xw[c(1, 2, 4, 5)])
cov_reg_yxw   <- calcRegCoeff(vcov_yxw, mu_yxz$y, mu_xw)

# Using beta_gen
beta_reg_yx1   <- beta_gen(df_yx1)
beta_reg_yx    <- beta_gen(df_yx)
beta_reg_yw1   <- beta_gen(df_yw1)
beta_reg_yw2   <- beta_gen(df_yw2)
beta_reg_yx1w1 <- beta_gen(df_yx1w1)
beta_reg_yx1w2 <- beta_gen(df_yx1w2)
beta_reg_yw    <- beta_gen(df_yw)
beta_reg_yxw1  <- beta_gen(df_yxw1)
beta_reg_yxw2  <- beta_gen(df_yxw2)
beta_reg_yx1w  <- beta_gen(df_yx1w)
beta_reg_yxw   <- beta_gen(df_yxw)

# Benchmarking regression coefficients ------------------------------------
compareRegScrBeta <- function(reg_data, cov_data, beta_gen_data, print = FALSE) {
  tab <- rbind("reg" = coef(reg_data),
               "script" = cov_data,
               "beta_gen" = beta_gen_data)
  if (print) print(tab)
  diffs <- apply(tab, 2, function(x) max(c(x[2] - x[1],
                                           x[3] - x[1],
                                           x[3] - x[2])))
  return(list(tab = tab, diffs = diffs))
}
comp_yx1 <- compareRegScrBeta(reg_yx1, cov_reg_yx1, beta_reg_yx1)
comp_yx  <- compareRegScrBeta(reg_yx, cov_reg_yx, beta_reg_yx)
comp_yw1 <- compareRegScrBeta(reg_yw1, cov_reg_yw1, beta_reg_yw1)
comp_yw2 <- compareRegScrBeta(reg_yw2, cov_reg_yw2, beta_reg_yw2)
comp_yw  <- compareRegScrBeta(reg_yw, cov_reg_yw, beta_reg_yw)
comp_yx1w1 <- compareRegScrBeta(reg_yx1w1, cov_reg_yx1w1, beta_reg_yx1w1)
comp_yx1w2 <- compareRegScrBeta(reg_yx1w2, cov_reg_yx1w2, beta_reg_yx1w2)
comp_yxw1 <- compareRegScrBeta(reg_yxw1, cov_reg_yxw1, beta_reg_yxw1)
comp_yxw2 <- compareRegScrBeta(reg_yxw2, cov_reg_yxw2, beta_reg_yxw2)
comp_yx1w <- compareRegScrBeta(reg_yx1w, cov_reg_yx1w, beta_reg_yx1w)
comp_yxw <- compareRegScrBeta(reg_yxw, cov_reg_yxw, beta_reg_yxw)

test_that("Script and beta_gen solutions are equivalent", {
  expect_equal(comp_yx1$tab["script", ], comp_yx1$tab["beta_gen", ])
  expect_equal(comp_yx$tab["script", ], comp_yx$tab["beta_gen", ])
  expect_equal(comp_yw1$tab["script", ], comp_yw1$tab["beta_gen", ])
  expect_equal(comp_yw2$tab["script", ], comp_yw2$tab["beta_gen", ])
  expect_equal(comp_yx1w1$tab["script", ], comp_yx1w1$tab["beta_gen", ])
  expect_equal(comp_yx1w2$tab["script", ], comp_yx1w2$tab["beta_gen", ])
  expect_equal(comp_yw$tab["script", ], comp_yw$tab["beta_gen", ])
  expect_equal(comp_yxw1$tab["script", ], comp_yxw1$tab["beta_gen", ])
  expect_equal(comp_yxw2$tab["script", ], comp_yxw2$tab["beta_gen", ])
  expect_equal(comp_yx1w$tab["script", ], comp_yx1w$tab["beta_gen", ])
  expect_equal(comp_yxw$tab["script", ], comp_yxw$tab["beta_gen", ])
})

test_that("Numerical and analytical solutions are close", {
  expect_lt(max(comp_yx1$tab["reg", ]   - comp_yx1$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yx$tab["reg", ]    - comp_yx$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yw1$tab["reg", ]   - comp_yw1$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yw2$tab["reg", ]   - comp_yw2$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yx1w1$tab["reg", ] - comp_yx1w1$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yx1w2$tab["reg", ] - comp_yx1w2$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yw$tab["reg", ]    - comp_yw$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yxw1$tab["reg", ]  - comp_yxw1$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yxw2$tab["reg", ]  - comp_yxw2$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yx1w$tab["reg", ]  -  comp_yx1w$tab["beta_gen", ]), 0.1)
  expect_lt(max(comp_yxw$tab["reg", ]   - comp_yxw$tab["beta_gen", ]), 0.1)
})
