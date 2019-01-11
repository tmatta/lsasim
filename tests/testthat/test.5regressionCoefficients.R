context("Regression coefficients are calculated correctly")
set.seed(240)

# Generating data ---------------------------------------------------------
q_gen_wrap <- function(x = 0, w = 0) {
  suppressMessages(questionnaire_gen(1e3, family = "gaussian", theta = TRUE,
                                     full_output = TRUE, n_X = x, n_W = w))
}
df_1X <- q_gen_wrap(1)
df_2X <- q_gen_wrap(2)
df_1W <- q_gen_wrap(0, list(2))
df_2W <- q_gen_wrap(0, list(2, 2))
df_1V <- q_gen_wrap(0, list(3))
df_1X1W <- q_gen_wrap(1, list(2))
df_2X1W <- q_gen_wrap(2, list(2))
df_1X1V <- q_gen_wrap(1, list(3))
df_2X1V <- q_gen_wrap(2, list(3))
df_2X2W2V <- q_gen_wrap(2, list(2, 2, 3, 4))

# Generating replications -------------------------------------------------
calcPctIn <- function(data) {
  estimates <- replicate(n    = 10,
                         expr = suppressMessages(beta_gen(data, MC = TRUE,
                                                          replications = 50)))
  pct_coverage <- estimates[, "cov_in_CI", ]
  avg_coverage <- apply(pct_coverage, 1, mean)
  return(list(estimates      = estimates,
              differences    = estimates[, "cov_matrix", ] - estimates[, "MC", ],
              coverage_ratio = avg_coverage))
}
pct_df_1X <- calcPctIn(df_1X)
pct_df_2X <- calcPctIn(df_2X)
pct_df_1W <- calcPctIn(df_1W)
pct_df_1V <- calcPctIn(df_1V)
pct_df_1X1W <- calcPctIn(df_1X1W)
pct_df_1X1V <- calcPctIn(df_1X1V)
pct_df_2W <- calcPctIn(df_2W)
pct_df_2X1W <- calcPctIn(df_2X1W)
pct_df_2X1V <- calcPctIn(df_2X1V)
pct_df_2X2W2V <- calcPctIn(df_2X2W2V)

# Testing for near-equality -----------------------------------------------
test_that("Betas from covariance matrix match those from resampling", {
  expect_gte(min(pct_df_1X$coverage), 0.9)
  expect_gte(min(pct_df_2X$coverage), 0.9)
  expect_gte(min(pct_df_1W$coverage), 0.9)
  expect_gte(min(pct_df_1V$coverage), 0.9)
  expect_gte(min(pct_df_1X1W$coverage), 0.9)
  expect_gte(min(pct_df_1X1V$coverage), 0.9)
  expect_gte(min(pct_df_2W$coverage), 0.9)
  expect_gte(min(pct_df_2X1W$coverage), 0.9)
  expect_gte(min(pct_df_2X1V$coverage), 0.9)
  expect_gte(min(pct_df_2X2W2V$coverage), 0.9)
})

# Testing for correspondence between correlation and family-gen data ------
context("Betas behave equally for correlation and normal data")
cov_mx <- matrix(c(1, .5, .25, .5, 1, .25, .25, .25, .25), 3)

df_cor <- questionnaire_gen(1e5, theta = TRUE, cat_prop = list(1, 1, c(.5, 1)),
                            full_output = TRUE, cov_matrix = cov_mx)
df_fam <- questionnaire_gen(1e5, theta = TRUE, cat_prop = list(1, 1, c(.5, 1)),
                            full_output = TRUE, cov_matrix = cov_mx,
                            family = "gaussian")
beta_cor <- beta_gen(df_cor)
beta_fam <- beta_gen(df_fam)

test_that("Equivalent betas", expect_lte(max(abs(beta_cor - beta_fam)), 0.1))
