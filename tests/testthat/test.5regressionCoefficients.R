context("Regression coefficients are calculated correctly")

# Simple case -------------------------------------------------------------
betaX <- beta_gen(questionnaire_gen(1e3, n_X = 1, n_W = 0, theta = TRUE,
                                    full_output = TRUE), MC = TRUE)
betaXX <- beta_gen(questionnaire_gen(1e3, n_X = 2, n_W = 0, theta = TRUE,
                                     full_output = TRUE), MC = TRUE)
betaW2 <- beta_gen(questionnaire_gen(1e3, n_X = 0, n_W = list(2), theta = TRUE,
                                     full_output = TRUE), MC = TRUE)
<<<<<<< Updated upstream

# TODO: test W2W2, XW2, XXW2, XW2W2, XXW2W2, W3, W4...
# Generating replications -------------------------------------------------
calcPctIn <- function(data) {
  estimates <- replicate(100, suppressMessages(beta_gen(data, MC = TRUE,
                                                        replications = 100,
                                                        analytical = FALSE)))
  pct_coverage <- estimates[, "cov_in_CI", ]
  avg_coverage <- apply(pct_coverage, 1, mean)
  return(avg_coverage)
}
pct_df_1X <- calcPctIn(df_1X)
pct_df_2X <- calcPctIn(df_2X)
pct_df_1W <- calcPctIn(df_1W)
pct_df_2W <- calcPctIn(df_2W)
# TODO: test W2W2, XW2, XXW2, XW2W2, XXW2W2, W3, W4...

=======

# TODO: test W2W2, XW2, XXW2, XW2W2, XXW2W2, W3, W4...

>>>>>>> Stashed changes
test_that("Beta hat from covariance inside MC CI", {
  expect_equivalent(betaX[, "cov_in_CI"], c(1, 1))
  expect_equivalent(betaXX[, "cov_in_CI"], c(1, 1, 1))
  expect_equivalent(betaW[, "cov_in_CI"], c(1, 1))
})


# Calculating coverage ----------------------------------------------------
# Generating data ---------------------------------------------------------
# q_gen_wrap <- function(x = 0, w = 0) {
#   questionnaire_gen(1e3, family = "gaussian", theta = TRUE, full_output = TRUE,
#                     n_X = x, n_W = w)
# }
# df_1X <- q_gen_wrap(1)
# df_2X <- q_gen_wrap(2)
# df_1W <- q_gen_wrap(0, list(2))
# df_2W <- q_gen_wrap(0, list(2, 2))
#
# # Generating replications -------------------------------------------------
# calcPctIn <- function(data) {
#   estimates <- replicate(100, suppressMessages(beta_gen(data, MC = TRUE,
#                                                        replications = 100)))
#   pct_coverage <- estimates[, "cov_in_CI", ]
#   avg_coverage <- apply(pct_coverage, 1, mean)
#   return(avg_coverage)
# }
# pct_df_1X <- calcPctIn(df_1X)
# pct_df_2X <- calcPctIn(df_2X)
# pct_df_1W <- calcPctIn(df_1W)
# pct_df_2W <- calcPctIn(df_2W)
#
# # Testing for near-equality -----------------------------------------------
# test_that("n_X = 1", expect_gte(min(pct_df_1X), 0.5))
# test_that("n_X = 2", expect_gte(min(pct_df_2X), 0.5))
# test_that("n_W = 1", expect_gte(min(pct_df_1W), 0.5))
# test_that("n_W = 2", expect_gte(min(pct_df_2W), 0.5))
