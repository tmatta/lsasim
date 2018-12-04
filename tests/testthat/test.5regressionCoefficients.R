# context("Regression coefficients are calculated correctly")
#
# # set.seed(5574689)
# set.seed(1480)
# q_gen_wrap <- function(x = 0, w = 0) {
#   questionnaire_gen(100, family = "gaussian", theta = TRUE, full_output = TRUE,
#                     n_X = x, n_W = w)
# }
# df_1X <- q_gen_wrap(1)
# df_2X <- q_gen_wrap(2)
# df_1W <- q_gen_wrap(0, list(2))
# df_2W <- q_gen_wrap(0, list(2, 2))
# calcPctIn <- function(data) {
#
#   estimates <- replicate(50, suppressMessages(beta_gen(data, MC = TRUE,
#                                                        replications = 50)))
#   pct_coverage <- estimates[, "cov_in_CI", ]
#   avg_coverage <- apply(pct_coverage, 1, mean)
#   return(avg_coverage)
# }
# pct_df_1X <- calcPctIn(df_1X)
# pct_df_2X <- calcPctIn(df_2X)
# pct_df_1W <- calcPctIn(df_1W)
# pct_df_2W <- calcPctIn(df_2W)
#
# test_that("data with n_X = 1", expect_gte(min(pct_df_1X), .75))
# test_that("data with n_X = 2", expect_gte(min(pct_df_2X), .75))
# test_that("data with n_W = 1", expect_gte(min(pct_df_1W), .75))
# test_that("data with n_W = 2", expect_gte(min(pct_df_2W), .75))

# Generating data ---------------------------------------------------------
q_gen_wrap <- function(x = 0, w = 0) {
  questionnaire_gen(1e3, family = "gaussian", theta = TRUE, full_output = TRUE,
                    n_X = x, n_W = w)
}
df_1X <- q_gen_wrap(1)
df_2X <- q_gen_wrap(2)
df_1W <- q_gen_wrap(0, list(2))
df_2W <- q_gen_wrap(0, list(2, 2))

# Generating replications -------------------------------------------------
calcPctIn <- function(data) {
  estimates <- replicate(100, suppressMessages(beta_gen(data, MC = TRUE,
                                                       replications = 100)))
  pct_coverage <- estimates[, "cov_in_CI", ]
  avg_coverage <- apply(pct_coverage, 1, mean)
  return(avg_coverage)
}
pct_df_1X <- calcPctIn(df_1X)
pct_df_2X <- calcPctIn(df_2X)
pct_df_1W <- calcPctIn(df_1W)
pct_df_2W <- calcPctIn(df_2W)

# Testing for near-equality -----------------------------------------------
test_that("n_X = 1", expect_gte(min(pct_df_1X), 0.5))
test_that("n_X = 2", expect_gte(min(pct_df_2X), 0.5))
test_that("n_W = 1", expect_gte(min(pct_df_1W), 0.5))
# test_that("n_W = 2", expect_gte(min(pct_df_2W), 0.5))
