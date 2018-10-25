context("Regression coefficients are calculated correctly")

dfX <- questionnaire_gen(100, family = "gaussian", theta = TRUE,
                         full_output = TRUE, n_X = 2, n_W = 0)
dfW <- questionnaire_gen(100, family = "gaussian", theta = TRUE,
                         full_output = TRUE, n_X = 0, n_W = list(2))
calcPctIn <- function(data) {
  estimates <- replicate(50, suppressMessages(beta_gen(data, MC = TRUE, replications = 50)))
  pct_coverage <- estimates[, "cov_in_CI", ]
  avg_coverage <- apply(pct_coverage, 1, mean)
  return(avg_coverage)
}
pct_dfX <- calcPctIn(dfX)
pct_dfW <- calcPctIn(dfW)

test_that("data with n_X", expect_gt(min(pct_dfX), .5))
test_that("data with n_W", expect_gt(min(pct_dfW), .5))
