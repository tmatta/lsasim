context("Preservation of covariances and correlations from matrices")

# Generating datasets -----------------------------------------------------
n <- 1e4
dfX <- questionnaire_gen(n, theta = TRUE, full_output = TRUE, n_X = 1, n_W = 0)
dfW <- questionnaire_gen(n, theta = TRUE, full_output = TRUE, n_X = 0, n_W = 1)
dfWg <- questionnaire_gen(n, theta = TRUE, full_output = TRUE,
                          cat_prop = list(1, c(.4, .5, 1)), family = "gaussian")

# Calculating covariances and correlations --------------------------------
corYX_obs <- cor(dfX$bg$theta, dfX$bg$q1)
covYX_obs <- cov(dfX$bg$theta, dfX$bg$q1)
corYX_exp <- dfX$cor_matrix[1, 2]
covYX_exp <- dfX$cov_matrix[1, 2]

corYW_obs <- suppressWarnings(polycor::polyserial(dfW$bg$theta, dfW$bg$q1))
covYW_obs <- corYW_obs * sqrt(dfW$var_W[[1]]) * sqrt(dfW$var_YX)
corYW_exp <- dfW$cor_matrix[1, 2]
covYW_exp <- dfW$cov_matrix[1, 2]

corYWg_obs <- c(cor(dfWg$bg$theta, dfWg$bg$q1 == 2),
                cor(dfWg$bg$theta, dfWg$bg$q1 == 3))
covYWg_obs <- corYWg_obs * sqrt(dfWg$var_W[[1]][-1]) * sqrt(dfWg$var_YX)
covYWg_exp <- dfWg$linear_regression$vcov_YXW[1, 2:3]

# Running tests -----------------------------------------------------------
test_that("Covariances and correlations are preserved for n_X = 1, n_W = 0", {
  expect_equivalent(corYX_obs, corYX_exp, tolerance = 0.1)
  expect_equivalent(covYX_obs, covYX_exp, tolerance = 0.1)
})
test_that("Covariances and correlations are preserved for n_X = 0, n_W = 1", {
  expect_equivalent(corYW_obs, corYW_exp, tolerance = 0.1)
  expect_equivalent(covYW_obs, covYW_exp, tolerance = 0.1)
  expect_equivalent(covYWg_obs, covYWg_exp, tolerance = 0.1)
})
