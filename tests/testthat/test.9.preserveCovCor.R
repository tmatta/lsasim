# context("Preservation of covariances and correlations from matrices")
#
#
# # Generating datasets -----------------------------------------------------
# n <- 1e3
# dfX <- questionnaire_gen(n, theta = TRUE, full_output = TRUE, n_X = 1, n_W = 0)
# dfW <- questionnaire_gen(n, theta = TRUE, full_output = TRUE, n_X = 0, n_W = 1)
#
# # Calculating covariances and correlations --------------------------------
# corYX_obs <- cor(dfX$bg$theta, dfX$bg$q1)
# covYX_obs <- cov(dfX$bg$theta, dfX$bg$q1)
# corYX_exp <- dfX$cor_matrix[1, 2]
# covYX_exp <- dfX$cov_matrix[1, 2]
#
# corYW_obs <- suppressWarnings(polycor::polyserial(dfW$bg$theta, dfW$bg$q1))
# covYW_obs <- corYW_obs * sqrt(dfW$var_W[[1]]) * sqrt(dfW$var_YX)
# corYW_exp <- dfW$cor_matrix[1, 2]
# covYW_exp <- dfW$cov_matrix[1, 2]
#
# # Running tests -----------------------------------------------------------
# test_that("Covariances and correlations are preserved for n_X = 1, n_W = 0", {
#   expect_equivalent(corYX_obs, corYX_exp, tolerance = 0.5)
#   expect_equivalent(covYX_obs, covYX_exp, tolerance = 0.5)
# })
# test_that("Covariances and correlations are preserved for n_X = 0, n_W = 1", {
#   expect_equivalent(corYW_obs, corYW_exp, tolerance = 0.5)
#   expect_equivalent(covYW_obs, covYW_exp, tolerance = 0.5)
# })
