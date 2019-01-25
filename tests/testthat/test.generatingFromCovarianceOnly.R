context("Variable names from a matrix are passed to the output")

items <- c("Q1", "Q5", "Q9", "Q13")
covar <- matrix(c(7, 3, 2, 7,
                  3, 9,-1, 0,
                  2,-1, 8, 4,
                  7, 0, 4, 9), 4)
dimnames(covar) <- list(items, items)
correl <- cov2cor(covar)
df_cov <- questionnaire_gen(10, cov_matrix = covar)
df_cor <- questionnaire_gen(10, cor_matrix = correl)

test_that("Names are preserved", {
  expect_identical(names(df_cov)[-1], items)
  expect_identical(names(df_cor)[-1], items)
})

context("Streching n_W according to the number of variables")

df_3X1W <- questionnaire_gen(10, cov_matrix = covar, n_X = 3, n_W = list(4),
                             family = "gaussian")
df_2X2W <- questionnaire_gen(10, cov_matrix = covar, n_X = 2, n_W = list(4),
                             family = "gaussian")
df_1X3W <- questionnaire_gen(10, cov_matrix = covar, n_X = 1, n_W = list(4),
                             family = "gaussian")
df_0X4W <- questionnaire_gen(10, cov_matrix = covar, n_X = 0, n_W = list(4),
                             family = "gaussian")

test_that("n_W is expanded correctly", {
  expect_equivalent(sapply(df_3X1W[5], function(x) length(levels(x))), rep(4, 1))
  expect_equivalent(sapply(df_2X2W[4:5], function(x) length(levels(x))), rep(4, 2))
  expect_equivalent(sapply(df_1X3W[3:5], function(x) length(levels(x))), rep(4, 3))
  expect_equivalent(sapply(df_0X4W[2:5], function(x) length(levels(x))), rep(4, 4))
})
