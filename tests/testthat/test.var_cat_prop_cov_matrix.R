context("Correspondence between cat_prop and cov_matrix")

# Binomial W --------------------------------------------------------------
cat1prop <- runif(2)

propW    <- list(c(cat1prop[1], 1), c(cat1prop[2], 1))
propXW   <- list(1, c(cat1prop[1], 1), c(cat1prop[2], 1))
propYXW  <- list(1, 1, c(cat1prop[1], 1), c(cat1prop[2], 1))

dfW   <- questionnaire_gen(100, propW, full_output = TRUE)
dfXW  <- questionnaire_gen(100, propXW, full_output = TRUE)
dfYXW <- questionnaire_gen(100, propYXW, full_output = TRUE, theta = TRUE)

varCatPropW   <- sapply(propW, function(x) x[1] * (1 - x[1]))
varCatPropXW  <- sapply(propXW, function(x) x[1] * (1 - x[1]))[-1]
varCatPropYXW <- sapply(propYXW, function(x) x[1] * (1 - x[1]))[-1:-2]

varCovMatrixW   <- diag(dfW$cov_matrix)
varCovMatrixXW  <- diag(dfXW$cov_matrix)[-1]
varCovMatrixYXW <- diag(dfYXW$cov_matrix)[-1:-2]

test_that("Proportions preserved for binomial W", {
  expect_equivalent(varCatPropW, varCovMatrixW)
  expect_equivalent(varCatPropXW, varCovMatrixXW)
  expect_equivalent(varCatPropYXW, varCovMatrixYXW)
})

# Polynomial W ------------------------------------------------------------
catW1prop <- punif(cumsum(runif(2, max = 1/2)))
catW2prop <- punif(cumsum(runif(3, max = 1/3)))

propW   <- list(c(catW1prop, 1), c(catW2prop, 1))
propXW  <- list(1, c(catW1prop, 1), c(catW2prop, 1))
propYXW <- list(1, 1, c(catW1prop, 1), c(catW2prop, 1))

dfW   <- questionnaire_gen(100, propW, full_output = TRUE)
dfXW  <- questionnaire_gen(100, propXW, full_output = TRUE)
dfYXW <- questionnaire_gen(100, propYXW, full_output = TRUE, theta = TRUE)

propW_abs <- unlist(lapply(propW, function(x) c(x[1], diff(x))))
varCatPropW   <- sapply(propW, function(x) x[1] * (1 - x[1]))
varCatPropXW  <- sapply(propXW, function(x) x[1] * (1 - x[1]))[-1]
varCatPropYXW <- sapply(propYXW, function(x) x[1] * (1 - x[1]))[-1:-2]

varCovMatrixW   <- diag(dfW$cov_matrix)
varCovMatrixXW  <- diag(dfXW$cov_matrix)[-1]
varCovMatrixYXW <- diag(dfYXW$cov_matrix)[-1:-2]

test_that("Proportions preserved for polynomial W", {
  expect_equivalent(varCatPropW, varCovMatrixW)
  expect_equivalent(varCatPropXW, varCovMatrixXW)
  expect_equivalent(varCatPropYXW, varCovMatrixYXW)
})
