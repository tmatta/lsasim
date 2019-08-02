# Temporary loading of packages for manual testing ----------------------------
#library(lsasim)
#packageVersion("lsasim")
#library(devtools)
#library(testthat)

# Actual test -----------------------------------------------------------------
context("beta_gen() behavior with n_W > 0 and c_means != 0")

# Generating data -------------------------------------------------------------
qGenWrap <- function(nX, nW, mu = NULL, n = 2e3) {
	# ad-hoc wrapper for questionnaire_gen()
	data <- suppressMessages(questionnaire_gen(n_obs       = n,
						            	                   n_X         = nX,
                                             n_W         = nW,
                                             c_mean      = mu,
                                             theta       = TRUE,
                                             full_output = TRUE,
                                             family      = "gaussian"))
  means <- apply(data$bg[-1], 2, mean)
  betas <- suppressMessages(beta_gen(data, MC = TRUE))
  return(list(data = data, means = means, betas = betas))
}

# The numbering logic below is df_abc, where the "abc" contains numbers for n_X, n_W and mu
df_100 <- qGenWrap(1, 0)
df_101 <- qGenWrap(1, 0, 1)

# Checking means -------------------------------------------------------------
test_that("Observed means equal expectations", {
	expect_equivalent(df_100$means, c(0, 0), tolerance = 0.1)
  expect_equivalent(df_101$means, c(1, 1), tolerance = 0.1)
})

# Checking betas --------------------------------------------------------------
test_that("Estimated true betas are equivalent to MC estimates", {
  expect_equivalent(df_100$betas[, 5], c(1, 1))
  expect_equivalent(df_101$betas[, 5], c(1, 1))
})