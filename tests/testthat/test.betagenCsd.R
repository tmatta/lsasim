# Temporary loading of packages for manual testing ----------------------------
library(lsasim)
library(devtools)
library(testthat)
packageVersion("lsasim")

# Actual test -----------------------------------------------------------------
context("beta_gen() behavior with n_W > 0 and c_sd != 1")

# Generating data -------------------------------------------------------------
qGenWrap <- function(nX, nW, sd = NULL, n = 10e3) {
	# ad-hoc wrapper for questionnaire_gen()
	data <- questionnaire_gen(n_obs       = n,
                            n_X         = nX,
                            n_W         = nW,
                            c_sd        = sd,
                            theta       = TRUE,
                            full_output = TRUE,
                            family      = "gaussian",
                            verbose = FALSE)
  if (nX > 0) {
    cols_continuous <- c("theta", paste0("q", 1:nX))
  } else {
    cols_continuous <- c("theta")
  }
  sds <- apply(data$bg[cols_continuous], 2, sd)
  betas <- beta_gen(data, MC = TRUE, verbose = FALSE, CI = c(.000001, .999999))
  return(list(data = data, sds = sds, betas = betas))
}

# The numbering logic below is df_abc, where the "abc" contains numbers for n_X, n_W and sd

df_101  <- qGenWrap(1, 0, 2:3)
df_011b <- qGenWrap(0, list(2), 2)
df_011t <- qGenWrap(0, list(3), 2)
df_111b <- qGenWrap(1, list(2), 2:3)
df_111t <- qGenWrap(1, list(3), 2:3)
df_229m <- qGenWrap(2, list(2, 3), 7:9)

# Checking means -------------------------------------------------------------
test_that("Observed means equal expectations", {
  expect_equivalent(df_101$sds,  2:3, tolerance = 0.1)
  expect_equivalent(df_011b$sds, 2,   tolerance = 0.1)
  expect_equivalent(df_011t$sds, 2,   tolerance = 0.1)
  expect_equivalent(df_111b$sds, 2:3, tolerance = 0.1)
  expect_equivalent(df_111t$sds, 2:3, tolerance = 0.1)
  expect_equivalent(df_229m$sds, 7:9, tolerance = 0.1)
})

# Checking betas --------------------------------------------------------------
test_that("Estimated true betas are equivalent to MC estimates", {
  expect_equivalent(df_101$betas[, 5],  c(1, 1))
  expect_equivalent(df_011b$betas[, 5],  c(1, 1))
  expect_equivalent(df_011t$betas[, 5],  c(1, 1, 1))
  expect_equivalent(df_111b$betas[, 5],  c(1, 1, 1))
  expect_equivalent(df_111t$betas[, 5],  c(1, 1, 1, 1))

})