# Temporary loading of packages for manual testing ----------------------------

# Actual test -----------------------------------------------------------------
context("beta_gen() behavior with n_W > 0 and c_means != 0")

# Generating data -------------------------------------------------------------
qGenWrap <- function(nX, nW, mu = NULL, n = 10e3) {
	# ad-hoc wrapper for questionnaire_gen()
	data <- questionnaire_gen(n_obs       = n,
                            n_X         = nX,
                            n_W         = nW,
                            c_mean      = mu,
                            theta       = TRUE,
                            full_output = TRUE,
                            family      = "gaussian",
                            verbose = FALSE)
  if (nX > 0) {
    cols_continuous <- c("theta", paste0("q", 1:nX))
  } else {
    cols_continuous <- c("theta")
  }
  means <- apply(data$bg[cols_continuous], 2, mean)
  betas <- beta_gen(data, MC = TRUE, verbose = FALSE, MC_replications = 100)
  return(list(data = data, means = means, betas = betas))
}

# The numbering logic below is df_abc, where the "abc" contains numbers for n_X, n_W and mu

# Zero mean
df_100  <- qGenWrap(1, 0)
df_010b <- qGenWrap(0, list(2))
df_010t <- qGenWrap(0, list(3))
df_110b <- qGenWrap(1, list(2))
df_110t <- qGenWrap(1, list(3))

# Positive means
df_101  <- qGenWrap(1, 0, 1)
df_011b <- qGenWrap(0, list(2), 1)
df_011t <- qGenWrap(0, list(3), 1)
df_111b <- qGenWrap(1, list(2), 1)
df_111t <- qGenWrap(1, list(3), 1)
df_229m <- qGenWrap(2, list(2, 3), 9)

# Negative means
df_101n  <- qGenWrap(1, 0, -1)
df_011nb <- qGenWrap(0, list(2), -1)
df_011nt <- qGenWrap(0, list(3), -1)
df_111nb <- qGenWrap(1, list(2), -1)
df_111nt <- qGenWrap(1, list(3), -1)
df_229nm <- qGenWrap(2, list(2, 3), -9)

# Mixed means
df_101m  <- qGenWrap(1, 0, c(10, -20))
df_111mb <- qGenWrap(1, list(2), c(20, -30))
df_111mt <- qGenWrap(1, list(3), c(30, -40))

# Checking means -------------------------------------------------------------
test_that("Observed means equal expectations", {
  # Zero means
	expect_equivalent(df_100$means,  c(0, 0), tolerance = 0.1)
  expect_equivalent(df_010b$means, c(0),    tolerance = 0.1)
  expect_equivalent(df_010t$means, c(0),    tolerance = 0.1)
  expect_equivalent(df_110b$means, c(0, 0), tolerance = 0.1)
  expect_equivalent(df_110t$means, c(0, 0), tolerance = 0.1)
  # Positive means
  expect_equivalent(df_101$means,  c(1, 1), tolerance = 0.1)
  expect_equivalent(df_011b$means, c(1),    tolerance = 0.1)
  expect_equivalent(df_011t$means, c(1),    tolerance = 0.1)
  expect_equivalent(df_111b$means, c(1, 1),  tolerance = 0.1)
  expect_equivalent(df_111t$means, c(1, 1),  tolerance = 0.1)
  # Negative means
  expect_equivalent(df_101n$means,  c(-1, -1), tolerance = 0.1)
  expect_equivalent(df_011nb$means, c(-1),    tolerance = 0.1)
  expect_equivalent(df_011nt$means, c(-1),    tolerance = 0.1)
  expect_equivalent(df_111nb$means, c(-1, -1),  tolerance = 0.1)
  expect_equivalent(df_111nt$means, c(-1, -1),  tolerance = 0.1)
  # Mixed means
  expect_equivalent(df_101m$means,  c(10, -20), tolerance = 0.1)
  expect_equivalent(df_111mb$means, c(20, -30),  tolerance = 0.1)
  expect_equivalent(df_111mt$means, c(30, -40),  tolerance = 0.1)
})

# Checking betas --------------------------------------------------------------
test_that("Estimated true betas are equivalent to MC estimates", {
  # Zero means
  expect_equivalent(df_100$betas[, 5],  c(1, 1))
  expect_equivalent(df_010b$betas[, 5], c(1, 1))
  expect_equivalent(df_010t$betas[, 5], c(1, 1, 1))
  expect_equivalent(df_110b$betas[, 5], c(1, 1, 1))
  expect_equivalent(df_110t$betas[, 5], c(1, 1, 1, 1))
  # Positive means
  expect_equivalent(df_101$betas[, 5],  c(1, 1))
  expect_equivalent(df_011b$betas[, 5],  c(1, 1))
  expect_equivalent(df_011t$betas[, 5],  c(1, 1, 1))
  expect_equivalent(df_111b$betas[, 5],  c(1, 1, 1))
  expect_equivalent(df_111t$betas[, 5],  c(1, 1, 1, 1))
  # Negative means
  expect_equivalent(df_101n$betas[, 5],  c(1, 1))
  expect_equivalent(df_011nb$betas[, 5],  c(1, 1))
  expect_equivalent(df_011nt$betas[, 5],  c(1, 1, 1))
  expect_equivalent(df_111nb$betas[, 5],  c(1, 1, 1))
  expect_equivalent(df_111nt$betas[, 5],  c(1, 1, 1, 1))
  # Mixed means
  expect_equivalent(df_101m$betas[, 5],  c(1, 1))
  expect_equivalent(df_111mb$betas[, 5],  c(1, 1, 1))
  expect_equivalent(df_111mt$betas[, 5],  c(1, 1, 1, 1))
})