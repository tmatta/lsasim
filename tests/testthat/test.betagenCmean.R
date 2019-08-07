# Actual test -----------------------------------------------------------------
context("beta_gen() behavior with n_W > 0 and c_means != 0")

# Generating data -------------------------------------------------------------
qGenWrap <- function(nX, nW, mu = NULL, n = 300, verbose = FALSE) {
  if (verbose) cat("n_X = ", nX, ", n_W = ", unlist(nW), ", mu = ",
                   ifelse(is.null(mu), 0, mu), "\n")
	# ad-hoc wrapper for questionnaire_gen()
	data <- questionnaire_gen(n_obs       = n,
                            n_X         = nX,
                            n_W         = nW,
                            c_mean      = mu,
                            theta       = TRUE,
                            full_output = TRUE,
                            family      = "gaussian",
                            verbose     = FALSE)
  if (nX > 0) {
    cols_continuous <- c("theta", paste0("q", 1:nX))
  } else {
    cols_continuous <- c("theta")
  }
  means <- apply(data$bg[cols_continuous], 2, mean)

  # Constructing a coverage percentage for the true betas
  betas <- list()
  for (rep in seq(50)) {
    if (verbose & (rep %% 5 == 0)) cat(rep, "")
    betas[[rep]] <- beta_gen(data    = data,
                             MC      = TRUE,
                             verbose = FALSE,
                             CI      = c(.0001, .9999))
  }
  if (verbose) cat("\n")
  coverages <- sapply(betas, function(x) x[, "cov_in_CI"])
  avg_coverage <- apply(coverages, 1, mean)

  return(list(data = data, means = means, avg_coverage = avg_coverage))
}

# The numbering logic below is df_abc, where the "abc" contains numbers of n_X amd n_W and for mu

# Zero mean
df_100  <- qGenWrap(1, 0)
df_010b <- qGenWrap(0, list(2))
# df_010t <- qGenWrap(0, list(3))
df_110b <- qGenWrap(1, list(2))
# df_110t <- qGenWrap(1, list(3))

# Positive means
df_101  <- qGenWrap(1, 0, 1)
df_011b <- qGenWrap(0, list(2), 1)
# df_011t <- qGenWrap(0, list(3), 1)
df_111b <- qGenWrap(1, list(2), 1)
# df_111t <- qGenWrap(1, list(3), 1)
df_229m <- qGenWrap(2, list(2, 3), 9)

# Negative means
df_101n  <- qGenWrap(1, 0, -1)
df_011nb <- qGenWrap(0, list(2), -1)
# df_011nt <- qGenWrap(0, list(3), -1)
df_111nb <- qGenWrap(1, list(2), -1)
# df_111nt <- qGenWrap(1, list(3), -1)
df_229nm <- qGenWrap(2, list(2, 3), -9)

# Mixed means
df_101m  <- qGenWrap(1, 0, c(10, -20))
df_111mb <- qGenWrap(1, list(2), c(20, -30))
# df_111mt <- qGenWrap(1, list(3), c(30, -40))

# Checking means -------------------------------------------------------------
test_that("Observed means equal expectations", {
  # Zero means
	expect_equivalent(df_100$means,  c(0, 0), tolerance = 0.2)
  expect_equivalent(df_010b$means, c(0),    tolerance = 0.2)
  # expect_equivalent(df_010t$means, c(0),    tolerance = 0.2)
  expect_equivalent(df_110b$means, c(0, 0), tolerance = 0.2)
  # expect_equivalent(df_110t$means, c(0, 0), tolerance = 0.2)
  # Positive means
  expect_equivalent(df_101$means,  c(1, 1), tolerance = 0.2)
  expect_equivalent(df_011b$means, c(1),    tolerance = 0.2)
  # expect_equivalent(df_011t$means, c(1),    tolerance = 0.2)
  expect_equivalent(df_111b$means, c(1, 1),  tolerance = 0.2)
  # expect_equivalent(df_111t$means, c(1, 1),  tolerance = 0.2)
  # Negative means
  expect_equivalent(df_101n$means,  c(-1, -1), tolerance = 0.2)
  expect_equivalent(df_011nb$means, c(-1),     tolerance = 0.2)
  # expect_equivalent(df_011nt$means, c(-1),     tolerance = 0.2)
  expect_equivalent(df_111nb$means, c(-1, -1), tolerance = 0.2)
  # expect_equivalent(df_111nt$means, c(-1, -1), tolerance = 0.2)
  # Mixed means
  expect_equivalent(df_101m$means,  c(10, -20), tolerance = 0.2)
  expect_equivalent(df_111mb$means, c(20, -30),  tolerance = 0.2)
  # expect_equivalent(df_111mt$means, c(30, -40),  tolerance = 0.2)
})

# Checking betas --------------------------------------------------------------
test_that("Estimated true betas are equivalent to MC estimates", {
  # Zero means
  expect_gte(min(df_100$avg_coverage),  .5)
  expect_gte(min(df_010b$avg_coverage), .5)
  expect_gte(min(df_010t$avg_coverage), .5)
  expect_gte(min(df_110b$avg_coverage), .5)
  expect_gte(min(df_110t$avg_coverage), .5)
  # Positive means
  expect_gte(min(df_101$avg_coverage),  .5)
  expect_gte(min(df_011b$avg_coverage), .5)
  expect_gte(min(df_011t$avg_coverage), .5)
  expect_gte(min(df_111b$avg_coverage), .5)
  expect_gte(min(df_111t$avg_coverage), .5)
  # Negative means
  expect_gte(min(df_101n$avg_coverage),  .5)
  expect_gte(min(df_011nb$avg_coverage), .5)
  expect_gte(min(df_011nt$avg_coverage), .5)
  expect_gte(min(df_111nb$avg_coverage), .5)
  expect_gte(min(df_111nt$avg_coverage), .5)
  # Mixed means
  expect_gte(min(df_101m$avg_coverage),  .5)
  expect_gte(min(df_111mb$avg_coverage), .5)
  expect_gte(min(df_111mt$avg_coverage), .5)
})