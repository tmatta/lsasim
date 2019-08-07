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

df_110b <- qGenWrap(1, list(2))
df_229m <- qGenWrap(2, list(2, 3), 9)
df_229nm <- qGenWrap(2, list(2, 3), -9)
df_111mb <- qGenWrap(1, list(2), c(20, -30))

# Checking means -------------------------------------------------------------
test_that("Observed means equal expectations", {
  expect_equivalent(df_110b$means, c(0, 0), tolerance = 0.2)
  expect_equivalent(df_229m$means, rep(9, 3),  tolerance = 0.2)
  expect_equivalent(df_229nm$means, rep(-9, 3), tolerance = 0.2)
  expect_equivalent(df_111mb$means, c(20, -30),  tolerance = 0.2)
})

# Checking betas --------------------------------------------------------------
test_that("Estimated true betas are equivalent to MC estimates", {
  expect_gte(min(df_110b$avg_coverage), .5)
  expect_gte(min(df_229m$avg_coverage), .5)
  expect_gte(min(df_229nm$avg_coverage), .5)
  expect_gte(min(df_111mb$avg_coverage), .5)
})