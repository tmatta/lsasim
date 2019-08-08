context("beta_gen() behavior with n_W > 0 and c_sd != 1")

# Generating data -------------------------------------------------------------
qGenWrap <- function(nX, nW, sd = NULL, n = 400, verbose = FALSE) {
  if (verbose) cat("n_X = ", nX, ", n_W = ", unlist(nW), ", sd = ",
                   ifelse(is.null(sd), 0, sd), "\n")
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
  
  return(list(data = data, sds = sds, avg_coverage = avg_coverage))
}

# The numbering logic below is df_abc, where the "abc" contains numbers for n_X, n_W and sd

df_229m <- qGenWrap(2, list(2, 3), 7:9)

# Checking means -------------------------------------------------------------
test_that("Observed means equal expectations", {
  expect_equivalent(df_229m$sds, 7:9, tolerance = 0.2)
})

# Checking betas --------------------------------------------------------------
test_that("Estimated true betas are equivalent to MC estimates", {
  expect_gte(min(df_229m$avg_coverage), .5)
})