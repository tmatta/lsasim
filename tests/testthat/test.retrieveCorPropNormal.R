context("Retrieval of original proportions and correlations")

cum_prop <- list(1, c(.25, 1), c(.2, .8, 1))
yw_mean <- NULL
yw_cov <- matrix(c(1, .5, .5, .5, 1, .8, .5, .8, 1), nrow = 3)
bg <- questionnaire_gen(n_obs = 1e4, cat_prop = cum_prop, family = "gaussian",
                        c_mean = yw_mean, cov_matrix = yw_cov, theta = TRUE)

test_that("Observed correlations are similar to input for family='gaussian'", {
  obs_cor <- polycor::hetcor(bg[, -1])$correlations
  expect_equivalent(obs_cor, yw_cov, tolerance = 0.05)
})

test_that("Observed proportions are similar to those from input", {
  q_vars <- names(bg)[-c(1, 2)]
  names(cum_prop) <- c("theta", q_vars)
  for (q in q_vars) {
    obs_prop <- cumsum(prop.table(table(bg[q])))
    expect_equivalent(cum_prop[[q]], obs_prop, tolerance = 0.01)
  }
})
