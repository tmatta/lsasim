context("Retrieval of original proportions and correlations")

cum_prop_2 <- list(1, c(.25, 1), c(.2, 1))
cum_prop_3 <- list(1, c(.25, .5, .75, 1), c(.2, .4, .6, .8, 1))
yw_cor <- matrix(c(1, .5, .5, .5, 1, .8, .5, .8, 1), nrow = 3)
bg2 <- questionnaire_gen(n_obs = 1e4, cat_prop = cum_prop_2, theta = TRUE,
                         family = "gaussian", cor_matrix = yw_cor)
obsCor_bg2 <- polycor::hetcor(bg2[, -1])$correlations

test_that("Correlations are preserved", {
  expect_equivalent(obsCor_bg2, yw_cor, tolerance = 0.05)
})

test_that("Observed proportions are similar to those from input", {
  q_vars <- names(bg2)[-c(1, 2)]
  names(cum_prop_2) <- c("theta", q_vars)
  for (q in q_vars) {
    obs_prop <- cumsum(prop.table(table(bg2[q])))
    expect_equivalent(cum_prop_2[[q]], obs_prop, tolerance = 0.01)
  }
})
