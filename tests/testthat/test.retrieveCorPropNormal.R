context("Retrieval of original proportions and correlations")

# Defining auxiliary objects ----------------------------------------------
cum_prop_2 <- list(1, c(.25, 1), c(.2, 1))
cum_prop_3 <- list(1, c(.25, .5, .75, 1), c(.2, .4, .6, .8, 1))
cum_prop_4 <- list(c(.1, .3, .6, 1))

yw_cor <- matrix(c(1, .5, .5, .5, 1, .8, .5, .8, 1), nrow = 3)

# Generating datasets -----------------------------------------------------
bg2 <- questionnaire_gen(1e4, cat_prop = cum_prop_2, theta = TRUE,
                         cor_matrix = yw_cor)
bg2norm <- questionnaire_gen(1e4, cat_prop = cum_prop_2, theta = TRUE,
                         family = "gaussian", cor_matrix = yw_cor)
bg3 <- questionnaire_gen(1e4, cat_prop = cum_prop_3, theta = TRUE)
bg3norm <- questionnaire_gen(1e4, cat_prop = cum_prop_3, theta = TRUE,
                         family = "gaussian")
bg4 <- questionnaire_gen(1e4, cat_prop = cum_prop_4)
bg4norm <- questionnaire_gen(1e4, cat_prop = cum_prop_4, family = "gaussian")

obsCor_bg2 <- polycor::hetcor(bg2[, -1])$correlations
obsCor_bg2norm <- polycor::hetcor(bg2norm[, -1])$correlations

# Performing tests --------------------------------------------------------
test_that("Correlations are preserved", {
  expect_equivalent(obsCor_bg2, yw_cor, tolerance = 0.05)
  expect_equivalent(obsCor_bg2norm, yw_cor, tolerance = 0.05)
})

test_that("Observed proportions are similar to those from input", {
  q_vars <- names(bg2)[-c(1, 2)]
  names(cum_prop_2) <- names(cum_prop_3) <- c("theta", q_vars)
  for (q in q_vars) {
    obs_prop2 <- cumsum(prop.table(table(bg2[q])))
    obs_prop3 <- cumsum(prop.table(table(bg3[q])))
    expect_equivalent(cum_prop_2[[q]], obs_prop2, tolerance = 0.01)
    expect_equivalent(cum_prop_3[[q]], obs_prop3, tolerance = 0.01)
  }
})