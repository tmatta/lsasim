context("Examples from lsasim 1.0.1 still work")

test_that("Example from questionnaire_gen() still works", {
  cum_prop <- list(1, c(.25, .6, 1))
  bg <- questionnaire_gen(n_obs = 10, cat_prop = cum_prop,
                          cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                          c_mean = 2, c_sd = 1.5, theta = TRUE)
  expect_output(str(bg), "10 obs")
})
