context("Replications of Linda's reports")

test_that("Linda's report on lsasim 1.0.1.9007", {
  expect_error(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.25, .6, 1)),
                                 cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                 n_vars = 3,
                                 c_mean = 2, c_sd = 1.5, theta = TRUE))
  expect_warning(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.25, .6, 1)),
                                   cor_matrix = matrix(c(1, .6, .6, 1), 2),
                                   n_W = 2, #categorical
                                   c_mean = 2, c_sd = 1.5, theta = TRUE))
  expect_warning(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.25, .6, 1)) ,
                                   cor_matrix = matrix(c(1, .6, .6, 1), 2),
                                   n_W = 2, #categorical
                                   n_X = 2, #continuous
                                   c_mean = 2, c_sd = 1.5, theta = TRUE))
  expect_error(questionnaire_gen(n_obs = 5,
                                 cat_prop = list(1, c(.5, 1), c(.5, 1),
                                                 c(0.25, 0.60, 1.00)),
                                 cor_matrix = matrix(c(1, .2, .2, .2,
                                                       .2, 1, .2, .2,
                                                       .2, .2, 1, .2,
                                                       .2, .2, .2, 1),
                                                     nrow = 4),
                                 n_vars = 3, theta = TRUE))
  expect_error(questionnaire_gen(n_obs = 5,
                                 cat_prop = list(1, c(.5, 1), c(.5, 1),
                                                 c(0.25, 0.60, 1.00)),
                                 cor_matrix = matrix(c(1, .2, .2, .2,
                                                       .2, 1, .2, .2,
                                                       .2, .2, 1, .2,
                                                       .2, .2, .2, 1 ),
                                                     nrow = 4),
                                 n_vars = 3, theta = FALSE))
})

test_that("Improper matrices yield errors", {
  expect_error(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.25, .6, 1)),
                                 cor_matrix = matrix(c(1.5, .6, .6, 1), 2),
                                 c_mean = 2, c_sd = 1.5, theta = TRUE))
})

test_that("Improper cumulative proportions yield errors", {
  expect_error(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.60, .20, 1)),
                                 cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                 c_mean = 2, c_sd = 1.5, theta = TRUE))
  expect_error(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.25, .6, 1.5)),
                                 cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                 c_mean = 2, c_sd = 1.5, theta = TRUE))
})

context("Linda's report on lsasim 1.0.1.9008")

test_that("Sizes of cat_prop, cor_matrix and n_vars don't conflict", {
  expect_message(questionnaire_gen(n_obs = 5, cat_prop =  list(c(1), c(.5, 1)),
                                  cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                  n_vars = 2,
                                  theta = T),
                "Generating background data from correlation matrix")
  expect_error(questionnaire_gen(n_obs = 5, cat_prop = list(c(.5, 1)) ,
                                 cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                 n_vars = 1,
                                 theta = T))
  expect_error(questionnaire_gen(n_obs = 5, cat_prop =  list(c(1), c(.5, 1)) ,
                                 cor_matrix = matrix(c(1, .5, .5,
                                                       .5, 1, .5,
                                                       .5, .5, 1), nrow = 3),
                                 n_vars = 2,
                                 theta = T))
  expect_error(questionnaire_gen(n_obs = 5, cat_prop =  list(c(1), c(.5, 1)) ,
                                 cor_matrix = matrix(c(1, .65,
                                                       .6, 1), 2, byrow = T),
                                 n_vars = 2,
                                 theta = T))
  expect_error(questionnaire_gen(n_obs = 5, cat_prop =  list(1, 1, c(.5, 1)) ,
                                 cor_matrix = matrix(c(1, .5, .5,
                                                       .45, 1, .5,
                                                       .5, .5, 1), 3, byrow = T),
                                 n_vars = 3,
                                 theta = T))
})

context("Linda's report on lsasim 1.0.1.9009")

test_that("Case 1: consistent changing cor_matrix, n_vars and cat_prop", {
  data1 <- questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.2, 1), c(.5, 1)),
                             cor_matrix = matrix(c(1, .5, .5,
                                                   .5, 1, .5,
                                                   .5, .5, 1), nrow = 3),
                             n_vars = 3,
                             theta = T)
  data2 <- questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.2, 1), c(.5, 1)),
                             n_vars = 3,
                             theta = T)
  data3 <- questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.2, 1), c(.5, 1)),
                            theta = T)
  classes <- c("theta" = "numeric", "q1" = "factor", "q2" = "factor")
  expect_identical(sapply(data1, class)[-1], classes)
  expect_identical(sapply(data2, class)[-1], classes)
  expect_identical(sapply(data3, class)[-1], classes)
})

test_that("Case 2: errors are thrown", {
  expect_error(questionnaire_gen(n_obs = 5, n_vars = 4, n_X = 1, n_W = 2))
  expect_error(questionnaire_gen(n_obs = 5, n_vars = 4, n_X = 1, n_W = 2, theta = F))
})

test_that("Case 3: data is generated", {
  data1 <- questionnaire_gen(n_obs = 5, n_vars = 4, n_X = 1, n_W = 2, theta = T)
  data2 <- questionnaire_gen(n_obs = 5, n_vars = 4, n_X = 1, n_W = c(2, 2), theta = T)
  expect_output(str(data1), "5 obs")
  expect_output(str(data2), "5 obs")
})

test_that("Case 4: playing around more with n_X, n_W and theta", {
  expect_error(questionnaire_gen(n_obs = 5, n_X = 1, n_W = c(2, 3, 2),
                                 c_mean = c(1, 2), theta = F))
  data1 <- questionnaire_gen(n_obs = 5, n_X = 1, n_W = c(2, 3, 2),
                             c_mean = c(1, 2), theta = T)
  data2 <- questionnaire_gen(n_obs = 5, n_X = 3, n_W = c(2, 3, 2),
                             c_mean = 500, theta = F)
  expect_output(str(data1), "5 obs")
  expect_output(str(data2), "5 obs")
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 3, n_W = c(2, 3, 2),
                                   c_mean = c(100, 500), theta = F))
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 3, n_W = c(2, 3, 2),
                                   c_mean = c(100, 500), c_sd = 0, theta = F))
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 3, n_W = c(2, 3, 2),
                                   c_mean = c(100, 500), c_sd = 0, theta = T))
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 2, n_W = c(2, 3, 2),
                                   c_mean = c(500, 100), theta = T))
})

test_that("Case 5: error if c_sd is negative", {
  expect_error(questionnaire_gen(n_obs = 5, n_X = 1, n_W = c(2, 3, 2),
                                 c_mean = 100, c_sd = -1, theta = F))
})

test_that("Case 6: if n_X = NULL, a random number of n_X is generated", {
  set.seed(1234)
  data1 <- questionnaire_gen(n_obs = 5, n_W = c(2, 3, 2), theta = F)
  set.seed(5678)
  data2 <- questionnaire_gen(n_obs = 5, n_W = c(2, 3, 2), theta = F)
  set.seed(91011)
  data3 <- questionnaire_gen(n_obs = 5, n_W = c(2, 3, 2), theta = T)
  expect_output(str(data1), "4 variables")
  expect_output(str(data2), "5 variables")
  expect_output(str(data3), "8 variables")
})

test_that("Case 7: generating 1 binary and 1 3-cat W", {
  data <- questionnaire_gen(n_obs = 5, n_X = 0, n_W = c(2, 3),
                            cor_matrix = matrix(c(1, .6,
                                                  .6, 1), nrow = 2, byrow = T),
                             theta = F)
  # Case 7: Got Error here. However, can I just tell the program that I want 1
  # binary and 1 three-category variables without telling cat_prop, but only
  # cor_matrix? It’s possible to happen in the real setting. (Linda)
  expect_output(str(data), "5 obs")
})

test_that("Case 8: generating only one binary n_W", {
  set.seed(87326)
  data <- questionnaire_gen(n_obs = 100, n_X = 0, n_W = 2, theta = F)
  expect_identical(names(data), c("subject", "q1"))
  expect_output(str(data$q1), "Factor w/ 2 levels")
})
