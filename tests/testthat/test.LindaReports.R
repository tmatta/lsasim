context("Replications of Linda's reports")

test_that("Mismatches in number of variables yield errors or warnings", {
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

context("More verifications from Linda")

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
