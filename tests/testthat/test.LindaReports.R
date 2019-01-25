# lsasim 1.0.1.9007 -------------------------------------------------------

context("Linda's report on lsasim 1.0.1.9007")

test_that("Number of variables generated is consistent", {
  expect_error(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.25, .6, 1)),
                                 cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                 n_vars = 3,
                                 c_mean = 2, c_sd = 1.5, theta = TRUE))
  expect_warning(questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.25, .6, 1)),
                                   cor_matrix = matrix(c(1, .6, .6, 1), 2),
                                   n_W = 2, #categorical
                                   c_mean = 2, c_sd = 1.5, theta = TRUE))
  expect_error(questionnaire_gen(n_obs = 5,
                                 cat_prop = list(1, c(.25, .6, 1)),
                                 cor_matrix = matrix(c(1, .6, .6, 1), 2),
                                 n_W = 2, #categorical
                                 n_X = 2, #continuous
                                 c_mean = 2, c_sd = 1.5,
                                 theta = TRUE))
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

# lsasim 1.0.1.9008 -------------------------------------------------------

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

# lsasim 1.0.1.9009 -------------------------------------------------------

context("Linda's report on lsasim 1.0.1.9009")

test_that("Case 1: consistent changing cor_matrix, n_vars and cat_prop", {
  data1 <- questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.2, 1), c(.5, 1)),
                             cor_matrix = matrix(c(1, .5, .5,
                                                   .5, 1, .5,
                                                   .5, .5, 1), nrow = 3),
                             n_vars = 3, theta = T)
  data2 <- questionnaire_gen(n_obs = 5, cat_prop = list(1, c(.2, 1), c(.5, 1)),
                             n_vars = 3, theta = T)
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
  data2 <- questionnaire_gen(n_obs = 5, n_vars = 4, n_X = 1, n_W = list(2, 2), theta = T)
  expect_output(str(data1), "5 obs")
  expect_output(str(data2), "5 obs")
})

test_that("Case 4: playing around more with n_X, n_W and theta", {
  expect_error(questionnaire_gen(n_obs = 5, n_X = 1, n_W = list(2, 3, 2),
                                 c_mean = c(1, 2), theta = F))
  data1 <- questionnaire_gen(n_obs = 5, n_X = 1, n_W = list(2, 3, 2),
                             c_mean = c(1, 2), theta = T)
  data2 <- questionnaire_gen(n_obs = 5, n_X = 3, n_W = list(2, 3, 2),
                             c_mean = 500, theta = F)
  expect_output(str(data1), "5 obs")
  expect_output(str(data2), "5 obs")
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 3, n_W = list(2, 3, 2),
                                   c_mean = c(100, 500), theta = F))
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 3, n_W = list(2, 3, 2),
                                   c_mean = c(100, 500), c_sd = 0, theta = F))
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 3, n_W = list(2, 3, 2),
                                   c_mean = c(100, 500), c_sd = 0, theta = T))
  expect_warning(questionnaire_gen(n_obs = 5, n_X = 2, n_W = list(2, 3, 2),
                                   c_mean = c(500, 100), theta = T))
})

test_that("Case 5: error if c_sd is negative", {
  expect_error(questionnaire_gen(n_obs = 5, n_X = 1, n_W = list(2, 3, 2),
                                 c_mean = 100, c_sd = -1, theta = F))
})

test_that("Case 6: if n_X = NULL, a random number of n_X is generated", {
  set.seed(1234)
  data1 <- questionnaire_gen(n_obs = 5, n_W = list(2, 3, 2), theta = F)
  set.seed(5678)
  data2 <- questionnaire_gen(n_obs = 5, n_W = list(2, 3, 2), theta = F)
  set.seed(91011)
  data3 <- questionnaire_gen(n_obs = 5, n_W = list(2, 3, 2), theta = T)
  expect_output(str(data1), "4 variables")
  expect_output(str(data2), "4 variables")
  expect_output(str(data3), "7 variables")
})

test_that("Case 7: generating 1 binary and 1 3-cat W", {
  data <- questionnaire_gen(n_obs = 5, n_X = 0, n_W = list(2, 3),
                            cor_matrix = matrix(c(1, .6,
                                                  .6, 1), nrow = 2, byrow = T),
                             theta = F)
  expect_output(str(data), "5 obs")
})

test_that("Case 8: generating only one binary n_W", {
  set.seed(87326)
  data <- questionnaire_gen(n_obs = 100, n_X = 0, n_W = list(2), theta = F)
  expect_identical(names(data), c("subject", "q1"))
  expect_output(str(data$q1), "Factor w/ 2 levels")
})

# lsasim 1.0.1.9039 -------------------------------------------------------

context("Linda's report on lsasim 1.0.1.9039")

sd1 <- c(.5, 1.5)
set.seed(1234)
data1 <- questionnaire_gen(1000, family = "gaussian",
                          cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                          c_sd = sd1, theta = TRUE,
                          full_output = TRUE, n_X = 1, n_W = 0)
sd2 <- c(4, 9)
set.seed(1234)
data2 <- questionnaire_gen(1000,
                           cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                           c_sd = sd2, theta = TRUE,
                           full_output = TRUE, n_X = 1, n_W = 0)

test_that("Issue 1: SDs and variances are treated correctly", {
  expect_identical(data1$sd_YXW, sd1)
  expect_identical(data2$sd_YXW, sd2)
})

set.seed(1234)
data3 <- questionnaire_gen(1000, family = "gaussian",
                           cov_matrix = matrix(c(9, .6, .6, 1), nrow = 2),
                           theta = TRUE,
                           full_output = TRUE, n_X = 1, n_W = 0)

test_that("Issue 2: sd_YXW and var_YX are provided if cov_matrix is given", {
  expect_false(is.null(data3$sd_YXW))
  expect_false(is.null(data3$var_YX))
})

test_that("Issue 3: c_sd is not ignored if cov_matrix is provided", {
  expect_warning(questionnaire_gen(1000, family = "gaussian",
                                   cov_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                   c_sd = c(9, 1),
                                   theta = TRUE,
                                   full_output = TRUE, n_X = 1, n_W = 0))
  expect_warning(questionnaire_gen(1000, family = "gaussian",
                                   cov_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
                                   c_sd = c(-9, 1),
                                   theta = TRUE,
                                   full_output = TRUE, n_X = 1, n_W = 0))
})

set.seed(1234)
data4 <- questionnaire_gen(1000, family = "gaussian", theta = TRUE,
                           full_output = TRUE, n_X = 2, n_W = list(2, 3))
beta4  <- beta_gen(data4, MC = TRUE)
beta4q <- beta_gen(data4, MC = TRUE, rename_to_q = TRUE)
test_that("Issue 4: variable renaming is consistent", {
  expect_identical(names(data4$linear_regression$betas), rownames(beta4q))
})

set.seed(1234)
data5 <- questionnaire_gen(1000, family = "gaussian", theta = TRUE,
                          full_output = TRUE, n_X = 2, n_W = list(2, 3))

test_that("Issue 5: replications has no effect in result if MC = FALSE", {
  expect_identical(beta_gen(data5, MC = FALSE),
                   beta_gen(data5, MC = FALSE, MC_replications = 1000))
})

set.seed(1234)
data6 <- questionnaire_gen(100, theta = TRUE, n_X = 2, n_W = list(2, 3, 8))
test_that("Issue 6: factors always start at 1", {
  expect_equal(names(table(data6$q3))[1], "1")
  expect_equal(names(table(data6$q4))[1], "1")
  expect_equal(names(table(data6$q5))[1], "1")
})
