context("c_sd is correctly parsed for Y and X")

# Testing scalar c_sd -----------------------------------------------------
sd_size_1 <- 2
dataX   <- questionnaire_gen(10, cat_prop = list(1),
                             full_output = TRUE, c_sd = sd_size_1)
dataXW  <- questionnaire_gen(10, cat_prop = list(1, c(.5, 1)),
                             full_output = TRUE, c_sd = sd_size_1)
dataYW  <- questionnaire_gen(10, cat_prop = list(1, c(.5, 1)), theta = TRUE,
                             full_output = TRUE, c_sd = sd_size_1)
dataYXW <- questionnaire_gen(10, cat_prop = list(1, 1, c(.5, 1)), theta = TRUE,
                             full_output = TRUE, c_sd = sd_size_1)

test_that("scalar c_sd are correctly passed", {
  expect_equivalent(unique(diag(dataX$cov_matrix)[1]), sd_size_1 ^ 2)
  expect_equivalent(unique(diag(dataXW$cov_matrix)[1]), sd_size_1 ^ 2)
  expect_equivalent(unique(diag(dataYW$cov_matrix)[1]), sd_size_1 ^ 2)
  expect_equivalent(unique(diag(dataYXW$cov_matrix)[1:2]), sd_size_1 ^ 2)
})

# Testing vector c_sd -----------------------------------------------------
sd_size_2 <- 2:3
dataX   <- questionnaire_gen(10, cat_prop = list(1, 1),
                             full_output = TRUE, c_sd = sd_size_2)
dataXW  <- questionnaire_gen(10, cat_prop = list(1, 1, c(.5, 1)),
                             full_output = TRUE, c_sd = sd_size_2)
dataYXW <- questionnaire_gen(10, cat_prop = list(1, 1, c(.5, 1)), theta = TRUE,
                             full_output = TRUE, c_sd = sd_size_2)

test_that("vector c_sd are correctly passed", {
  expect_equivalent(diag(dataX$cov_matrix)[1:2], sd_size_2 ^ 2)
  expect_equivalent(diag(dataXW$cov_matrix)[1:2], sd_size_2 ^ 2)
  expect_equivalent(diag(dataYXW$cov_matrix)[1:2], sd_size_2 ^ 2)
})
