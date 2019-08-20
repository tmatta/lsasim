# Library loading (during development)
# library(testthat)
# library(devkit)
# install("../lsasim")
# library(lsasim)

# Actual test
context("Cluster samples")
wrap_cluster_gen <- function(...) cluster_gen(..., family = "gaussian", verbose = FALSE)

test_that("Basic argument handling generates data", {
  df01 <- wrap_cluster_gen(1:2)
  df02 <- wrap_cluster_gen(2:4)
  df03 <- wrap_cluster_gen(2:4, cluster_labels = LETTERS[1:2])
  df04 <- wrap_cluster_gen(2:3, resp_labels = LETTERS[1:2])
  df05 <- wrap_cluster_gen(2:4,
                           cluster_labels = LETTERS[1:2],
                           resp_labels = LETTERS[3:5])
  df06 <- wrap_cluster_gen(2:3, n_X = 1)
  df07 <- wrap_cluster_gen(2:3, n_X = 1, n_W = 1)
  df08 <- wrap_cluster_gen(2:3, n_X = 2:3, n_W = 3:4)
  df09 <- wrap_cluster_gen(2:3, n_X = 0, n_W = list(5, 2))
  # TODO: add tests for c_mean
  # TODO: add tests for separate_questionnaires
  # TODO: add tests for collapse
  expect_output(str(df01), "List of 1")
  expect_output(str(df02), "List of 2")
  expect_equal(names(df03), LETTERS[1:2])
  expect_equal(df04$country[[1]]$uniqueID[1], "A1_country1")
  expect_equal(df05$B[[3]]$uniqueID[4], "D4_B3_A1")  
  expect_equal(as.vector(sapply(df06$country,
                                function(c) sapply(c[1:3], class))),
               rep(c("integer", "numeric", "factor"), 2))
  expect_equal(as.vector(sapply(df07$country, function(c) sapply(c, class))),
               rep(c("integer", "numeric", "factor",
                     "numeric", "numeric", "character"), 2))
  expect_equal(as.vector(sapply(df08$country, function(c) sapply(c, class))),
               rep(c("integer", "numeric", "numeric",
                     "factor", "factor", "factor",
                     "numeric", "numeric", "character"), 2))
  expect_output(str(df09$country[[1]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$country[[1]]$q2), "Factor w/ 2 levels")
  expect_output(str(df09$country[[2]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$country[[2]]$q2), "Factor w/ 2 levels")
})

# TODO: add tests for validation errors
# TODO: add tests for weights (compare with example on PISA Manual)
# TODO: rename n_obs to "n"?

# Sandbox (temporary)
# cluster_gen(n_obs = c(1, 2, 4),
#             n_X = 0,
#             n_W = 1,
#             N = c(5, 20, 100),
#             separate_questionnaires = TRUE,
#             collapse = "full")