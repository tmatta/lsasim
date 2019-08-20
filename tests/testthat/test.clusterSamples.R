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
  df03 <- wrap_cluster_gen(2:3, cluster_labels = LETTERS[1:2])
  df04 <- wrap_cluster_gen(2:3, resp_labels = LETTERS[1:2])
  df05 <- wrap_cluster_gen(2:4, cluster_labels = LETTERS[1:3],
                      resp_labels = LETTERS[3:5])
  df06 <- wrap_cluster_gen(2:3, n_X = 1)
  df07 <- wrap_cluster_gen(2:3, n_X = 1, n_W = 1)
  df08 <- wrap_cluster_gen(2:3, n_X = 2:3, n_W = 3:4)
  df09 <- wrap_cluster_gen(2:3, n_X = 0, n_W = list(5, 2))

  expect_output(str(df01), "List of 1")
  expect_output(str(df02), "List of 2")
  expect_equal(names(df03), "B")
  expect_equal(df04$class[[1]]$uniqueID[1], "A1_school1")
  expect_equal(df05$C[[3]]$uniqueID[4], "D4_B3_A1")  
  expect_equal(as.vector(sapply(df06$class, function(c) sapply(c[1:3], class))),
               rep(c("integer", "numeric", "factor"), 2))
  expect_equal(as.vector(sapply(df07$class, function(c) sapply(c, class))),
               rep(c("integer", "numeric", "factor", "character"), 2))
  expect_equal(as.vector(sapply(df08$class, function(c) sapply(c, class))),
               rep(c("integer", "numeric", "numeric",
                     "factor", "factor", "factor", "character"), 2))
  expect_output(str(df09$class[[1]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$class[[1]]$q2), "Factor w/ 2 levels")
  expect_output(str(df09$class[[2]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$class[[2]]$q2), "Factor w/ 2 levels")
})


# Sandbox (temporary)
# cluster_gen(n_obs = c(2, 1, 3),
#             n_X = 1,
#             n_W = 1,
#             # c_mean = list(10, 100, 1000),  # FIXME: vector c_mean not working
#             separate_questionnaires = FALSE,
#             collapse = "none")
