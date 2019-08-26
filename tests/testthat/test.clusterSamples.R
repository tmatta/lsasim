context("Cluster samples")
wrap_cluster_gen <- function(...) {
  cluster_gen(..., family = "gaussian", verbose = FALSE)
}

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
  df10 <- wrap_cluster_gen(rep(10, 3),
                           c_mean = list(c(1, 10), c(1e2, 1e3)), n_X = 2)
  df11 <- wrap_cluster_gen(2:4, separate_questionnaires = FALSE,
                           c_mean = c(1, 10), n_X = 2)
  df12 <- wrap_cluster_gen(2:4, collapse = "partial")
  df13 <- wrap_cluster_gen(2:4, collapse = "full")
  df14 <- wrap_cluster_gen(2:4, collapse = "full",
                           separate_questionnaires = FALSE)

  expect_output(str(df01), "List of 1")
  expect_output(str(df02), "List of 2")
  expect_equal(names(df03), LETTERS[1:2])
  expect_equal(df04$country[[1]]$uniqueID[1], "A1_country1")
  expect_equal(df05$B[[3]]$uniqueID[4], "D4_B3_A1")  
  expect_equal(as.vector(sapply(df06$country,
                                function(c) sapply(c[1:3], class))),
               rep(c("integer", "numeric", "factor"), 2))
  # expect_equal(as.vector(sapply(df07$country, function(c) sapply(c, class))),
  #              rep(c("integer", "numeric", "factor",
  #                    "numeric", "character"), 2))
  # expect_equal(as.vector(sapply(df08$country, function(c) sapply(c, class))),
  #              rep(c("integer", "numeric", "numeric",
  #                    "factor", "factor", "factor",
  #                    "numeric", "character"), 2))
  # TODO: uncomment after weights have been reimplemented
  expect_output(str(df09$country[[1]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$country[[1]]$q2), "Factor w/ 2 levels")
  expect_output(str(df09$country[[2]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$country[[2]]$q2), "Factor w/ 2 levels")
  expect_equivalent(sapply(df10, function(x) apply(x[[1]][2:3], 2, mean))[, 1],
                    c(1, 10), tolerance = .1)
  expect_equivalent(sapply(df10, function(x) apply(x[[2]][2:3], 2, mean))[, 1],
                    c(1, 10), tolerance = .1)
  expect_equivalent(sapply(df10, function(x) apply(x[[1]][2:3], 2, mean))[, 2],
                    c(1e2, 1e3), tolerance = .1)
  expect_equivalent(sapply(df10, function(x) apply(x[[2]][2:3], 2, mean))[, 2],
                    c(1e2, 1e3), tolerance = .1)
  expect_output(str(df11), "List of 6")
  expect_output(str(df12), "List of 2")
  expect_output(str(df13), "24 obs.")
  expect_output(str(df14), "24 obs.")
})

test_that("Errors are caught", {
  expect_error(cluster_gen(1))
  expect_error(cluster_gen(2:4, separate_questionnaires = FALSE, n_X = 1:2))
  expect_error(cluster_gen(2:4, separate_questionnaires = FALSE, n_W = 1:2))
  expect_error(cluster_gen(2:4, separate_quest = FALSE, c_mean = list(1, 2)))
  expect_error(cluster_gen(2:4, cluster_labels = "a"))
  expect_error(cluster_gen(2:4, separate_quest = FALSE, collapse = "partial"))
})

# TODO: add tests for weights (compare with example on PISA Manual)
# cluster_gen(c(1, 2, 3), N = c(1, 100, 1000))
# TODO: make it clear that N is the population across all clusters

#  cluster_gen(c(2, 3, 4), n_X = 1, n_W = 1, c_mean = 10, cluster_labels=c("school", "class"), resp_labels=c("teacher", "student"), separate_questionnaires=FALSE)
# cluster_gen(c(1, 2, 3), N = c(10, 100, 1000), collapse = "full")


# cluster_gen(1:4, separate_questionnaires = FALSE)
# cluster_gen(list(1, 2, c(1, 2), c(3, 2, 3)), separate_questionnaires = FALSE)
# cluster_gen(list(2, c(3, 2), c(1, 1, 2, 3, 2), rep(3, 9)),  n_X = 1, n_W = 1, separate_questionnaires = FALSE)