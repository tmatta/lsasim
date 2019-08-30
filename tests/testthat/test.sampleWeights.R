context("Sampling weights")
wrap_cluster_gen <- function(n, N, sum_pop, meth = "SRS",sep = FALSE,
                             verbose = FALSE, ...) {
  data <- cluster_gen(
    n                       = n,
    N                       = N,
    sum_pop                 = sum_pop,
    n_X                     = 1,
    n_W                     = 1,
    sampling_method         = meth,
    separate_questionnaires = sep,
    verbose                 = verbose,
    ...
  )
  if (verbose) print(data)
  return(data)
}

calcWeights <- function(data_list) {
  w <- sapply(data_list, function(x) colSums(x[4:6]))
  w_sum <- rowSums(w)
  w_1_i <- w_sum[1]
  n_i_w_ij <- w_sum[3]
  out <- c(w_1_i, n_i_w_ij)
  return(out)
}

# Basic weight tests -----------------------------------------------------------
ex1 <- wrap_cluster_gen(c(1, 2, 3), c(10, 100, 600))
test_that("Weights are correct", {
  expect_equivalent(calcWeights(ex1), c(50 * 3 * 2, 10000 * 3 * 2))
})

# Example from PISA manual tables ----------------------------------------------
ex_3.3 <- wrap_cluster_gen(n = c(school = 4, student = 10),
                           N = c(        10,           40))
ex_3.4 <- wrap_cluster_gen(n = list(school =  4,student = c(10, 10, 10,  10)),
                           N = list(         10,          c(15, 30, 40, 100)))
ex_3.5 <- wrap_cluster_gen(n = list(school =  4, student = c(10, 10, 10,  10)),
                           N = list(         10,           c(10, 15, 20,  25)))
ex_3.6 <- wrap_cluster_gen(n = list(school =  4, student = c(10, 10, 10,  10)),
                           N = list(         10,           c(40, 45, 80, 100)))
ex_3.7 <- wrap_cluster_gen(n = list(school =  4, student = c(10, 10, 10,  10)),
                           N = list(         10,           c(20, 40, 80, 100)),
                           sum_pop = 400, "PPS")

test_that("Weights are correct", {
  expect_equivalent(calcWeights(ex_3.3), c(2.5 * 10 * 4, 400))
  expect_equivalent(calcWeights(ex_3.4), c(2.5 * 10 * 4, 462.5))
  expect_equivalent(calcWeights(ex_3.5), c(2.5 * 10 * 4, 175))
  expect_equivalent(calcWeights(ex_3.6), c(2.5 * 10 * 4, 662.5))
  expect_equivalent(calcWeights(ex_3.7), c(9.75 * 10, 400))
})

test_that("Labels are correct", {
  weight_names <- c("school.weight", "final.student.weight")
  expect_equal(names(calcWeights(ex_3.3)), weight_names)
  expect_equal(names(calcWeights(ex_3.4)), weight_names)
  expect_equal(names(calcWeights(ex_3.5)), weight_names)
  expect_equal(names(calcWeights(ex_3.6)), weight_names)
  expect_equal(names(calcWeights(ex_3.7)), weight_names)
})