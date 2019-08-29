context("Sampling weights")
wrap_cluster_gen <- function(n, N, meth = "SRS", sep = FALSE, verbose = FALSE) {
  cluster_gen(n                       = n,
              N                       = N,
              n_X                     = 1,
              n_W                     = 1,
              sampling_method         = meth,
              separate_questionnaires = sep,
              verbose                 = verbose)
}

calcWeights <- function(data_list) {
  w <- sapply(data_list, function(x) colSums(x[4:6]))
  w_sum <- rowSums(w)
  w_1_i <- w_sum[1]
  n_i_w_ij <- w_sum[3]
  out <- c(w_1_i, n_i_w_ij)
  return(out)
}


# TODO: add tests for weights (compare with example on PISA Manual)

# Basic weight tests -----------------------------------------------------------
ex1 <- wrap_cluster_gen(c(1, 2, 3), c(10, 100, 600))
test_that("Weights are correct", {
  expect_equivalent(calcWeights(ex1), c(50 * 3 * 2, 10000 * 3 * 2))
})

# Example from PISA manual tables ----------------------------------------------
ex_3.3 <- wrap_cluster_gen(n = c(1,  4, 10),
                           N = c(1, 10, 40))
ex_3.4 <- wrap_cluster_gen(n = list(1,  4, c(10, 10, 10,  10)),
                           N = list(1, 10, c(15, 30, 40, 100)))
ex_3.5 <- wrap_cluster_gen(n = list(1,  4, c(10, 10, 10,  10)),
                           N = list(1, 10, c(10, 15, 20,  25)))
ex_3.6 <- wrap_cluster_gen(n = list(1,  4, c(10, 10, 10,  10)),
                           N = list(1, 10, c(40, 45, 80, 100)))

test_that("Weights are correct", {
  expect_equivalent(calcWeights(ex_3.3), c(2.5 * 10 * 4, 400))
  expect_equivalent(calcWeights(ex_3.4), c(2.5 * 10 * 4, 462.5))
  expect_equivalent(calcWeights(ex_3.5), c(2.5 * 10 * 4, 175))
  expect_equivalent(calcWeights(ex_3.6), c(2.5 * 10 * 4, 662.5))
})

test_that("Labels are correct", {
  weight_names <- c("school.weight", "final.student.weight")
  expect_equal(names(calcWeights(ex_3.3)), weight_names)
  expect_equal(names(calcWeights(ex_3.4)), weight_names)
  expect_equal(names(calcWeights(ex_3.5)), weight_names)
  expect_equal(names(calcWeights(ex_3.6)), weight_names)
})