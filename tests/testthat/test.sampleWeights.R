context("Sampling weights")
wrap_cluster_gen <- function(n, N, meth = "SRS", sum_pop = sapply(N, sum),
                             sep = FALSE, verbose = FALSE, ...) {
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

# FIXME: cluster_gen(c(school = 2, class = 3, stu = 5)) "not 'all class', 'each'"
# FIXME: cluster_gen(c(school = 2, class = 3, stu = 5)) weights OK? If N is set, yes.

# Custom weight tests ----------------------------------------------------------
test_that("Weights are correct", {
  ex1 <- wrap_cluster_gen(c(1, 2, 3), c(10, 100, 600))
  ex2 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 4)),
                          meth = "PPS")
  ex3 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 4)),
                          meth = "SRS")
  expect_equivalent(calcWeights(ex1), c(50 * 3 * 2, 10000 * 3 * 2))
  expect_equivalent(calcWeights(ex2)["final.student.weight"], 400)
  expect_equivalent(calcWeights(ex3)["school.weight"], 2.5 * (10 + 5 + 2 + 3))
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
                           sum_pop = c(10, 400), "PPS")

test_that("Weights from PISA examples are correct", {
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

# Exploring different sampling methods -----------------------------------------
calcWeights2 <- function(data_list) {
  p_1_i <- sapply(data_list, function(x) sapply(x, function(y) x[[y]][1, 4]))
  
  w <- sapply(data_list, function(x) colSums(x[4:6]))
  w_sum <- rowSums(w)

  w_1_i <- p_1_i
  n_i_w_ij <- w_sum[3]
  out <- c(w_1_i, n_i_w_ij)
  return(out)
}

n1 <- list(cnt = 1, sch = 3, cls = c(2, 1, 3), stu = rep(2, 6))
N1 <- list(cnt = 1, sch = 9, cls = c(8, 7, 6), stu = rep(8, 6))
ex4 <- wrap_cluster_gen(n1, N1, meth = "SRS", sep = TRUE, collapse = "partial")
ex5 <- wrap_cluster_gen(n1, N1, meth = "PPS", sep = TRUE, collapse = "partial")
ex6 <- wrap_cluster_gen(n1, N1, meth = "mixed", sep = TRUE, collapse = "partial")
ex7 <- wrap_cluster_gen(n1, N1, meth = c("PPS", "SRS", "PPS"), sep = TRUE, collapse = "partial")
ex8 <- wrap_cluster_gen(n = list(cnt =  2,
                                 sch = c(1, 1),
                                 cls = c(5, 7),
                                 stu = rep(10, 12)),
                        N = list(cnt = 10,
                                 sch = c(50, 2),
                                 cls = c(10, 7),
                                 stu = c(rep(50, 5), rep(10, 7))),
                        sep = TRUE, collapse = "partial",
                        meth = c("SRS", "PPS", "SRS"))
# FIXME: check sch.weight. Should add to pop size on each upper level separately
# final.stu.weight should be 500 for cnt1.
test_that("Weights are correct for different sampling methods", {
  w1 <- c(1 * 3 + 3 * (2 + 1 + 3) + 4 * 4 + 7 * 2 + 2 * 6)
  w2 <- c(3 * 3 + 12 * 2 + 21 + 6 * 3 + 16 * 4 + 28 * 2 + 8 * 6)
  expect_equivalent(calcWeights(ex4), c(w1, w2))
  w3 <- c(3 * 3 + 3.5 * 2 + 7 + 7/3 * 3 + 12 * 4 + 24 * 2 + 8 * 6)
  expect_equivalent(calcWeights(ex5)[2], w3)
  expect_equivalent(calcWeights(ex6)[2], w2)
  w4 <- c(3 * 3 + 12 * 2 + 21 + 6 * 3 + 12 * 4 + 24 * 2 + 8 * 6)
  expect_equivalent(calcWeights(ex7)[2], w4)
  w5 <- c(250 + 10 + 3.4 * 5 + 17/7 * 7 + 10 * 50 + 2 * 70)
  expect_equivalent(calcWeights(ex8)[2], w5)
})