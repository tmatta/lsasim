context("Issue 51")

test_that("cat_prop and rho can be specified simultaneously", {
  X <- rpois(1L, lambda = 5)
  W <- rpois(1L, lambda = 5)
  props <- c(as.list(rep(1, X)), rep(list(c(0.5, 1)), W))
  expect_output(cluster_gen(c(5, 2), cat_prop = props, rho = runif(1, -1, 1)))
  expect_warning(cluster_gen(c(5, 2), n_X = X, n_W = W, cat_prop = props))
})
