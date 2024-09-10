context("Issue 51")

test_that("cat_prop and rho can be specified simultaneously", {
  X <- rpois(1L, lambda = 5)
  W <- rpois(1L, lambda = 5)
  props <- c(as.list(rep(1, X)), rep(list(c(0.5, 1)), W))
  r <- runif(1, -0.1, 1)
  enn <- c(sample(5:20, 1L), sample(2:5, 1L))
  dt_separated_q <- cluster_gen(enn, cat_prop = props, rho = r, verbose = FALSE)
  dt_joint_q <- cluster_gen(
    enn, cat_prop = props, rho = r, separate_questionnaires = FALSE,
    verbose = FALSE
  )
  expect_s3_class(dt_separated_q, "lsasimcluster")
  expect_s3_class(dt_joint_q, "lsasimcluster")
  expect_warning(
    cluster_gen(enn, n_X = X, n_W = W, cat_prop = props, verbose = FALSE)
  )
})
