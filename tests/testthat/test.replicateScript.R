context("Replication of results from script 1")

test_that("Function ouput identical to script", {
  vcov_yxw <- readRDS("vcov_yxw.rds")
  vcov_yxz <- readRDS("vcov_yxz.rds")
  vcov_yfz <- readRDS("vcov_xfz.rds")
  obj <- cov_gen(seed = 674634)
  expect_identical(obj$vcov_yxw, vcov_yxw)
  expect_identical(obj$vcov_yxz, vcov_yxz)
  expect_identical(obj$vcov_yfz, vcov_yfz)
})
