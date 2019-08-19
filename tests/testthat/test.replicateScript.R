context("Replication of results from script 1")

test_that("Function ouput identical to script", {
  vcov_yxw <- readRDS("vcov_yxw.rds")
  vcov_yxz <- readRDS("vcov_yxz.rds")
  vcov_yfz <- readRDS("vcov_xfz.rds")
  set.seed(674634)
  vcov <- cov_gen(pr_grp_1 = .66, n_fac = 9, n_ind = 4, Lambda = c(.6, .9))
  expect_equivalent(vcov$vcov_yxw, vcov_yxw)
  expect_equivalent(vcov$vcov_yxz, vcov_yxz)
  expect_equivalent(vcov$vcov_yfz, vcov_yfz)
})
