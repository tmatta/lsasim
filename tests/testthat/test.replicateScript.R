context("Code replication")

test_that("Function ouput identical to script", {
  vcov_yxw <- readRDS("vcov_yxw.rds")
  vcov_yxz <- readRDS("vcov_yxz.rds")
  vcov_xfz <- readRDS("vcov_xfz.rds")
  obj <- questionnaire_gen_2()
  expect_identical(obj$vcov_yxw, vcov_yxw)
  expect_identical(obj$vcov_yxz, vcov_yxz)
  expect_identical(obj$vcov_xfz, vcov_xfz)
})
