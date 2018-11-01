context("Sample means are close to the expected value: mu == 0")

wrap_q_gen <- function(nX, nW, mu = NULL) {
  questionnaire_gen(1e4, n_X = nX, n_W = nW, c_mean = mu, full_output = TRUE)
}

df3X0W <- wrap_q_gen(3, 0)
df2X1W <- wrap_q_gen(2, list(2))
df1X2W <- wrap_q_gen(1, list(2, 3))
df0X3W <- wrap_q_gen(0, list(2, 3, 4))

almostEqualMean <- function(sample) {
  data <- sample$bg[-1]
  data_cat <- sample$cat_prop[lapply(sample$cat_prop, length) > 1]
  mu_cont <- sample$c_mean
  mu_cat  <- unlist(lapply(data_cat, function(x) c(x[1], diff(x))))
  mean_cont <- mean_cat <- data_cont <- data_cat <- NULL
  if (sample$n_X > 0) {
    data_cont <- data[1:sample$n_X]
    mean_cont <- sapply(data_cont, mean)
  }
  if (sample$n_W > 0) {
    data_cat  <- data[(sample$n_X + 1):(sample$n_X + sample$n_W)]
    mean_cat  <- unlist(lapply(data_cat, function(x) prop.table(table(x))))
  }
  expect_equal(c(mean_cont, mean_cat), c(mu_cont, mu_cat),
               tolerance = 0.5, check.attributes = FALSE)
}
test_that("n_X = 3, n_W = 0 works", almostEqualMean(df3X0W))
test_that("n_X = 2, n_W = 1 works", almostEqualMean(df2X1W))
test_that("n_X = 1, n_W = 2 works", almostEqualMean(df1X2W))
test_that("n_X = 0, n_W = 3 works", almostEqualMean(df0X3W))

context("Sample means are close to the expected value: mu != 0")

df3X0W <- wrap_q_gen(3, 0, 1:3)
df2X1W <- wrap_q_gen(2, 1, 1:2)
df1X2W <- wrap_q_gen(1, 1, -1)
test_that("n_X = 3, n_W = 0 works", almostEqualMean(df3X0W))
test_that("n_X = 2, n_W = 1 works", almostEqualMean(df2X1W))
test_that("n_X = 1, n_W = 2 works", almostEqualMean(df1X2W))
