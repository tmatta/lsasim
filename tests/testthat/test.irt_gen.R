context("item_gen on polythomous cases")
# GitHub context: https://github.com/tmatta/lsasim/issues/40

n_items <- 30
set.seed(345)

# ======================================================== #
# Normal example                                           #
# ======================================================== #
subject <- 1:100
theta <- rnorm(100,0,1)
student_dt <- data.frame(subject, theta)

item <- as.integer(c(1:n_items))
a <- runif(n_items, 0.5, 1.5)
b <- runif(n_items, -3, 3)
c <- runif(n_items, 0, .15)
item_par <- data.frame(item, a, b, c)

resp_matrix <- response_gen(
  subject = sort(rep(subject, 30)), item = rep(item,100), theta = theta,
  a_par = a, b_par = b, c_par = c
)

# ======================================================== #
# Item means make sense and everything looks fine?         #
# ======================================================== #
test_that("Normal example works", {
  item_means <- colMeans(resp_matrix[item])
  intercept <- coefficients(lm(colMeans(resp_matrix[item]) ~ b))[1]
  expect_equivalent(min(item_means), 0, tol = 0.1)
  expect_equivalent(max(item_means), 1, tol = 0.1)
  expect_equivalent(intercept, 0.5, tol = 0.1)
})

# ======================================================== #
# Extreme case                                             #
# ======================================================== #

# student mean theta is -20, I don't expect them to solve anything.
# But guessing parameters are super high. So, regardles if their theta 80%-90%
# of them should solve these items.
subject <- 1:100
theta <- rnorm(100, -20 ,1)
student_dt <- data.frame(subject, theta)

item <- as.integer(c(1:n_items))
a <- runif(n_items, 0.5, 1.5)
b <- runif(n_items, -3, 3)

#chance is super high
c <- runif(n_items, 0.8, .9)
item_par <- data.frame(item, a, b, c)
resp_matrix <- response_gen(
  subject = sort(rep(subject, 30)), item = rep(item,100), theta = theta,
  a_par = a, b_par = b, c_par = c
)
test_that("Extreme example works", {
  item_means <- colMeans(resp_matrix[item])
  intercept <- coefficients(lm(colMeans(resp_matrix[item]) ~ b))[1]
  expect_equivalent(min(item_means), .8, tol = 0.1)
  expect_equivalent(max(item_means), .9, tol = 0.1)
  expect_equivalent(intercept, 0.85, tol = 0.1)
})
