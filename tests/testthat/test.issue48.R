context("Item threshold production")

set.seed(9947061)
i1 <- item_gen(b_bounds = c(-1, 1), thresholds = c(1, 2, 4), n_1pl = c(1, 1, 1))
i2 <- item_gen(b_bounds = c(-1, 1), thresholds = 1:6, n_1pl = rep(2, 6))
i3 <- item_gen(
    b_bounds = c(-2, 2), a_bounds = c(.75, 1.25), thresholds = c(1, 2, 3),
    n_1pl = c(5, 5, 5), n_2pl = c(0, 0, 5)
  )

test_that("Threshold values are as expected", {
  expect_equal(i1$d1, c(0, -.21, -.46))
  expect_equal(i1$d2, c(0, .21, -.03))
  expect_equal(i1$d3, c(0, 0, .1))
  expect_equal(i1$d4, c(0, 0, .39))
  expect_equal(
    i2$d1,
    c(0, 0, -0.38, -0.26, -0.31, -0.4, -0.54, -0.55, -0.51, -0.43, -0.49, -0.5)
  )
  expect_equal(
    i2$d2,
    c(0, 0, 0.38, 0.26, 0.1, -0.11, -0.32, -0.08, -0.11, -0.32, -0.33, -0.33)
  )
  expect_equal(
    i2$d3,
    c(0, 0, 0, 0, 0.21, 0.51, 0.16, 0.17, 0, -0.03, -0.21, -0.09)
  )
  expect_equal(
    i2$d4,
    c(0, 0, 0, 0, 0, 0, 0.71, 0.46, 0.16, 0.17, 0.14, 0.06)
  )
  expect_equal(
    i2$d5, c(0, 0, 0, 0, 0, 0, 0, 0, 0.46, 0.61, 0.37, 0.36)
  )
  expect_equal(
    i2$d6, c(0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.52, 0.49)
  )
  expect_equal(
    i3$d1,
    c(
      0, 0, 0, 0, 0, -0.43, -0.44, -0.43, -0.6, -0.34, -0.76, -0.77, -0.7,
      -0.35, -0.83, -0.8, -0.78, -1.09, -0.61, -1.24
    )
  )
  expect_equal(
    i3$d2,
    c(
      0, 0, 0, 0, 0, 0.43, 0.44, 0.43, 0.6, 0.34, 0.06, 0.22, 0.22, -0.07, 0.34,
      -0.08, -0.12, 0.07, -0.04, 0.15
    )
  )
  expect_equal(
    i3$d3,
    c(
      0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0.7, 0.56, 0.49, 0.42, 0.49, 0.88, 0.91,
      1.02, 0.65, 1.1
    )
  )
})
