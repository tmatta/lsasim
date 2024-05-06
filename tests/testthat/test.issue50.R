# Adapted from https://github.com/tmatta/lsasim/issues/50#issue-2254039296

# Data and subset
set.seed(8078908)
param <- NAEPirtparams::parameters
item_par <- param[param$level %in% 8 & param$subject %in% "Mathematics" & param$year %in% 2015, ]

# Happy path
item_par_1 <- item_par
item_par_1$item <- sample(1e5, nrow(item_par_1))
block_1 <- block_design(10L, item_par_1)

# Sequential items
item_par_2 <- item_par
item_par_2$item <- seq(1, nrow(item_par_2))
block_2 <- block_design(10L, item_par_2)

# Tests
test_that("Results are independent of item numbering", {
  expect_identical(block_1$block_descriptives, block_2$block_descriptives)
})
test_that("Errors are properly triggered", {
  expect_error(block_design(10L, item_par), "must contain an 'item' column.")
  item_par$item <- sample(100, size = nrow(item_par), replace = TRUE)
  expect_error(block_design(10L, item_par), "must contain unique 'item'.")
})
