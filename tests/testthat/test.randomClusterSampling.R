context("Random cluster sampling")

# ==============================================================================
# Setting up population
# ==============================================================================
N1 <- list(school = 2, class = c(3, 2), student = c(5, 4, 5, 5, 5))
N2 <- list(
	school = 5,
	class  = c(20, 8, 5, 7, 3),
	student = c(rep(20, 20), rep(30, 8), rep(10, 5), rep(5, 7), rep(9, 3))
)

# TODO: create test for this (issue # 27)
set.seed(1)
df1 <- cluster_gen(
	n = select(sch = 1, cl = 2, st = 4),
	N = N1,
	n_X = 1,
	n_W = 1,
	verbose = FALSE
)
set.seed(4)
df2 <- cluster_gen(
	n = select(sch = 3, cls = 1, stu = 10),
	N = N2,
	n_X = 1,
	n_W = 1,
	verbose = FALSE
)
df3 <- cluster_gen(n = select(4, 2, 1), N = c(10, 5, 3), verbose = FALSE)
df4 <- cluster_gen(n = select(4, 2), N = c(10, 5), verbose = FALSE)

# ==============================================================================
# Adding tests
# ==============================================================================
test_that("N can't be a multiplier if n is select", {
	expect_error(cluster_gen(n = select(4, 1), N = 2))
})

#TODO: add function to clean up NA data (#39), then add df1--df4 as tests (#27)