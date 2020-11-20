library(devtools); library(lsasim); library(testthat) #TEMP
context("Hugo's 2.0.2 report")

# ======================================================== #
# Prep                                                     #
# ======================================================== #
set.seed(12334)

# Cluster elements --------------------------------------- #
n1 <- c(3, 6)
n2 <- c(groups = 4, people = 2)
n3 <- c(school = 3, class = 2, student = 5)
n4 <- c(20, 50)
n5 <- list(school = 3, class = c(2, 1, 3), student = c(20, 20, 10, 30, 30, 30))
n5a <- list(school = 3, class = c(2, 3, 3), student = c(20, 20, 10, 30, 30, 30))
n6 <- list(school = 3, class = c(2, 1, 3), student = ranges(10, 50))
n6a <- list(school = 3, class = c(2, 3, 3), student = ranges(10, 50))
n7 <- list(school = 10, student = ranges(10, 50))
n8 <- list(school = 3, student = c(20, 20, 10))
n8a <- list(school = 3, class = c(2, 2, 2),student = c(20, 20, 10))
n8b <- list(school = 3, class = c(2, 3, 3),student = c(20, 20, 10, 5))
n8c <- list(school = 3, class = c(2, 1, 3),student = c(20, 20, 10))
n9 <- list(
	school = 10, class = c(2,1,3,1,1,1,2,1,2,1), student = ranges(10, 50)
)
n10 <- list(
	country = 2, school = 10, class = c(2,1,3,1,1,1,2,1,2,1),
	student = ranges(10, 50)
)
n11 <- list(
	culture = 2, country = 2, school = 10, class = c(2,1,3,1,1,1,2,1,2,1),
	student = ranges(10, 50)
)
n12 <- list(
	culture = 2, country = 2, district = 3, school = 10,
	class = c(2,1,3,1,1,1,2,1,2,1), student = ranges(10, 50)
)
N1 <- c(100, 20)

# Wrapper function --------------------------------------- #
cluster_gen_2 <- function(...) {
	cluster_gen(..., verbose = FALSE, calc_weights = FALSE)
}

# ======================================================== #
# GitHub Issue 13                                          #
# ======================================================== #

data1 <- cluster_gen_2(n3, n_X = c(1, 2), c_mean = list(10, c(-100, 1e3)))
data2 <- cluster_gen_2(n3, n_X = c(1, 2), sigma = list(.1, c(1, 2)))

test_that("summarize_cluster() works", {
	expect_message(
		invisible(capture.output(
			summarize_clusters(data1, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(data2, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(data1, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(data2, print_hetcor=FALSE)))
	)
	expect_output(str(summarize_clusters(data1, print="none")), "List of 2")
	expect_output(str(summarize_clusters(data2, print="none")), "List of 2")
})