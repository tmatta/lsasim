# library(devtools); library(lsasim); library(testthat) #TEMP

# ======================================================== #
# General prep                                             #
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
context("Hugo's 2.0.2 report: GitHub issue #13")

data1 <- cluster_gen_2(n3, n_X = c(1, 2), c_mean = list(10, c(-100, 1e3)))
data2 <- cluster_gen_2(n3, n_X = c(1, 2), sigma = list(.1, c(1, 2)))
dat4 <- cluster_gen_2(n4, n_X = 3, c_mean = c(0.0005, 0.0006, 0.0008))
dat5 <- cluster_gen_2(n5, n_X = c(1, 3), c_mean = list(34.55, c(0.1, 0.2, 0.3)))
dat5a <- cluster_gen_2(n5a, n_X = c(3, 3), c_mean = list(c(1, 2, 2), c(0.15, 0.25, 0.35)))
dat6 <- cluster_gen_2(n6, n_X = c(2, 2), c_mean = list(c(10, 20), c(200, 3)))
dat10 <- cluster_gen_2(n10, n_X = c(2, 3, 3), c_mean = list(c(0.1, 0.225), c(0.87, 0.005, 30), c(70, 700, 7000)))
dat11 <- cluster_gen_2(n11, n_X = c(1, 2, 2, 2), c_mean = list(0.001, c(0.15, 0.25), c(0.35, 0.45), c(0.55, 0.65)))
dat12 <- cluster_gen_2(n12, n_X = c(1, 1, 2, 2, 3), c_mean = list(0.07, 0.75, c(10.55, 25), c(44, 66), c(78, 88, 98)))
set.seed(12334)
s4 <- cluster_gen_2(n4, n_X = 4, sigma = c(0.7, 0.8, 0.9, 0.11))
set.seed(12334)
s5 <- cluster_gen_2(n5, n_X = c(1, 2), sigma = list(.07, c(14.5, 20.5)))
set.seed(12334)
s6a <- cluster_gen_2(n6a, n_X = c(3, 3), sigma = list(c(21, 37, 48), c(51, 58, 67)))
set.seed(12334)
s7 <- cluster_gen_2(n7, n_X = 2, sigma = c(72, 81))
set.seed(12334)
s8 <- cluster_gen_2(n8, n_X = 4, sigma = c(0.111, 0.113, 0.115, 0.117))
set.seed(12334)
s9 <- cluster_gen_2(n9, n_X = c(2, 2), sigma = list(c(99, 101), c(0.006, 0.008)))

test_that("Stress-testing summarize_cluster()", {
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
		invisible(capture.output(
			summarize_clusters(dat4, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(dat5, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(dat5a, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(dat11, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(dat12, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(s4, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(s5, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(s6a, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(s7, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(s8, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summarize_clusters(s9, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(data1, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(data2, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(dat4, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(dat5, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(dat5a, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(dat6, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(dat10, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(dat11, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(s4, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(s5, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(s6a, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(s7)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(s8)))
	)
	expect_message(
		invisible(capture.output(summarize_clusters(s9)))
	)
	expect_output(str(summarize_clusters(data1, print="none")), "List of 2")
	expect_output(str(summarize_clusters(data2, print="none")), "List of 2")
	expect_output(str(summarize_clusters(dat4, print="none")), "List of 2")
	expect_output(str(summarize_clusters(dat5, print="none")), "List of 2")
	expect_output(str(summarize_clusters(dat5a, print="none")), "List of 2")
	expect_output(str(summarize_clusters(dat6, print="none")), "List of 2")
	expect_output(str(summarize_clusters(dat10, print="none")), "List of 2")
	expect_output(str(summarize_clusters(dat11, print="none")), "List of 2")
	expect_output(str(summarize_clusters(dat12, print="none")), "List of 2")
	expect_output(str(summarize_clusters(s4, print="none")), "List of 2")
	expect_output(str(summarize_clusters(s5, print="none")), "List of 2")
	expect_output(str(summarize_clusters(s6a, print="none")), "List of 2")
	expect_output(str(summarize_clusters(s7, print="none")), "List of 2")
	expect_output(str(summarize_clusters(s8, print="none")), "List of 2")
	expect_output(str(summarize_clusters(s9, print="none")), "List of 2")
})

# ======================================================== #
# GitHub Issue 14                                          #
# ======================================================== #
context("Hugo's 2.0.2 report: GitHub issue #14")

set.seed(12334)
m2 <- matrix(c(
	1, 0.05, 0.8,
	0.05, 1, 0.77,
	0.8, 0.77, 1
), 3, 3)
m3 <- matrix(c(
	1,0.5,0.8,
	0.5,1,0.77,
	0.8,0.77,1
),3,3)
set.seed(12334)
c3 <- cluster_gen_2(n5, cor_matrix = m3)
c3a <- cluster_gen_2(n4, cor_matrix = m3)
m4 <- matrix(c(
	1,0.12,0.1,
	0.12,1,0.11,
	0.1,0.11,1
),3,3)
set.seed(12334)
c4 <- cluster_gen_2(n4, cor_matrix = m4)
set.seed(12334)
c4_1 <- cluster_gen_2(n4, n_W=1, cor_matrix = m4)
m5 <- matrix(c(
	1, 0.55, 0.75, 0.3,
	0.55, 1, 0.15, 0.9,
	0.75, 0.15, 1, 0.25,
	0.3, 0.9, 0.25, 1
), 4, 4)

test_that("Working with matrices", {
	expect_error(cluster_gen_2(n5, cor_matrix = m2))
	expect_error(cluster_gen_2(n5, n_X=3, cor_matrix = m2))
	expect_error(cluster_gen_2(n5, n_X=3, n_W=0, cor_matrix = m2))
	expect_error(cluster_gen_2(n5, n_X=2, n_W=1, cor_matrix = m2))
	expect_output(str(c3$school), "List of 3")
	expect_output(str(c3$class), "List of 6")
	expect_output(str(c3a), "List of 20")
	expect_output(str(c4), "List of 20")
	expect_warning(capture.output(invisible(summarize_clusters(c4))))
	expect_output(str(c4_1), "List of 20")
	expect_output(str(summarize_clusters(c4_1, 4, "none"))$school, "List of 7")
	expect_error(cluster_gen_2(n6a, cor_matrix = m5))
})