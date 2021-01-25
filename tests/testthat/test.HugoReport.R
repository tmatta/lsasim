# ======================================================== #
# General prep                                             #
# ======================================================== #
context("Hugo's report on lsasim 2.0.2")
set.seed(12334)

# Cluster elements ---------------------------------------
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
			summary(data1, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(data2, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(dat4, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(dat5, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(dat5a, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(dat11, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(dat12, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(s4, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(s5, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(s6a, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(s7, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(s8, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(
			summary(s9, print="all", print_hetcor=FALSE)
		))
	)
	expect_message(
		invisible(capture.output(summary(data1, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(data2, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(dat4, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(dat5, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(dat5a, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(dat6, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(dat10, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(dat11, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(s4, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(s5, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(s6a, print_hetcor=FALSE)))
	)
	expect_message(
		invisible(capture.output(summary(s7)))
	)
	expect_message(
		invisible(capture.output(summary(s8)))
	)
	expect_message(
		invisible(capture.output(summary(s9)))
	)
	expect_output(str(summary(data1, print="none")), "List of 2")
	expect_output(str(summary(data2, print="none")), "List of 2")
	expect_output(str(summary(dat4, print="none")), "List of 2")
	expect_output(str(summary(dat5, print="none")), "List of 2")
	expect_output(str(summary(dat5a, print="none")), "List of 2")
	expect_output(str(summary(dat6, print="none")), "List of 2")
	expect_output(str(summary(dat10, print="none")), "List of 2")
	expect_output(str(summary(dat11, print="none")), "List of 2")
	expect_output(str(summary(dat12, print="none")), "List of 2")
	expect_output(str(summary(s4, print="none")), "List of 2")
	expect_output(str(summary(s5, print="none")), "List of 2")
	expect_output(str(summary(s6a, print="none")), "List of 2")
	expect_output(str(summary(s7, print="none")), "List of 2")
	expect_output(str(summary(s8, print="none")), "List of 2")
	expect_output(str(summary(s9, print="none")), "List of 2")
})

# ======================================================== #
# GitHub Issue 14                                          #
# ======================================================== #

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
	expect_warning(capture.output(invisible(summary(c4))))
	expect_output(str(c4_1), "List of 20")
	expect_output(str(summary(c4_1, 4, "none"))$school, "List of 7")
	expect_error(cluster_gen_2(n6a, cor_matrix = m5))
})

# ======================================================== #
# GitHub Issue 15                                          #
# ======================================================== #

set.seed(12334)
n7 <- list(school = 10, student = ranges(1000, 5000))
r1 <- cluster_gen_2(n4, rho = c(0.005, 0.01, 0.85))
r1a <- cluster_gen_2(n4, rho = c(0.005, 0.001, 0.85))
r2 <- cluster_gen_2(n4, rho = c(0.05, 0.1, 0.45))
r3 <- cluster_gen_2(n4, rho = c(0.5, 0.45))
r4 <- cluster_gen_2(n4, rho = 0.2)
r5 <- cluster_gen_2(n7, rho = c(0.005, 0.01, 0.85))
r6 <- cluster_gen_2(n7, rho = c(0.05, 0.1, 0.45))
r7 <- cluster_gen_2(n7, rho = c(0.5, 0.45))
r8 <- cluster_gen_2(n7, rho = 0.2)

test_that("Testing rho", {
	expect_output(str(anova(r1, FALSE)), "List of 2")
	expect_output(str(anova(r2, FALSE)), "List of 2")
	expect_output(str(anova(r3, FALSE)), "List of 2")
	expect_output(str(anova(r4, FALSE)), "List of 2")
	expect_output(str(anova(r5, FALSE, FALSE)), "List of 2")
	expect_output(str(anova(r6, FALSE, FALSE)), "List of 2")
	expect_output(str(anova(r7, FALSE, FALSE)), "List of 2")
	expect_output(str(anova(r8, FALSE, FALSE)), "List of 2")
})

# ======================================================== #
# GitHub Issue 16                                          #
# ======================================================== #

set.seed(12334)
r1 <- cluster_gen_2(n4, n_X = 5, c_mean = c(0.1, 0.5, 0.001, 234, 701), sigma = c(0.111, 0.113, 0.115, 0.117, 0.119))
r2 <- cluster_gen_2(n4, n_X = 5, c_mean = c(10, 55, 0.21, 2.34, 5000), sigma = c(40, 100, 0.11, 3, 1500))
r3 <- cluster_gen_2(n4, n_X = 5, c_mean = c(10, 55, 0.21, 2.34, 5000), sigma = c(40, 100, 0.11, 3, 1500))
r4 <- cluster_gen_2(n4, n_X = 5, c_mean = c(0.1, 0.5, 0.001, 234, 701), sigma = c(0.111, 0.113, 0.115, 0.117, 0.119))
r5 <- cluster_gen_2(n6, n_X = 3, c_mean = c(0.3, 0.35, 0.4), sigma = c(0.11, 0.22, 0.33))
r6 <- cluster_gen_2(n6, n_X = 3, c_mean = c(0.7, 100, 355), sigma = c(0.2, 21, 0.7))
r7 <- cluster_gen_2(n10, n_X = 4, c_mean = c(1, 20, 0.25, 50.54), sigma = c(0.3, 3.5, 0.1, 3))
r8 <- cluster_gen_2(n10, n_X = 4, c_mean = c(0.001, 0.005, 213, 234), sigma = c(0.0001, 0.001, 11, 6))

test_that("mean values are the expected", {
	r1s <- summary(r1, print_hetcor=FALSE, print="none")$school$y_bar
	r2s <- summary(r2, print_hetcor=FALSE, print="none")$school$y_bar
	r3s <- summary(r3, print_hetcor=FALSE, print="none")$school$y_bar
	r4s <- summary(r4, print_hetcor=FALSE, print="none")$school$y_bar
	r5s <- summary(r5, print_hetcor=FALSE, print="none")$school$y_bar
	r6s <- summary(r6, print_hetcor=FALSE, print="none")$school$y_bar
	r7s <- summary(r7, print_hetcor=FALSE, print="none")$school$y_bar
	r8s <- summary(r8, print_hetcor=FALSE, print="none")$school$y_bar
	expect_equivalent(r1s["q3"], -9.61962e-4, 1e-3)
	expect_equivalent(r2s, c(10.59184, 56.59062, 0.2136, 2.41009, 5081.18057))
	expect_equivalent(r3s, c(10.4341, 56.5640, 0.2109, 2.2098, 5013.1259), 1e-4)
	expect_equivalent(r4s, c(0.0958, 0.5077, 0.0024, 233.995, 700.9958), 1e-4)
	expect_equivalent(r5s, c(0.30242, 0.50752, 0.48754), 1e-4)
	expect_equivalent(r6s, c(0.78077, 93.89067, 354.82602), 1e-4)
	expect_equivalent(r7s, c(1.05165, 19.59206, 0.25891, 50.14332), 1e-4)
	expect_equivalent(r8s, c(0.00102, 0.005, 211.45723, 232.78356), 1e-4)
})

# ======================================================== #
# GitHub issue 17                                          #
# ======================================================== #

set.seed(12334)
m1 <- matrix(c(
	1, 0.2, 0.3, 0.4,
	0.2, 1, 0.5, 0.7,
	0.3, 0.5, 1, 0.8,
	0.4, 0.7, 0.8, 1
), 4, 4)
mc1 <- cluster_gen_2(n1, c_mean = c(0.1, 0.5, 0.001, 234), cor_matrix = m1)
mc4 <- cluster_gen_2(n7, c_mean = c(0.1, 0.5, 0.001, 234), cor_matrix = m1)
mc1_1 <- cluster_gen_2(
	n1, n_X=4, c_mean = c(0.1, 0.5, 0.001, 234), cor_matrix = m1
)
set.seed(12334)
mc7 <- cluster_gen_2(
	n7, n_X=4, n_W=0, c_mean = c(0.1, 0.5, 0.001, 234), cor_matrix = m1
)
set.seed(12334)
mc7_1 <- cluster_gen_2(
	n7, n_X=2, n_W=2, c_mean = c(0.5, 0.001), cor_matrix = m1
)
mc7_2 <- cluster_gen_2(
	n7, n_X=3, n_W=1, c_mean = c(0.5, 0.001, 234), cor_matrix = m1
)
set.seed(12334)
m2 <- matrix(c(
	1, 0.5, 0.6,
	0.5, 1, 0.9,
	0.6, 0.9, 1
), 3, 3)
mc2 <- cluster_gen_2(n7, c_mean = c(55, 2.34, 5001), cor_matrix = m2)
set.seed(12334)
mc3 <- cluster_gen_2(n7, c_mean = c(55, 2.34, 5001), cor_matrix = m2)
set.seed(12334)
mc3_1 <- cluster_gen_2(
	n7, n_X=3, n_W=0, c_mean = c(55, 2.34, 5001), cor_matrix = m2
)
set.seed(12334)
mc3_2 <- cluster_gen_2(
	n7, n_X=2, n_W=1, c_mean = c(55, 2.34), cor_matrix = m2
)
m3 <- matrix(c(
	1, 0.55, 0.77,
	0.55, 1, 0.33,
	0.77, 0.33, 1
), 3, 3)
set.seed(12334)
mc5 <- cluster_gen_2(n6, c_mean = c(0.3, 0.35, 0.4), cor_matrix = m3)
m4 <- matrix(c(1, 0.55, 0.55, 1), 2, 2)
set.seed(12334)
mc6 <- cluster_gen_2(n6, c_mean = c(0.7, 355), cor_matrix = m4)
set.seed(12334)
mc7 <- cluster_gen_2(n10, c_mean = c(20, 0.25, 50.54), cor_matrix = m3)
set.seed(12334)
mc8 <- cluster_gen_2(n10, c_mean = c(213, 234), cor_matrix = m4)

set.seed(12334)
mc8 <- cluster_gen_2(
	n10, n_X=2, n_W=0, c_mean = c(213, 234), cor_matrix = m4
)

test_that("Clusters are created correctly", {
	expect_output(str(mc1), "school:List of 3")
	expect_output(str(mc4), "school:List of 10")
	expect_output(str(mc1_1), "school:List of 3")
	expect_output(str(mc7), "country:List of 2")
	expect_output(str(mc7_1), "school:List of 10")
	expect_output(str(mc7_2), "school:List of 10")
	expect_output(str(mc2), "school:List of 10")
	expect_output(str(mc3), "school:List of 10")
	expect_output(str(mc3_1), "school:List of 10")
	expect_output(str(mc3_2), "school:List of 10")
})
test_that("Summaries produce expected output", {
	expect_output(str(summary(mc7_2, print="none")), "List of 7")
	expect_output(str(summary(mc3_1, print="none")), "List of 7")
	expect_output(str(summary(mc3_2, print="none")), "List of 7")
	expect_output(str(summary(mc7_1, print="none")), "List of 7")
	expect_output(str(summary(mc8, print="none")), "List of 7")
})

# ======================================================== #
# GitHub issue 18                                          #
# ======================================================== #

m1 <- matrix(
	c(
		1, 0.2, 0.3, 0.4,
		0.2, 1, 0.5, 0.7,
		0.3, 0.5, 1, 0.8,
		0.4, 0.7, 0.8, 1
	), 4, 4
)
m2 <- matrix(
	c(
		1, 0.5, 0.6,
		0.5, 1, 0.9,
		0.6, 0.9, 1
	), 3, 3
)
m3 <- matrix(
	c(
		1, 0.55, 0.77,
		0.55, 1, 0.33,
		0.77, 0.33, 1
	), 3, 3
)
m4 <- matrix(c(1, 0.55, 0.55, 1), 2, 2)
set.seed(12334); sc1 <- cluster_gen_2(n7, sigma = 1:4, cor_matrix = m1)
set.seed(123554); sc1_1 <- cluster_gen_2(n7, sigma = 1:4, cor_matrix = m1)
sc2 <- cluster_gen_2(n7, sigma = c(0.5, 0.7, 10), cor_matrix = m2)
sc3 <- cluster_gen_2(n7, sigma = c(0.2, 0.4, 5), cor_matrix = m3)
set.seed(12334)
sc4 <- cluster_gen_2(n7, sigma = c(7, 1.5), cor_matrix = m4)
sc1_2 <- cluster_gen_2(n7, n_X=4, n_W=0, sigma = c(1, 2, 3, 4), cor_matrix = m1)
set.seed(12334)
sc2_2 <- cluster_gen_2(n7, n_X=2, n_W=1, sigma = c(0.5, 0.7), cor_matrix = m2)
set.seed(12334)
sc3_2 <- cluster_gen_2(n7, n_X=3, n_W=0, sigma = c(0.2, 0.4, 5), cor_matrix = m3)
set.seed(12334)
sc4_2 <- cluster_gen_2(n7, n_X=2, n_W=0, sigma = c(7, 1.5), cor_matrix = m4)

test_that("Clusters are generated correctly", {
	expect_output(str(sc1), "school:List of 10")
	expect_output(str(sc1_1), "school:List of 10")
	expect_output(str(sc2), "school:List of 10")
	expect_output(str(sc3), "school:List of 10")
	expect_output(str(sc4), "school:List of 10")
	expect_output(str(sc1_2), "school:List of 10")
	expect_output(str(sc2_2), "school:List of 10")
	expect_output(str(sc3_2), "school:List of 10")
	expect_output(str(sc4_2), "school:List of 10")
})
test_that("Summaries produce expected output", {
	expect_output(str(summary(sc1, print="none")), "List of 7")
	expect_output(str(summary(sc2, print="none")), "List of 7")
	expect_output(str(summary(sc3, print="none")), "List of 7")
	expect_output(str(summary(sc4, print="none")), "List of 7")
})

# ======================================================== #
# GitHub Issue 19                                          #
# ======================================================== #

set.seed(12334)
m1 <- matrix(
	c(1, 0.2, 0.3, 0.4, 0.2, 1, 0.5, 0.7, 0.3, 0.5, 1, 0.8, 0.4, 0.7, 0.8, 1),
	4, 4
)
sample_size <- 50
rho_hat <- matrix(nrow=sample_size, ncol=2)
for (i in seq_len(sample_size)) {
	cr1 <- cluster_gen_2(n7, n_X=2, n_W=2, cor_matrix=m1, rho=c(0.1, 0.2))
	rho_hat[i, ] <- c(
		anova(cr1, calc.se=FALSE, print=FALSE)$population$q1[[3]],
		anova(cr1, calc.se=FALSE, print=FALSE)$population$q2[[3]]
	)
}
n7_500 <- list(school = 500, student = ranges(10, 50))
cr1_500 <- cluster_gen_2(n7_500, n_X=2, n_W=2, cor_matrix=m1, rho=c(0.1, 0.2))

test_that("rho converges to true values", {
	expect_equivalent(apply(rho_hat, 2, mean), c(0.1, 0.2), tol = 0.05)
	expect_equivalent(
		object = c(
			anova(cr1_500, calc.se=FALSE, print=FALSE)$population$q1[[3]],
			anova(cr1_500, calc.se=FALSE, print=FALSE)$population$q2[[3]]
		),
		expected = c(0.1, 0.2),
		tol = 0.05
	)
})

# ======================================================== #
# GitHub issue 20                                          #
# ======================================================== #

m1 <- matrix(
	c(1, 0.2, 0.3, 0.4,0.2, 1, 0.5, 0.7, 0.3, 0.5, 1, 0.8, 0.4, 0.7, 0.8, 1),
	4, 4
)
m2 <- matrix(c(1, 0.5, 0.6, 0.5, 1, 0.9, 0.6, 0.9, 1), 3, 3)
m3 <- matrix(c(1, 0.55, 0.77, 0.55, 1, 0.33, 0.77, 0.33, 1), 3, 3)
m4 <- matrix(c(1, 0.55, 0.55, 1), 2, 2)
set.seed(12334)
csc1 <- cluster_gen_2(
	n7, n_X=4, n_W=0, c_mean=c(10, 20, 30, 40), sigma=c(1, 2, 3, 4),
	cor_matrix=m1
)
set.seed(12334)
csc2 <- cluster_gen_2(
	n7, n_X=1, n_W=2, c_mean=100, sigma=3, cor_matrix=m2
)

set.seed(12334)
csc3 <- cluster_gen_2(
	n7, n_X=2, n_W=1, c_mean=c(100, 150), sigma=c(3, 4), cor_matrix=m3
)
set.seed(12334)
csc4 <- cluster_gen_2(
	n7, n_X=2, n_W=0, c_mean=c(210, 310), sigma=c(2, 5), cor_matrix=m4
)

test_that("Clusters are generated correctly", {
	expect_output(str(csc1), "school:List of 10")
	expect_output(str(csc2), "school:List of 10")
	expect_output(str(csc3), "school:List of 10")
	expect_output(str(csc4), "school:List of 10")
	expect_error(
		cluster_gen_2(
			n7, n_X = 5,
			c_mean = list(15, c(10, 55, 0.21, 2.34, 5000)),
			sigma  = list(20, c(40, 100, 0.11, 3, 1500))
		)
	)
	expect_warning(
		cluster_gen_2(
			n7, n_X = 5,
			c_mean = c(10, 55, 0.21, 2.34, 5000),
			sigma  = c(40, 100, 0.11, 3.0, 1500),
			rho = 0.2
		)
	)
})

csc2_norho <- cluster_gen_2(
	n7, n_X = 5,
	c_mean = c(10, 55, 0.21, 2.34, 5000),
	sigma  = c(40, 100, 0.11, 3.0, 1500)
)

test_that("Summaries are generated correctly", {
	expect_equivalent(
		object = summary(csc2, print="none")$school$y_bar,
		expected = 100,
		tol = 1
	)
	expect_equivalent(
		object = summary(csc3, print="none")$school$y_bar,
		expected = c(100, 150),
		tol = 1
	)
	expect_equivalent(
		object = summary(csc2_norho, print="none")$school$y_bar,
		expected =  c(10, 55, 0.21, 2.34, 5000),
		tol = 1
	)
})
