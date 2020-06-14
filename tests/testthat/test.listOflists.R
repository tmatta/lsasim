context("n_W as list of lists")
# ==============================================================================
# Basic elements
# ==============================================================================
n3 <- c(school = 3, class = 2, student = 10)

# nW1 implies the following number of categories:
# schools = q11-12-13, q21-22-23-24, q31-32-33-34
# classes = q11-12, q21-q22-23-24-25
nW1 <- list(list(3, 4, 4), list(2, 5))

# nW2 implies the following number of categories:
# school1 = q11-?, q21-?, q31-?
# school2 = q11-?, q21-?, q31-?, q41-?
# school3 = q11-?, q21-?, q31-?, q41-?
# classX = n_W = X (X = 1,...,6)
nW2 <- list(c(3, 4, 4), 1:6)

# nW3 implies the following number of categories:
# schools = q11-12-13, q21-22-23
# classes = q11-12-13-14, q21-q22-23-24, q31-q32-q33-q34
nW3 <- list(school = list(3, 3), class = list(4, 4, 4))

# ==============================================================================
# Function wrapper
# ==============================================================================
cluster_gen_2 <- function(...) {
	cluster_gen(..., verbose = FALSE, calc_weights = FALSE)
}
# ==============================================================================
# Generating datasets
# ==============================================================================
df1 <- cluster_gen_2(n3, n_X = 0, n_W = nW1, family = "gaussian")
df2 <- cluster_gen_2(n3, n_X = 0, n_W = nW2, family = "gaussian")
df3 <- cluster_gen_2(n3, n_X = 0, n_W = nW3, family = "gaussian")
# ==============================================================================
# Testing
# ==============================================================================
test_that("questionnaire_gen correcty parses n_W as a list of lists: df1", {
	expect_output(str(df1$school[[1]]$q1), "Factor w/ 3 levels")
	expect_output(str(df1$school[[2]]$q1), "Factor w/ 3 levels")
	expect_output(str(df1$school[[3]]$q1), "Factor w/ 3 levels")
	expect_output(str(df1$school[[1]]$q2), "Factor w/ 4 levels")
	expect_output(str(df1$school[[2]]$q2), "Factor w/ 4 levels")
	expect_output(str(df1$school[[3]]$q2), "Factor w/ 4 levels")
	expect_output(str(df1$school[[1]]$q3), "Factor w/ 4 levels")
	expect_output(str(df1$school[[2]]$q3), "Factor w/ 4 levels")
	expect_output(str(df1$school[[3]]$q3), "Factor w/ 4 levels")
	expect_output(str(df1$class[[1]]$q1), "Factor w/ 2 levels")
	expect_output(str(df1$class[[2]]$q1), "Factor w/ 2 levels")
	expect_output(str(df1$class[[3]]$q1), "Factor w/ 2 levels")
	expect_output(str(df1$class[[4]]$q1), "Factor w/ 2 levels")
	expect_output(str(df1$class[[5]]$q1), "Factor w/ 2 levels")
	expect_output(str(df1$class[[1]]$q2), "Factor w/ 5 levels")
	expect_output(str(df1$class[[2]]$q2), "Factor w/ 5 levels")
	expect_output(str(df1$class[[3]]$q2), "Factor w/ 5 levels")
	expect_output(str(df1$class[[4]]$q2), "Factor w/ 5 levels")
	expect_output(str(df1$class[[5]]$q2), "Factor w/ 5 levels")
	expect_output(str(df1$class[[6]]$q1), "Factor w/ 2 levels")
	expect_output(str(df1$class[[6]]$q2), "Factor w/ 5 levels")
})

test_that("questionnaire_gen correcty parses n_W as a list of lists: df2", {
	expect_length(sapply(df2$school[1], names), 3 + 2)
	expect_length(sapply(df2$school[2], names), 4 + 2)
	expect_length(sapply(df2$school[3], names), 4 + 2)
	expect_length(sapply(df2$class[1], names), 1 + 2)
	expect_length(sapply(df2$class[2], names), 2 + 2)
	expect_length(sapply(df2$class[3], names), 3 + 2)
	expect_length(sapply(df2$class[4], names), 4 + 2)
	expect_length(sapply(df2$class[5], names), 5 + 2)
	expect_length(sapply(df2$class[6], names), 6 + 2)
})

test_that("questionnaire_gen correcty parses n_W as a list of lists: df3", {
	expect_output(str(df3$school[[1]]$q1), "Factor w/ 3 levels")
	expect_output(str(df3$school[[2]]$q1), "Factor w/ 3 levels")
	expect_output(str(df3$school[[3]]$q1), "Factor w/ 3 levels")
	expect_output(str(df3$school[[1]]$q2), "Factor w/ 3 levels")
	expect_output(str(df3$school[[2]]$q2), "Factor w/ 3 levels")
	expect_output(str(df3$school[[3]]$q2), "Factor w/ 3 levels")
	expect_output(str(df3$class[[1]]$q1), "Factor w/ 4 levels")
	expect_output(str(df3$class[[2]]$q1), "Factor w/ 4 levels")
	expect_output(str(df3$class[[3]]$q1), "Factor w/ 4 levels")
	expect_output(str(df3$class[[4]]$q1), "Factor w/ 4 levels")
	expect_output(str(df3$class[[5]]$q1), "Factor w/ 4 levels")
	expect_output(str(df3$class[[6]]$q1), "Factor w/ 4 levels")
	expect_output(str(df3$class[[1]]$q2), "Factor w/ 4 levels")
	expect_output(str(df3$class[[2]]$q2), "Factor w/ 4 levels")
	expect_output(str(df3$class[[3]]$q2), "Factor w/ 4 levels")
	expect_output(str(df3$class[[4]]$q2), "Factor w/ 4 levels")
	expect_output(str(df3$class[[5]]$q2), "Factor w/ 4 levels")
	expect_output(str(df3$class[[6]]$q2), "Factor w/ 4 levels")
	expect_output(str(df3$class[[1]]$q3), "Factor w/ 4 levels")
	expect_output(str(df3$class[[2]]$q3), "Factor w/ 4 levels")
	expect_output(str(df3$class[[3]]$q3), "Factor w/ 4 levels")
	expect_output(str(df3$class[[4]]$q3), "Factor w/ 4 levels")
	expect_output(str(df3$class[[5]]$q3), "Factor w/ 4 levels")
	expect_output(str(df3$class[[6]]$q3), "Factor w/ 4 levels")
})