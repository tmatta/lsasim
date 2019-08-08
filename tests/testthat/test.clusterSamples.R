# Library loading (during development)
library(devtools)
library(testthat)
library(devkit)
install("../lsasim")
library(lsasim)
packageVersion("lsasim")

# Actual test
context("Cluster samples")

# # Basic dataset
# clusters1 <- cluster_gen(c(3, 2), c(10, 5), n_X = 1, n_W = list(2, 3),
#                          collapse = TRUE)
# message("Data generated")
# print(clusters1)
# message("Means per cluster and variable")
# print(lapply(clusters1, function(x) aggregate(. ~ clusterID, x[-1], mean)))

# # Changing means
# clusters2 <- cluster_gen(c(3, 2), 1000, n_X = 1, n_W = list(2, 2),
#                          c_mean = list(1, 3), collapse = TRUE)
# message("Different means per cluster and variable")
# print(lapply(clusters2, function(x) aggregate(. ~ clusterID, x[-1], mean)))

# test_that("Data is correctly generated", {
#     expect_output(str(clusters1$class), "30 obs. of  5 variables:")
# })

# Testing scenario:
# Countries          1               2
#                --------        ---------
# Schools        1       2       1       2
#               ---     ---     ---     ---
# Classes       1 2     1 2     1 2     1 2

cluster_gen(cluster = c(2, 2, 2), separate_questionnaires = TRUE,
            collapse = TRUE, c_mean = list(c(10, 100), 20, 30))
# TODO: only collapses classes
# TODO: complete collapse to one data frame (repeating answers for teachers in students)
# TODO: higher level (e.g. country) n_obs should match number in a lower level
# TODO: ensure consistency of level numbering

# TODO: incorporate n_obs (students) as level?
# TODO: Control over inter-class correlation (intra-class is handled by quest_gen?)

# print(cluster_gen(c(2, 2, 2), c_mean = list(c(10, 2), c(20, 4), c(40, 8)),
#       separate_questionnaires = FALSE))
cluster_gen(c(2, 2, 2), separate_questionnaires = FALSE, n_obs = 3,
            collapse = TRUE)
# TODO: add student ID to clusterID to make a unique ID