# Library loading (during development)
library(devtools)
library(testthat)
library(devkit)
library(lsasim)
install("../lsasim")

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

cluster_gen(clusters = c(2, 2, 2),
            separate_questionnaires = TRUE,
            collapse = "full")

cluster_gen(c(2, 2, 2), separate_questionnaires = FALSE, n_obs = 3,
            collapse = "full")

cluster_gen(clusters = c(2, 1, 3),
            labels = c("country", "school", "class"),
            # n_X = c(1, 2, 1), n_W = c(1, 2, 0),
            # FIXME: give warning for n_W as a vector and !sep_quest
            #n_X = 1, n_W = 1,  # FIXME: also broken
            # FIXME: n_X = 0 breaks
            n_obs = c(3, 4, 2),
            # c_mean = list(10, 100, 1000),  # FIXME: vector c_mean not working
            separate_questionnaires = FALSE,  # FIXME: numbers missing if FALSE
            collapse = "full")