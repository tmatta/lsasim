context("Cluster samples")

# Basic dataset
clusters1 <- cluster_gen(c(3, 2), c(10, 5), n_X = 1, n_W = list(2, 3))
message("Data generated")
print(clusters1)
message("Means per cluster and variable")
print(lapply(clusters1, function(x) aggregate(. ~ clusterID, x[-1], mean)))

# Changing means
clusters2 <- cluster_gen(c(3, 2), 1000, n_X = 1, n_W = list(2, 2),
                         c_mean = list(1, 3))
message("Different means per cluster and variable")
print(lapply(clusters2, function(x) aggregate(. ~ clusterID, x[-1], mean)))

test_that("Data is correctly generated", {
    expect_output(str(clusters1$class), "30 obs. of  5 variables:")
})