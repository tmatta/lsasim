context("Sampling alternatives on cluster_gen")

enn <- list(3, c(20, 15, 25))
ENN <- list(5, c(200, 500, 400, 100, 100))
set.seed(9674731); cluster_gen(enn, ENN, calc_weights = TRUE, sampling_method = "mixed")
