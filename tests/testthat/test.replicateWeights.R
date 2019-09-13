# context("Basic summary statistics for cluster samples")

# set.seed(230)
# df <- cluster_gen(c(sch = 4, stu = 10), n_X = 3, n_W = 1, verb = TRUE)
# jackknife(df$sch[[4]], c("sch.weight", "final.stu.weight"))
# df2 <- cluster_estimates(df, var_method = "Jackknife", verbose = FALSE, print_data = TRUE)
# ASK: Rename to "replicate_weights" and have the statistics be secondary?