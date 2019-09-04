# context("Basic summary statistics for cluster samples")

# set.seed(230)
# df <- cluster_gen(c(sch = 4, stu = 10), n_X = 3, n_W = 1, verb = FALSE)
# jackknife(df$sch[[4]], c("sch.weight", "final.stu.weight"))
# cluster_estimates(df, var_method = "Jackknife", verbose = FALSE, print_reps = TRUE)
# ASK: Rename to "replicate_weights" and have the statistics be secondary?