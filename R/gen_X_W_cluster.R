#' @title Generate n_X and n_W for clusters
#' @description Generates n_X and n_W for `cluster_gen` based on a correlation matrix
#' @param n_levels number of levels
#' @param separate to the `separate_questionnaires` argument of `cluster_gen`
#' @param class_cor corresponds to the `class_cor` argument of `cluster_gen`
gen_X_W_cluster <- function(n_levels, separate, class_cor) {
    number_of_W <- function() rbinom(n = rzeropois(5), prob = .5, size = 3) + 2
    if (separate) {
        n_X <- list()
        for (l in seq(n_levels)) {
            n_X[[l]] <- rzeropois(1.5) # a positive number of Xs
        }
        n_W <- list()
        for (l in seq(n_levels)) {
            n_W[[l]] <- as.list(number_of_W())
        }
    } else {
        n_X <- rzeropois(1.5) # a positive number of Xs
        n_W <- as.list(number_of_W())
    }
    out <- list("n_X" = n_X, "n_W" = n_W)
    return(out)
}