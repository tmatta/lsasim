#' @title Calculate variance within classes
#' @seealso anova.lsasimcluster
#' @param n_j number of elements in class j
#' @param M total sample size
#' @param N number of classes j
#' @param s2_j variance of all elements in class j
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.
calc_var_within <- function(n_j, s2_j, M, N) {
    X <- colnames(s2_j)
    s2_within <- vector()
    for (x in X) {
        s2_within <- append(s2_within, sum((n_j - 1) * s2_j[, x]) / (M - N))
    }
    names(s2_within) <- X
    return(s2_within)
}