#' @title Calculate variance between classes
#' @param n_j number of elements in class j
#' @param y_bar_j mean of variable of interest per class j
#' @param y_bar mean of variable of interest across classes
#' @param n_tilde function of the variance of n_N, M and N. See documentation and code of \code{lsasim:::summary.lsasimcluster} for details
#' @param N number of classes j
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.
#' @seealso anova.lsasimcluster
calc_var_between <- function(n_j, y_bar_j, y_bar, n_tilde, N) {
    if (class(y_bar_j)[1] != "matrix") y_bar_j <- as.matrix(y_bar_j)
    if (is.null(colnames(y_bar_j))) {
        X <- ncol(y_bar_j)
    } else {
        X <- colnames(y_bar_j)
    }
    s2_between <- vector()
    for (x in X) {
        s2_between <- append(
            s2_between,
            sum(n_j * (y_bar_j[, x] - y_bar[x]) ^ 2) / (n_tilde * (N - 1))
        )
    }
    names(s2_between) <- X
    return(s2_between)
}