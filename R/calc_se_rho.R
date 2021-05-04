#' @title Calculate Standard Error of Intraclass Correlation
#' @seealso anova.lsasimcluster
#' @param rho intraclass correlation
#' @param n_j number of elements in class j
#' @param N number of classes j
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.
calc_se_rho <- function(rho, n_j, N) {
    if (stats::var(n_j) == 0) {
        n <- unname(n_j[1])
        se_rho <- (1 - rho) * (1 + (n - 1) * rho) *
            sqrt(2 / (n * (n - 1) * (N - 1)))
    } else {
        stop("Not yet implemented for the case of varying n_j")
        # TODO: Implement equation (6.1) from Donner (1986) - issue #22
    }
    return(se_rho)
}