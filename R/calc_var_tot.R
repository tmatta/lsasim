#' @title Calculate the total variance
#' @seealso anova.lsasimcluster
#' @param n_tilde function of the variance of n_N, M and N. See documentation and code of \code{lsasim:::summary.lsasimcluster} for details
#' @param M total sample size
#' @param N number of classes j
#' @param s2_within Within-class variance
#' @param s2_between Between-class variance
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.
calc_var_tot <- function(M, N, n_tilde, s2_within, s2_between) {
    X <- names(s2_within)
    s2_tot <- vector()
    for (x in X) {
        s2_tot <- append(
            s2_tot,
            (M - N) / (M - 1) * s2_within[x] +
                n_tilde * (N - 1) / (M - 1) * s2_between[x]
        )
    }
    names(s2_tot) <- X
    return(s2_tot)
}