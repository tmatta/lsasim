#' @title Calculate ñ
#' @description Calculates n tilde
#' @seealso ?lsasim:::summary.lsasimcluster
#' @param M total number of population (i.e., sum of n_j over all j)
#' @param N number of each class j
#' @param n_j vector with size of each class j
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.

calc_n_tilde <- function(M, N, n_j) {
    n_bar <- M / N
    s2_n_j <- sum((n_j - n_bar) ^ 2) / (N - 1)
    n_tilde <- n_bar - s2_n_j / (N * n_bar)
    return(n_tilde)
}