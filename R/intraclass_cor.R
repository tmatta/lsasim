#' @title Intraclass correlation
#' @description Calculates the intraclass correlation of clustered data
#' @param tau2_hat estimate of the true between-class correlation
#' @param sigma2_hat estimate of the true within-class correlation
#' @references Snijders, T. A. B., & Bosker, R. J. (1999). Multilevel Analysis. Sage Publications.
#' @seealso cluster_gen ?lsasim:::summary.lsasimcluster
#' @export
intraclass_cor <- function(tau2_hat, sigma2_hat) {
    rho_hat <- tau2_hat / (tau2_hat + sigma2_hat)
    return(rho_hat)
}