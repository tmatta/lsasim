#' Generation of random correlation matrix
#'
#' Creates a random correlation matrix.
#'
#' @param n_var integer number of variables.
#' @param cov_bounds a vector containing the bounds of the covariance matrix.
#'
#' @section Details:
#' The result from \code{cor_gen} can be used directly with the \code{cor_matrix}
#' argument of \code{questionnaire_gen}.
#'
#' @examples
#' cor_gen(n_var = 10)
#'
#' @export
cor_gen <- function(n_var, cov_bounds = c(-1, 1)){
  r <- matrix(runif(n_var * n_var, cov_bounds[1], cov_bounds[2]), ncol = n_var)
  rxr <- r %*% t(r)
  q <- cov2cor(rxr)
  return(q)
}