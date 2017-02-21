#' Generation of random correlation matrix 
#'
#' Creates a random correlation matrix.  
#'
#' @param n_var integer number of variables.
#' 
#' @section Details:
#' The result from \code{cor_gen} can be used directly with the \code{cor_matrix}
#' argument of \code{questionnaire_gen}.
#' 
#' @examples
#' cor_gen(n_var = 10)
#' 
#' @export
cor_gen <- function(n_var){ 
  r <- matrix(runif(n_var * n_var, -1, 1), ncol = n_var) 
  rxr <- r %*% t(r) 
  q <- cov2cor(rxr) 
  return(q)
}


