#' Generation of random correlation matrix. 
#'
#' Creates a random correlation matrix.  
#'
#' @param n_var dimension of matrix
#' 
#' @return A \code{n_var} by \code{n_var} \code{matrix} 
#' 
#' @examples
#' cor_gen(nvar = 10)
#' 
cor_gen <- function(n_var){ 
  r <- matrix(runif(n_var * n_var, -1, 1), ncol = n_var) 
  rxr <- r %*% t(r) 
  q <- cov2cor(rxr) 
  return(q)
}


