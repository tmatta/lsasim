#' Randomly generate a matrix of factor loadings
#'
#' @param n_ind number of indicators per factor
#' @param limits vector with lower and upper limits for the uniformly-generated
#'   Lambdas
#' @param row_names vector with row names
#' @param col_names vector with col names
#'
lambda_gen <- function(n_ind, limits, row_names, col_names) {
  Lambda_mx <- matrix(0, ncol = length(n_ind), nrow = sum(n_ind))

  l_start <- cumsum(n_ind) - (n_ind - 1)
  l_end <- l_start + n_ind - 1

  for (i in seq(n_ind)) {
    loadings <- runif(n_ind[i], limits[1], limits[2])
    Lambda_mx[l_start[i]:l_end[i], i] <- loadings
  }
  Lambda <- Lambda_mx
  dimnames(Lambda) <- list(row_names, col_names)
  return(Lambda)
}
