#' Randomly generate a matrix of factor loadings
#'
#' @param n_ind number of indicators per factor
#' @param n_fac number of factors
#' @param limits vector with lower and upper limits for the uniformly-generated
#'   Lambdas
#' @param row_names vector with row names
#' @param col_names vector with col names
#'
lambda_gen <- function(n_ind, n_fac, limits, row_names, col_names) {
  n_ind_rep <- rep(n_ind, n_fac)  # number of indicators for each factor
  Lambda_mx <- matrix(0, ncol = length(n_ind_rep), nrow = sum(n_ind_rep))

  l_start <- cumsum(n_ind_rep) - (n_ind_rep - 1)
  l_end <- l_start + n_ind_rep - 1

  for (i in seq(n_ind_rep)) {
    loadings <- runif(n_ind_rep[i], limits[1], limits[2])
    Lambda_mx[l_start[i]:l_end[i], i] <- loadings
  }
  Lambda <- Lambda_mx
  dimnames(Lambda) <- list(row_names, col_names)
  return(Lambda)
}
