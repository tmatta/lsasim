#' Randomly generate the quantity of background variables
#'
#' @param n_vars number of variables in total (\code{n_X + n_W + theta})
#' @param n_X number of continuous variables
#' @param n_W number of categorical variables
#' @param theta number of latent variables
#'
#' @return vector with n_vars, n_X and n_W
#' @export
gen_variable_n <- function(n_vars, n_X, n_W, theta = FALSE) {
  rzeropois <- function(lambda) {
    # Generates one number sampled from a Zero-truncated Poisson distribution
    k <- 1
    t <- exp(-lambda) / (1 - exp(-lambda)) * lambda
    s <- t
    u <- runif(1)
    while (s < u) {
      k <- k + 1
      t <- t * lambda / k
      s <- s + t
    }
    return(k)
  }
  if (is.null(n_vars)) {
    if (is.null(n_W) | is.null(n_X)) {
      n_vars <- rzeropois(lambda = 4) + ifelse(is.null(n_W), 0, n_W) +
        ifelse(is.null(n_X), 0, n_X) + theta
    } else {
      n_vars <- n_W + n_X + theta

    }
  }
  if (is.null(n_X) & is.null(n_W)) {
    n_X <- rbinom(n = 1, size = n_vars, prob = .2)
    n_W <- n_vars - n_X - theta
  } else {
    # Either n_X or n_W are missing
    if (is.null(n_X)) n_X <- rbinom(n = 1, prob = .2, size = n_vars - n_W)
    if (is.null(n_W)) n_W <- rbinom(n = 1, prob = .8, size = n_vars - n_X)
  }
  return(c(n_vars = n_vars, n_X = n_X, n_W = n_W, theta = theta))
}
