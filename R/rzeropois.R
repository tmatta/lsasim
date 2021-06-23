#' @title Generate data from a Zero-truncated Poisson
#' @description Random generation of one observation of a random variable distributed as a Zero-truncated Poisson
#' @param lambda corresponds to the lambda parameter of a Poisson
#' @details The zero-truncated Poisson (a.k.a. conditional Poisson or positive Poisson) distribution is a discrete probability distribution whose support is the set of positive integers.
rzeropois <- function(lambda) {
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