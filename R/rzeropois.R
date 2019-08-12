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