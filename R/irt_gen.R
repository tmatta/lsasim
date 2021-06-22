#' Simulate item responses from an item response model
#'
#' Creates a data frame of item parameters.
#'
#' @param theta numeric ability estimate.
#' @param b_par numeric or vector of numerics difficulty parameter(s).
#' @param a_par numeric discrimination parameter.
#' @param c_par numeric guessing parameter.
#' @param D numeric parameter to specify logisitic (1) or normal (1.7).
#'
#' @examples
#' irt_gen(theta = 0.2, b_par = 0.6)
#' irt_gen(theta = 0.2, a_par = 1.15, b_par = 0.6)
#' irt_gen(theta = 0.2, a_par = 1.15, b_par = 0.6, c_par = 0.2)
#'
#' @export
irt_gen <- function(theta, a_par = 1, b_par, c_par = 0, D = 1) {
  unsummed <- c(0, D * a_par * (theta - b_par))
  num <- exp(cumsum(unsummed))
  den <- sum(num)
  response_pr <- c_par + (1 - c_par) * (num / den)
  response_pr[1] <- ifelse(c_par > 0, 1 - response_pr[2], response_pr[1])
  y <- sample(1:length(response_pr) - 1, size = 1, prob = response_pr)
  return(y)
}


