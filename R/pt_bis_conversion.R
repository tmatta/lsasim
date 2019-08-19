#' Analytical point-biserial conversion
#'
#' @param bis_cor biserial correlations
#' @param pr_group1 probability of group 1
#'
#' @importFrom stats dnorm qnorm
#'
#' @export
pt_bis_conversion <- function(bis_cor, pr_group1){
  yord      <- dnorm(qnorm(pr_group1))
  pr_group2 <- 1 - pr_group1
  bis_adj   <- sqrt(pr_group1 * pr_group2) / yord
  pt_bis    <- bis_cor / bis_adj
  return(pt_bis)
}

