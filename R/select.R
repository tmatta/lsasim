#' @title Transform regular vector into selection vector
#' @description Attaches a "select" class to a vector
#' @param x vector
#' @return same as `x`, but with a class attribute that classifies `x` as "select"
#' @export
select <- function(x, ...)
{
  out <- c(x, ...)
  class(out) <- "select"
  return(out)
}