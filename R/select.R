#' @title Transform regular vector into selection vector
#' @description Attaches a "select" class to a vector
#' @param ... parameters to be passed to `c()`
#' @return same as `x`, but with a class attribute that classifies `x` as "select"
#' @note This function was created to be used instead of `c()` in the `n` argument of `cluster_gen`.
#' @export
select <- function(...)
{
  out <- unname(c(...))
  class(out) <- "select"
  return(out)
}