#' @title Defines vector as range
#' @description Redefines the class of a vector as "range"
#' @param x first element
#' @param y second element
#' @return `c(x, y)`, but with the "range" class
#' @note This function was created to be used as an element in the `N` argument of `cluster_gen`. The name was chosen to avoid conflict with `base::range()`.
#' #TODO: mention ranges being demoted when used within c()
#' @export
ranges <- function(x, y)
{
    if (x > y) stop("The first argument cannot be larger than the second.")
    out <- c(x, y)
    class(out) <- "range"
    return(out)
}