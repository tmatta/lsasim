#' @title Defines vector as range
#' @description Redefines the class of a vector as "range"
#' @param x first element
#' @param y second element
#' @return `c(x, y)`, but with the "range" class
#' @note This function was created to be used as an element in the `N` argument of `cluster_gen`. The name was chosen to avoid conflict with `base::range()`.
#'
#' `ranges()` should always be used within a `list()`. Inserting a "range" vector inside a common vector (`c()`) will result in a common vector. For example, `c(3, ranges(8, 10))` is the same as `c(3, 8, 10)`, because when faced with conflicting classes in the same element, R will resolve to the simpler case ("numeric", in this case). An easier way to understand this concept is by checking `class(c(3, "a"))` is "character", meaning the number 3 was devolved into a character "3".
#'
#' @export
ranges <- function(x, y)
{
    if (x > y) stop("The first argument cannot be larger than the second.")
    out <- c(x, y)
    class(out) <- "range"
    return(out)
}