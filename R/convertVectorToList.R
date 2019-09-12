#' @title Convert Vector to Expanded List
#' @description Converts a vector to list where each element is replicated a certain number of times depending on the previous vector
#' @param x vector to ve converted
#' @return expanded/replicated version of x
convertVectorToList <- function(x)
{
  x_list <- as.list(x)
  for (lvl in 2:(length(x))) {
    x_list[[lvl]] <- rep(x[[lvl]],
                         prod(seq(from = x[[1]],
                                  to   = x[[lvl - 1]],
                                  length.out = lvl - 1)))
  }
  x <- x_list
  return(x)
}