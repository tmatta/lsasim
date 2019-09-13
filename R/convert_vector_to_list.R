#' @title Convert Vector to Expanded List
#' @description Converts a vector to list where each element is replicated a certain number of times depending on the previous vector
#' @param x vector to ve converted
#' @return expanded/replicated version of x
#' @export
convert_vector_to_list <- function(x)
{
  x_list <- as.list(x)
  for (lvl in 2:(length(x))) {
    if (class(x_list[[lvl]]) == "range") {
      x_list[[lvl]] <- sample_within_range(x[[lvl]], sum(x_list[[lvl - 1]]))
    } else {
      x_list[[lvl]] <- rep(x[[lvl]], sum(x_list[[lvl - 1]]))
    }
  }
  x <- x_list
  return(x)
}