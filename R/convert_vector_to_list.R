#' @title Convert Vector to Expanded List
#' @description Converts a vector to list where each element is replicated a certain number of times depending on the previous vector
#' @param x vector to be converted
#' @param ref_x reference vector
#' @return expanded/replicated version of x
#' @export
convert_vector_to_list <- function(x, ref_x = x)
{
  x_list <- as.list(x)
  ref_x_list <- as.list(ref_x)
  if (class(x_list[[1]]) == "range") {
    x_list[[1]] <- sample(x_list[[1]][1]:x_list[[1]][2], size = 1)
    ref_x_list[[1]] <- x_list[[1]]
  }
  for (lvl in 2:length(x)) {
    if (class(x_list[[lvl]]) == "range") {
      x_list[[lvl]] <- sample_within_range(x[[lvl]], sum(ref_x_list[[lvl - 1]]))
      ref_x_list[[lvl]] <- x_list[[lvl]]
    } else {
      x_list[[lvl]] <- rep(x[[lvl]], sum(ref_x_list[[lvl - 1]]))
      ref_x_list[[lvl]] <- rep(ref_x[[lvl]], sum(ref_x_list[[lvl - 1]]))
    }
  }
  x <- x_list
  return(x)
}