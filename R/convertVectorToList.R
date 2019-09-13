#' @title Convert Vector to Expanded List
#' @description Converts a vector to list where each element is replicated a certain number of times depending on the previous vector
#' @param x vector to ve converted
#' @return expanded/replicated version of x
convertVectorToList <- function(x)
{
  x_list <- as.list(x)
  for (lvl in 2:(length(x))) {
    if (class(x_list[[lvl]]) == "range") {
      browser()#TEMP
    } else {
      # browser()#TEMP
      x_list[[lvl]] <- rep(x[[lvl]], sum(x_list[[lvl - 1]]))
                          # prod(seq(from = x[[1]],
                          #           to   = x[[lvl - 1]],
                          #           length.out = lvl - 1)))
    }
  }
  x <- x_list
  return(x)
}


# TODO: Testing. Remove when OK
# n <- c(city = 2, school = 2, class = 3, student = 4)
# n_list <- convertVectorToList(n)
# drawClusterStructure(n_list)