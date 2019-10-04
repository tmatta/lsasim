#' @title Sample from range
#' @description Creates a uniformly-distributed sample from a 2-length vector
#' @note This function was created primarily to be used to expand an object with the "range" class.
#' @param rg a "range"-class vector
#' @param sample_size the size of the sample to be generated
#' @param seed pseudo-random number generator seed
#' @return A vector containing the generated sample
sample_within_range <- function(rg, sample_size = NULL, seed = NULL) {
    check_condition(class(rg) != "range" | length(rg) != 2,
                    "Wrong class or size for rg object")
    minimum <- rg[1]
    maximum <- rg[2]
    sample_space <- seq(from = minimum, to = maximum)
    if (is.null(sample_size)) sample_size <- length(sample_space)
    if (!is.null(seed)) set.seed(seed)
    out <- sample(sample_space, sample_size, replace = TRUE)
    return(out)
}