#' @title Generate replicates of a dataset using Jackknife
#' @param data dataset
#' @param weight_cols vector of weight columns
#' @param drop if `TRUE`, the observation that will not be part of the subsample is dropped from the dataset. Otherwise, it stays in the dataset but a new weight column is created to differentiate the selected observations
#' @return a list containing all the Jackknife replicates of `data`
#' @seealso brr
#' @examples
#' x <- data.frame(
#'     number = 1:5,
#'     letter = LETTERS[1:5],
#'     stringsAsFactors = FALSE
#' )
#' jackknife(x)
#' jackknife(x, drop = FALSE)
#' @export
jackknife <- function(data, weight_cols = "none", drop = TRUE) {
    check_condition(
        !(class(data) %in% c("data.frame", "matrix")),
        "Input must be a data frame or a matrix"
    )
    n_PSU <- nrow(data)
    R <- list() # replicate data
    for (rep in seq(n_PSU)) {
        if (drop) {
            replicate <- data[-rep, ]
        } else {
            replicate <- data
            replicate$replicate_weight <- 1
            replicate$replicate_weight[rep] <- 0
        }
        if (weight_cols[1] != "none") {
            adj_factor <- n_PSU / (n_PSU - 1)
            replicate[weight_cols] <- replicate[weight_cols] * adj_factor
        }
        replicate -> R[[paste0("R", rep)]]
    }
    return(R)
}