#' @title Generate replicates of a dataset using Jackknife
#' @param data dataset
#' @param weight_cols vector of weight columns
#' @return a list containing all the Jackknife replicates of `data`
#' @seealso cluster_estimates brr
#' @export
jackknife <- function(data, weight_cols = "none") {
    n_PSU <- nrow(data)
    R <- list()  # replicate data
    for (rep in seq(n_PSU)) {
        replicate <- data[-rep, ]
        if (weight_cols[1] != "none") {
            adj_factor <- n_PSU / (n_PSU - 1)
            replicate[weight_cols] <- replicate[weight_cols] * adj_factor
        }
        replicate -> R[[paste0("R", rep)]]
    }
    return(R)
}