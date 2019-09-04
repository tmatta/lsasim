#' @title Jackknife estimate of sampling variance of the mean
#' @description Estimates the mean variance for Jackknife replications
#' @param data_whole entire dataset (pre-Jackknife)
#' @param data_rep Jackknife list with replications of `data_whole`
#' @param stat statistic of interest to calculate (must be a base R function)
#' @param theta vector containing the variables of interest
#' @param full_output if `TRUE`, returns all intermediate objects created
#' @details `data_rep` can be obtained from 
#' @seealso jackknife
#' @export
jackknife_var <- function(data_whole, data_rep, stat = mean, theta = NULL,
                          full_output = FALSE) {
    statistic <- match.fun(stat)

    # Determining which variables to use
    if (is.null(theta)) theta <- names(data_whole)  # uses all the variables
    # Dropping categorical and nominal variables
    theta_classes <- sapply(data_whole[theta], class)
    numeric_cols <- theta[grepl("numeric|integer", theta_classes)]
    numeric_data_whole <- data_whole[numeric_cols]
    numeric_data_rep <- lapply(data_rep, function(x) x[names(numeric_data_whole)])

    # Calculating mean variance
    theta_whole <- sapply(numeric_data_whole, statistic)
    theta_rep <- t(sapply(numeric_data_rep, function(x) apply(x, 2, statistic)))
    # rownames(theta_rep) <- numeric_cols
    n <- length(theta_rep)
    sigma2_jack <- vector()
    for (col in numeric_cols) {
        if (nrow(theta_rep) == 1) {
            theta_i <- theta_rep
        } else {
            theta_i <- theta_rep[, col]
        }
        new_s2 <- (n - 1) / n * sum((theta_i - theta_whole[col]) ^ 2)
        sigma2_jack <- c(sigma2_jack, new_s2)
    }

    # Building output
    if (full_output) {
        out <- mget(ls())
    } else {
        out <- sigma2_jack
    }
    return(out)
}