#' @title Sampling variance of the mean for replications
#' @description Estimates the mean variance for Jackknife, BRR and BRR Fay replication methods
#' @param data_whole full, original dataset (the one that generated the replications)
#' @param data_rep list with replications of `data_whole`
#' @param method replication method. Can be "Jackknife", "BRR" or "BRR Fay"
#' @param k deflating weight factor (used only when `method = "BRR Fay")
#' @param stat statistic of interest to calculate (must be a base R function)
#' @param theta vector containing the variables of interest
#' @param full_output if `TRUE`, returns all intermediate objects created
#' @details `data_rep` can be obtained from 
#' @seealso jackknife brr
#' @export
replicate_var <- function(data_whole, data_rep, method, k = .5, stat = mean,
                          theta = NULL, full_output = FALSE)
{
    # Verification =============================================================
    if (k < 0 | k > 1) stop ("k must be between 0 and 1")

    # Fetching statistic of interest ===========================================
    statistic <- match.fun(stat)

    # Converting data to data frame, if necessary ==============================
    if (class(data_whole) == "matrix") data_whole <- data.frame(data_whole)
    if (any(sapply(data_rep, class) == "matrix")) {
        data_rep <- lapply(data_rep, data.frame)
    }

    # Determining which variables to use =======================================
    if (is.null(theta)) theta <- names(data_whole)  # uses all variables
    
    # Dropping categorical and nominal variables ===============================
    theta_classes <- sapply(data_whole[theta], class)
    numeric_cols <- theta[grepl("numeric|integer", theta_classes)]
    numeric_data_whole <- data_whole[numeric_cols]
    numeric_data_rep <- lapply(data_rep, function(x) x[names(numeric_data_whole)])

    # Calculating theta for the whole dataset and for each replication
    theta_whole <- sapply(numeric_data_whole, statistic)
    theta_rep <- t(sapply(numeric_data_rep, function(x) apply(x, 2, statistic)))

    # Calculating mean variance ================================================
    G <- length(theta_rep)
    sigma2 <- vector()
    for (col in numeric_cols) {

        # Retrieving theta_i from theta_rep ------------------------------------
        if (nrow(theta_rep) == 1) {
            theta_i <- theta_rep
        } else {
            theta_i <- theta_rep[, col]
        }

        # Defining variance multiplier -----------------------------------------
        if (method == "Jackknife") {
            multiplier <- (G - 1) / G
        } else if (method == "BRR") {
            multiplier <- 1 / G
        } else if (method == "BRR Fay") {
            multiplier <- 1 / (G * (1 - k) ^ 2)
        } else {
            stop("Invalid method. Please use 'Jackknife', 'BRR' or 'BRR Fay'.")
        }

        # Calculating variance and adding to output object ---------------------
        new_s2 <- multiplier * sum((theta_i - theta_whole[col]) ^ 2)
        sigma2 <- c(sigma2, new_s2)
    }

    # Building output ==========================================================
    if (full_output) {
        out <- mget(ls())
    } else {
        out <- sigma2
    }
    return(out)
}