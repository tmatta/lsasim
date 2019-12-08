#' @title Sampling variance of the mean for replications
#' @description Estimates the mean variance for Jackknife, BRR and BRR Fay replication methods
#' @param data_whole full, original dataset (the one that generated the replications)
#' @param data_rep list with replications of `data_whole`
#' @param method replication method. Can be "Jackknife", "BRR" or "BRR Fay"
#' @param k deflating weight factor (used only when `method = "BRR Fay")
#' @param stat statistic of interest to calculate (must be a base R function)
#' @param vars vector containing the variables of interest
#' @param full_output if `TRUE`, returns all intermediate objects created
#' @param weight_var variables containing the weights
#' @details `data_rep` can be obtained from 
#' @seealso jackknife brr
#' @importFrom stats weighted.mean sd
#' @export
replicate_var <- function(data_whole, data_rep, method, k = 0,
                          weight_var = NULL,
                          stat = weighted.mean, vars = NULL,
                          full_output = FALSE) {
    # Verification =============================================================
    if (k < 0 | k > 1) stop("k must be between 0 and 1")
    if (method != "BRR Fay" & k != .5 & k != 0) {
        warning(method, " ignores k. Use 'BRR Fay' instead.")
    }
    if (method == "BRR Fay" & k == 0) {
        k <- .5
    }
    if (missing(data_rep) & class(data_whole) == "list") {
        stop("If data_rep is missing, ",
             "data_whole must be a data frame or a matrix")
    }
    if (missing(data_rep) & class(data_whole) != "list") {
        message("Generating replications and statistics with ", method)
        if (method %in% c("Jackknife", "jackknife")) {
            data_rep <- jackknife(data_whole, drop = FALSE)
        } else if (method == "BRR") {
            data_rep <- brr(data_whole, k = 0, drop = FALSE)
        } else if (method == "BRR Fay") {
            data_rep <- brr(data_whole, k = k, drop = FALSE)
        } else {
            stop("Invalid method. Please use 'Jackknife', 'BRR' or 'BRR Fay'.")
        }
        weight_var <- "replicate_weight"
    }


    # Fetching statistic of interest ===========================================
    statistic <- match.fun(stat)

    # Converting data to data frame, if necessary ==============================
    if (class(data_whole) == "matrix") data_whole <- data.frame(data_whole)
    if (any(sapply(data_rep, class) == "matrix")) {
        data_rep <- lapply(data_rep, data.frame)
    }

    # Determining which variables to use =======================================
    if (is.null(vars)) {
        # use all variables
        vars <- list(whole = names(data_whole), rep = names(data_rep[[1]]))
    } else {
        vars <- list(whole = vars, rep = c(vars, weight_var))
    }
    
    # Dropping categorical and nominal variables ===============================
    vars_classes <- list(whole = sapply(data_whole[vars$whole], class),
                          rep   = sapply(data_rep[[1]][vars$rep], class))
    numeric_cols <- list(
        whole = vars$whole[grepl("numeric|integer", vars_classes$whole)],
        rep   = vars$rep[grepl("numeric|integer", vars_classes$rep)]
    )
    numeric_data <- list(
        whole = data_whole[numeric_cols$whole],
        rep   = lapply(data_rep, function(x) x[numeric_cols$rep])
    )

    # Calculating theta for the whole dataset and for each replication =========
    theta_whole <- sapply(numeric_data$whole, statistic)
    statistic_rep <- function(x, w = weight_var) {
        if (is.null(w)) {
            out <- apply(x, 2, statistic)
        } else {
            x_without_w <- x[, -match(w, names(x)), drop = FALSE]
            w_col <- x[, w]
            out <- apply(x_without_w, 2, function(x) statistic(x, w_col))
        }
        return(out)
    }

    theta_rep <- t(sapply(numeric_data$rep, statistic_rep))

    # Defining variance multiplier =============================================
    G <- ifelse(nrow(theta_rep) > 1, nrow(theta_rep), length(theta_rep)) # n_rep
    if (method %in% c("Jackknife", "jackknife")) {
        multiplier <- (G - 1) / G
    } else if (method == "BRR") {
        multiplier <- 1 / G
    } else if (method == "BRR Fay") {
        multiplier <- 1 / (G * (1 - k) ^ 2)
    } else {
        stop("Invalid method. Please use 'Jackknife', 'BRR' or 'BRR Fay'.")
    }

    # Looping through variables to calculate mean variances ====================
    sigma2 <- vector()
    for (col in numeric_cols$whole) {

        # Retrieving theta_i from theta_rep ------------------------------------
        if (nrow(theta_rep) == 1) {
            theta_i <- theta_rep
        } else {
            theta_i <- theta_rep[, col]
        }

        # Calculating variance for this variable, adding to output object ------
        new_s2 <- multiplier * sum((theta_i - theta_whole[col]) ^ 2)
        sigma2 <- c(sigma2, new_s2)
    }
    names(sigma2) <- numeric_cols$whole

    # Building output ==========================================================
    if (full_output) {
        out <- mget(ls())
    } else {
        out <- sigma2
    }
    return(out)
}