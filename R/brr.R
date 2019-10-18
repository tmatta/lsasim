#' @title Generate replicates of a dataset using Balanced Repeated Replication
#' @param data dataset
#' @param k deflating weight factor. \eqn{0 \leq k \leq 1}.
#' @param pseudo_strata number of pseudo-strata
#' @param weight_cols vector of weight columns
#' @param drop if `TRUE`, the observation that will not be part of the subsample is dropped from the dataset. Otherwise, it stays in the dataset but a new weight column is created to differentiate the selected observations
#' @return a list containing all the BRR replicates of `data`
#' @seealso jackknife
#' @note PISA uses the BRR Fay method with \eqn{k = 0.5}.
#' @references 
#' OECD (2015). Pisa Data Analysis Manual.
#' Rust, K. F., & Rao, J. N. K. (1996). Variance estimation for complex surveys using replication techniques. Statistical methods in medical research, 5(3), 283-310.
#' @export
brr <- function(data, k = 0, pseudo_strata = ceiling(nrow(data) / 2),
                weight_cols = "none", drop = TRUE) {
    # Verification =============================================================
    check_condition(k < 0 | k > 1, "k must be between 0 and 1")
    check_condition(
        !(class(data) %in% c("data.frame", "matrix")),
        "Input must be a data frame or a matrix"
    )

    # Determining the number of replicates =====================================
    total_replicates <- 4
    for (r in seq_len(pseudo_strata)) {
        if (r %% 4 == 0) total_replicates <- total_replicates + 4
    }

    # Associating data to pseudo-strata ========================================
    data$pseudo_stratum <- rep(seq_len(pseudo_strata), each = 2)[1:nrow(data)]
    # TODO: improve handling of odd-numbered datasets (don't always include the lasst one!)

    # Generating replicates ====================================================
    R <- list()  # will restore replicate data
    for (rep in seq_len(total_replicates)) {
        replicate <- data
        if (!drop) replicate$replicate_weight <- 2 - k
        for (p in seq_len(pseudo_strata)) {
            data_pseudo_stratum <- data[data$pseudo_stratum == p, ]
            chosen_one <- sample(data_pseudo_stratum$subject, size = 1)
            if (drop) {
                replicate <- replicate[replicate$subject != chosen_one, ]
            } else {
                replicate$replicate_weight[replicate$subject == chosen_one] <- k
            }
        }
        if (weight_cols[1] != "none") {
            adj_factor <- 2
            replicate[weight_cols] <- replicate[weight_cols] * adj_factor
        }
        replicate -> R[[paste0("R", rep)]]
    }
    return(R)
}