#' @title Generate replicates of a dataset using Balanced Repeated Replication
#' @param data dataset
#' @param pseudo_strata number of pseudo-strata
#' @param weight_cols vector of weight columns
#' @return a list containing all the BRR replicates of `data`
#' @seealso cluster_estimates jackknife
#' @export
brr <- function(data, k = 0, pseudo_strata = ceiling(nrow(data) / 2),
                weight_cols = "none") {
    # Verification =============================================================
    if (k < 0 | k > 1) stop ("k must be between 0 and 1")

    # Determining the number of replicates =====================================
    total_replicates <- 4
    for (r in seq_len(pseudo_strata)) {
        if (r %% 4 == 0) total_replicates <- total_replicates + 4
    }

    # Associating data to pseudo-strata ========================================
    data$pseudo_stratum <- rep(seq_len(pseudo_strata), each = 2)

    # Generating replicates ====================================================
    R <- list()  # will restore replicate data
    for (rep in seq_len(total_replicates)) {
        replicate <- data
        for (p in seq_len(pseudo_strata)) {
            data_pseudo_stratum <- data[data$pseudo_stratum == p, ]
            chosen_one <- sample(data_pseudo_stratum$subject, size = 1)
            replicate <- replicate[replicate$subject != chosen_one, ]
        }
        if (weight_cols[1] != "none") {
            adj_factor <- 2
            replicate[weight_cols] <- replicate[weight_cols] * adj_factor
        }
        replicate -> R[[paste0("R", rep)]]
    }
    return(R)
}