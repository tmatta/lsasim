#' @title Generate replicates of a dataset using Balanced Repeated Replication
#' @param data dataset
#' @param k deflating weight factor. \eqn{0 \leq k \leq 1}.
#' @param pseudo_strata number of pseudo-strata
#' @param reps number of replicates
#' @param max_reps maximum number of replicates (only functional if `reps = NULL`)
#' @param weight_cols vector of weight columns
#' @param drop if `TRUE`, the observation that will not be part of the subsample is dropped from the dataset. Otherwise, it stays in the dataset but a new weight column is created to differentiate the selected observations
#' @param id_col number of column in dataset containing subject IDs. Set 0 to use the row names as ID
#' @return a list containing all the BRR replicates of `data`
#' @seealso jackknife
#' @note PISA uses the BRR Fay method with \eqn{k = 0.5}.
#' @references 
#' OECD (2015). Pisa Data Analysis Manual.
#' Adams, R., & Wu, M. (2002). PISA 2000 Technical Report. Paris: Organisation for Economic Co-operation and Development (OECD).
#' Rust, K. F., & Rao, J. N. K. (1996). Variance estimation for complex surveys using replication techniques. Statistical methods in medical research, 5(3), 283-310.
#' @export
brr <- function(data, k = 0, pseudo_strata = ceiling(nrow(data) / 2),
                reps = NULL, max_reps = 80,
                weight_cols = "none", id_col = 1, drop = TRUE) {
    # Verification =============================================================
    check_condition(k < 0 | k > 1, "k must be between 0 and 1")
    check_condition(
        !(class(data) %in% c("data.frame", "matrix")),
        "Input must be a data frame or a matrix"
    )
    check_condition(
        id_col == 1 & ncol(data) == 1,
        "data only has one column. You might want to set id_col = 0.",
        FALSE
    )
    if(class(data[, id_col]) %in% c("character", "factor")) {
        warning("id_col pointed to a non-numeric variable. ",
                "Proceeding with id_col = 0.")
        id_col <- 0
    }

    # Adding subject variable (easier than working with row names) =============
    drop_subject <- FALSE
    if (id_col == 0) {
        data$subject <- seq(nrow(data))
        id_col <- match("subject", names(data))
        drop_subject <- TRUE
    }

    # Determining the number of replicates =====================================
    if (is.null(reps)) { 
        total_replicates <- 4
        for (r in seq_len(pseudo_strata)) {
            if (r %% 4 == 0) total_replicates <- total_replicates + 4
        }
        total_replicates <- min(total_replicates, max_reps)
    } else {
        total_replicates <- reps
    }

    # Associating data to pseudo-strata ========================================
    if (nrow(data) %% 2 == 0) {
        expanded_pseudo_strata <- rep(seq_len(pseudo_strata), each = 2)
        data$pseudo_stratum <- expanded_pseudo_strata
    } else {
        pseudo_strata <- pseudo_strata - 1
        if (pseudo_strata == 0) {
            data$pseudo_stratum <- 1
            pseudo_strata <- 1
        } else {   
            expanded_pseudo_strata <- rep(seq_len(pseudo_strata), each = 2)
            data$pseudo_stratum <- c(expanded_pseudo_strata, pseudo_strata)
        }
    }

    # Generating replicates ====================================================
    R <- list()  # will restore replicate data
    for (rep in seq_len(total_replicates)) {
        replicate <- data
        # if (!drop) replicate$replicate_weight <- 2 - k
        for (p in seq_len(pseudo_strata)) {
            data_pseudo_stratum <- data[data$pseudo_stratum == p, ]
            chosen_one <- sample(data_pseudo_stratum[, id_col], size = 1)
            if (drop) {
                replicate <- replicate[replicate[id_col] != chosen_one, ]
            } else {
                group_size <- nrow(data_pseudo_stratum)
                chosen_row <- replicate["pseudo_stratum"] == p &
                    replicate[id_col] == chosen_one
                not_chosen_row <- replicate["pseudo_stratum"] == p &
                    replicate[id_col] != chosen_one
                if (group_size == 3) {
                    adjusted_k <- 1 - (-1 / 2 / sqrt(2))
                    replicate$replicate_weight[chosen_row] <- 3 - adjusted_k * 2
                    replicate$replicate_weight[not_chosen_row] <- adjusted_k
                } else if (group_size == 2) {
                    replicate$replicate_weight[chosen_row] <- k
                    replicate$replicate_weight[not_chosen_row] <- 2 - k
                } else if (group_size == 1) {
                    replicate$replicate_weight[chosen_row] <- 2 - k
                }
            }
        }
        if (weight_cols[1] != "none") {
            adj_factor <- 2
            replicate[weight_cols] <- replicate[weight_cols] * adj_factor
        }
        if (drop_subject) replicate$subject <- NULL
        replicate -> R[[paste0("R", rep)]]
    }
    return(R)
}