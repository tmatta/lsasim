#' @title Summarizes clusters
#' @description Takes the output of `cluster_gen` and creates summary statistics of the questionnaire variables
#' @param data output of `cluster_gen`
#' @param digits controls the number of digits in the output (for `print = TRUE`)
#' @param print if `TRUE`, pretty-print a summary of statistics; otherwise, output statistics that will be useful for `intraclass_cor`.
#' @return list of summaries
#' @export
summarize_clusters <- function(data, digits = 2, print = TRUE) {
    # Filtering out subject, ID and weight variables ===========================
    detect_data_cols <- function(x) {
        grep("subject|ID|weight", x, invert = TRUE)
    }
    if (all(sapply(data, class) == "list")) {
        # Data was generated with separate_questionnaires = TRUE
        # data_names <- lapply(data, function(x) sapply(x[1], names))
    } else {
        # Data was generated with separate_questionnaires = FALSE
        data_list <- list(data)  # makes it behave like separate = TRUE
        names(data_list) <- names(data[1])
        data <- data_list
    }
    data_names <- lapply(data, function(x) sapply(x[1], names))
    data_cols <- lapply(data_names, detect_data_cols)

    # Producing summary statistics =============================================
    out <- list()
    for (n in names(data)) {
        for (i in 1:length(data[[n]])) {

            ## Calculating simple statistics for a cluster element i -----------
            df <- data[[n]][[i]][data_cols[[n]]]
            numeric_cols <- sapply(df, class) == "numeric"
            factor_cols <- sapply(df, class) == "factor"
            if (print) {
                message("Summary statistics for ", n, i)
                print(summary(df, digits = digits))
                cat("\n ")
                stdevs <- sapply(df[numeric_cols], sd)
                cat(paste("Std.dv.:", round(stdevs, digits = digits),
                          collapse = "   "), "\n")
                # TODO: align output of summary and sd
                for (w in names(df[factor_cols])) {
                    message("Statistics per category of ", w)
                    w_lvls <- levels(df[, w])
                    stats <- sapply(
                        w_lvls,
                        function(l) summary(df[df[, w] == l, numeric_cols])
                    )
                    colnames(stats) <- paste0(w, ".", w_lvls)
                    print(stats)
                }
                cli::cat_rule()
            } else {
                df_X <- df[numeric_cols]
                out[[n]]$y_bar_j <- rbind(out[[n]]$y_bar_j, colMeans(df_X))
                out[[n]]$n_j <- rbind(out[[n]]$n_j, nrow(df_X))
                out[[n]]$s2_j <- rbind(out[[n]]$s2_j, apply(df_X, 2, stats::var))
            }
        }

        ## Calculating summary statistics for cluster group n ------------------
        if (!print) {
            out[[n]]$y_bar <- apply(
                X      = out[[n]]$y_bar_j,
                MARGIN = 2, 
                FUN    = function(x) stats::weighted.mean(x, out[[n]]$n_j)
            )
            out[[n]]$N <- length(out[[n]]$n_j)
            out[[n]]$M <- sum(out[[n]]$n_j)
            calc_n_tilde <- function(M, N, n_j) {
                n_bar <- M / N
                s2_n_j <- sum((n_j - n_bar) ^ 2) / (N - 1)
                n_tilde <- n_bar - s2_n_j / (N * n_bar)
                return(n_tilde)
            }
            out[[n]]$n_tilde <- calc_n_tilde(out[[n]]$M, out[[n]]$N,
                                             out[[n]]$n_j)
        }
    }
    if (!print) return(out)
}