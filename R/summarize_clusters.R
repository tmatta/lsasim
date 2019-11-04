#' @title Summarizes clusters
#' @description Takes the output of `cluster_gen` and creates summary statistics of the questionnaire variables
#' @param data output of `cluster_gen`
#' @param digits controls the number of digits in the output (for `print = TRUE`)
#' @param print "all" will pretty-print a summary of statistics, "partial" will only print cluster-level sumamrizes; "none" outputs statistics as a list
#' @return list of summaries
#' @seealso anova_table
#' @export
summarize_clusters <- function(data, digits = 2, print = "partial") {
    # Wrap data in a list (for !separate_questionnaires) =======================
    if (all(sapply(data, class) != "list")) {
        data <- list(data)
        names(data) <- gsub("[0-9]", "", names(data[[1]])[1])
    }
    
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
            x_cols <- sapply(df, class) == "numeric"
            w_cols <- sapply(df, class) == "factor"
            if (print == "all") {
                message("Summary statistics for ", n, i)
                df_summary <- summary(df, digits = digits)
                df_table <- customize_summary(df_summary, df, x_cols, w_cols)
                print(df_table)
                for (w in names(df[w_cols])) {
                    message("\nStatistics per category of ", w)
                    w_lvls <- levels(df[, w])
                    x_names <- names(x_cols[x_cols])
                    stats <- lapply(
                        w_lvls,
                        function(l) summary(df[df[, w] == l, x_cols])
                    )
                    names(stats) <- paste0(w, ".", w_lvls)
                    stats_binded <- do.call(cbind, stats)
                    colnames(stats_binded) <- paste(x_names, rep(names(stats), each = length(x_names)), sep = " for ")
                    stats_binded <- stats_binded[, sort(colnames(stats_binded))]
                    # TODO: call customize_summary()
                    stats_table <- as.table(stats_binded)
                    print(stats_table)
                }
                cli::cat_rule()
            } else {
                df_X <- df[x_cols]
                out[[n]]$y_bar_j <- rbind(out[[n]]$y_bar_j, colMeans(df_X))
                out[[n]]$n_j <- rbind(out[[n]]$n_j, nrow(df_X))
                out[[n]]$s2_j <- rbind(out[[n]]$s2_j, apply(df_X, 2, stats::var))
            }
        }

        ## Calculating summary statistics for cluster group n ------------------
        if (print == "none") {
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
    
    # Printing aggregate level statistics or returning output ==================
    if (print != "none") {
        collapsed_data <- lapply(data, function(x) do.call(rbind, x))
        cli::cat_rule()
        for (n in names(collapsed_data)) {
            message("Summary statistics for all ", pluralize(n))
            df <- collapsed_data[[n]][names(x_cols)]
            df_summary <- summary(df)
            df_table <- customize_summary(df_summary, df, x_cols, w_cols)
            print(df_table)
        }
    } else {
        return(out)
    }
}