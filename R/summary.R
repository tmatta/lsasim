#' @title Summarizes clusters
#' @description Takes the output of `cluster_gen` and creates summary statistics of the questionnaire variables
#' @param object output of `cluster_gen`
#' @param digits loosely controls the number of digits (significant or not) in the output (for `print = TRUE`)
#' @param print "all" will pretty-print a summary of statistics, "partial" will only print cluster-level sumamrizes; "none" outputs statistics as a list
#' @param print_hetcor if `TRUE` (default), prints the heterogeneous correlation matrix
#' @param force_matrix if `TRUE`, prints the heterogeneous correlation matrix even if warnings are generated
#' @param ... additional arguments (unused; added for compatibility with generic)
#' @note
#' Setting `print="none"` allows for saving the results as an R object (list). Otherwise, the results will be simply printed and not saveable.
#'
#' Changing `digits` may yield unexpected results for the estimates of continuous variables, given how most of them are printed using the number of significant digits (for more information, see `help("summary")`).
#'
#' Please note that datasets containing large values for the coefficient of variation (sigma / mu) should yield imprecise results.
#' @return list of summaries
#' @seealso anova.lsasimcluster
#' @examples
#' n <- c(3, 30)
#' cls <- cluster_gen(n, n_X = 3, n_W = 5)
#' summary(cls)
#' summary(cls, print="none") # allows saving results
#' @export
summary.lsasimcluster <- function(
    object, digits=4, print="partial", print_hetcor=TRUE, force_matrix=FALSE,
    ...
) {
    # Validation ===============================================================
    check_condition(
        condition = !(print %in% c("partial", "all", "none")),
        message = "Invalid print option. Defaulting to partial.",
        fatal = FALSE
    )
    # Wrap data in a list (for !separate_questionnaires) =======================
    data <- object
    if (all(sapply(data, function(d) !is(d, "list")))) {
        data <- list(data)
        names(data) <- gsub("[0-9]", "", names(data[[1]])[1])
    }

    # Filtering out subject, ID and weight variables ===========================
    detect_data_cols <- function(x) {
        grep("subject|ID|weight", x, invert = TRUE)
    }
    if (all(sapply(data, function(d) is(d, "list")))) {
        # Data was generated with separate_questionnaires = TRUE
        # data_names <- lapply(data, function(x) sapply(x[1], names))
    } else {
        # Data was generated with separate_questionnaires = FALSE
        data_list <- list(data)  # makes it behave like separate = TRUE
        names(data_list) <- names(data[1])
        data <- data_list
    }
    data_names <- lapply(data, function(x) sapply(x, names))
    if (is(sapply(data_names, class), "matrix")) {
        # PSUs have equal number of elements
        data_names <- lapply(data, function(x) sapply(x[1], names))
        data_cols <- lapply(data_names, detect_data_cols)
        equal_n <- TRUE
    } else {
        # PSUs have different number of elements
        data_cols <- lapply(data_names, function(x) lapply(x, detect_data_cols))
        equal_n <- FALSE
    }

    # Producing summary statistics =============================================
    out <- list()
    for (n in names(data)) {
        for (i in 1:length(data[[n]])) {

            ## Calculating simple statistics for a cluster element i -----------
            if (equal_n) {
                data_columns <- data_cols[[n]]
            } else {
                data_columns <- data_cols[[n]][[i]]
            }
            df <- data[[n]][[i]][data_columns]
            x_cols <- sapply(df, class) == "numeric"
            w_cols <- sapply(df, class) == "factor"
            if (print == "all") {
                message("Summary statistics for ", n, i)
                df_summary <- summary(df, digits = digits)
                df_table <- customize_summary(
                    df_summary, df, x_cols, w_cols, digits
                )
                print(df_table)

                # Adding correlation matrix
                if (print_hetcor) printHetcor(df, force_matrix)
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
                    stats_table <- as.table(stats_binded)
                    print(stats_table)
                }

                # Adding correlation matrix
                if (print_hetcor) printHetcor(df, force_matrix)
                cli::cat_rule()
            } else {
                if (any(x_cols)) {
                    df_X <- df[x_cols]
                    out[[n]]$y_bar_j <- rbind(out[[n]]$y_bar_j, colMeans(df_X))
                    out[[n]]$n_j <- rbind(out[[n]]$n_j, nrow(df_X))
                    out[[n]]$s2_j <- rbind(
                        out[[n]]$s2_j, apply(df_X, 2, stats::var)
                    )
                }
            }
        }

        ## Calculating summary statistics for cluster group n ------------------
        if (print == "none") {
            out[[n]]$y_bar <- apply(
                X      = out[[n]]$y_bar_j,
                MARGIN = 2,
                FUN    = function(x) {
                    stats::weighted.mean(x, out[[n]]$n_j)
                }
            )
            out[[n]]$N <- length(out[[n]]$n_j)
            out[[n]]$M <- sum(out[[n]]$n_j)
            out[[n]]$n_tilde <- calc_n_tilde(
                out[[n]]$M, out[[n]]$N, out[[n]]$n_j
            )
        }
    }


    # Printing aggregate level statistics or returning output ==================
    if (print != "none") {
        collapsed_data <- tryCatch(
            lapply(data, function(x) do.call(rbind, x)),
            error = function(e) {
                data
                # warning(
                #     "Cannot summarize any further. ",
                #     "PSUs have different numbers of random variables."
                # )
            }
        )
        cli::cat_rule()
        for (n in names(collapsed_data)) {
            message("Summary statistics for all ", pluralize(n))
            if (is(collapsed_data[[n]], "data.frame")) {
                df <- collapsed_data[n]
            } else {
                df <- collapsed_data[[n]]
            }
            df_names <- lapply(df, names)
            df_cols <- lapply(df_names, detect_data_cols)
            df <- lapply(seq_along(df), function(x) df[[x]][df_cols[[x]]])
            df_summary <- lapply(df, summary)
            x_cols <- lapply(df, function(x) sapply(x, class) == "numeric")
            w_cols <- lapply(df, function(x) sapply(x, class) == "factor")
            df_table <- lapply(
                seq_along(df_summary),
                function(x) {
                    customize_summary(
                        df_summary[[x]], df[[x]], x_cols[[x]], w_cols[[x]], digits
                    )
                }
            )
            print(df_table)

            # Adding correlation matrix
            if (print_hetcor) {
                lapply(
                    seq_along(df),
                    function(x) printHetcor(df[[x]], force_matrix)
                )
            }
        }
    } else {
        return(out)
    }
}

printHetcor <- function(df, force=FALSE) {
    tryCatch(
        expr = {
            hetcor_printed <- polycor::hetcor(df)$correlations
            message("\nHeterogeneous correlation matrix\n")
            print(hetcor_printed)
        },
        warning = function(w) {
            if (force) {
                message("\n Heterogeneous correlation matrix\n")
                print(polycor::hetcor(df)$correlations)
            } else {
                warning(
                    "Generation of a correlation matrix yielded warnings ",
                    "and has been suppressed. ",
                    "Set 'force_matrix=TRUE' to generate it anyway. ",
                    "Alternatively, set print_hetcor=FALSE to suppress the ",
                    "calculation of this matrix."
                )
            }
        }
    )
}
