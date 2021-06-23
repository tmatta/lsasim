#' @title Customize Summary
#' @seealso summary ?lsasim:::summary.lsasimcluster
#' @param df_summary dataframe containing summary statistics
#' @param df original data frame
#' @param numeric_cols indices of the numeric columns
#' @param factor_cols indices of the factor columns
#' @param digits controls the number of digits in the output
#' @description Adds standard deviations and removes quantiles from a `summary()` output
#' @importFrom stats sd
customize_summary <- function(
    df_summary, df, numeric_cols, factor_cols, digits = 3
) {
    # Addind standard deviations ===============================================
    stdevs <- sapply(df[numeric_cols], sd)
    stdevs_txt <- c(
        paste("Stddev.:", round(stdevs, digits)), rep("", sum(factor_cols))
    )
    df_table <- rbind(df_summary, stdevs_txt)
    rownames(df_table)[7] <- ""

    # Removing quantiles, adding prop tables ===================================
    for (col in seq(ncol(df_table))) {
        is_numeric <- numeric_cols[col]
        is_factor <- factor_cols[col]
        if (is_numeric) {

            ## Remove quantiles
            # Cols 2, 3, 5 contain Q1, Q2 and Q3 (to be erased)
            df_table[c(2, 3, 5), col] <- NA
            # Cols 4, 6, 7 contain Mean, Max and SD (to be moved)
            df_table[c(4, 6, 7), col] -> df_table[c(2, 3, 5), col]
            df_table[c(4, 6, 7), col] <- NA
        } else if (is_factor) {

            ## Add prop tables
            prop_table <- round(prop.table(table(df[, col])), digits)
            prop_table_str <- paste(names(prop_table), prop_table, sep = ":")
            prop_table_str <- c("Prop.", prop_table_str)
            n_cats <- length(prop_table) + 1
            fits <- nrow(df_table) >= 2 * n_cats + 2
            while (!fits) {
                df_table <- rbind(df_table, NA)
                fits <- nrow(df_table) >= 2 * n_cats + 2
            }
            df_table[(n_cats + 1):(2 * n_cats), col] <- prop_table_str
        }
    }

    # Returning table ==========================================================
    return(as.table(df_table))
}