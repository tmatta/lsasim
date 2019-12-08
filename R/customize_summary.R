#' @title Customize Summary
#' @seealso summary summarize_clusters
#' @param df_summary dataframe containing summary statistics
#' @param df original data frame
#' @param numeric_cols indices of the numeric columns
#' @param factor_cols indices of the factor columns
#' @description Adds standard deviations and removes quantiles from a `summary()` output
#' @importFrom stats sd
customize_summary <- function(df_summary, df, numeric_cols, factor_cols) {
    stdevs <- sapply(df[numeric_cols], sd)
    stdevs_txt <- c(
        paste("Stddev.:", round(stdevs, 2)), rep("", sum(factor_cols))
    )
    df_table <- rbind(df_summary, stdevs_txt)
    rownames(df_table)[7] <- ""
    # TODO: remove quantiles
    return(as.table(df_table))
}