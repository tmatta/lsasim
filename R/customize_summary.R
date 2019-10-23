#' @title Customize Summary
#' @seealso summary summarize_clusters
#' @description Adds standard deviations and removes quantiles from a `summary()` output
customize_summary <- function(df_summary, df, numeric_cols, factor_cols) {
    stdevs <- sapply(df[numeric_cols], sd)
    stdevs_txt <- c(
        paste("Stddev.:", round(stdevs, 2)), rep("", sum(factor_cols))
    )
    # browser()#TEMP
    df_table <- rbind(df_summary, stdevs_txt)
    rownames(df_table)[7] <- ""
    # TODO: remove quantiles
    return(as.table(df_table))
}