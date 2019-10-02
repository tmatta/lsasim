#' @title Summarizes clusters
#' @description Takes the output of `cluster_gen` and creates summary statistics of the questionnaire variables
#' @param data output of `cluster_gen`
#' @return list of summaries
#' @export
summarize_clusters <- function(data) {
    # Filtering out subject, ID and weight variables ===========================
    data_names <- lapply(data, function(x) sapply(x[1], names))
    detect_data_cols <- function(x) {
        grep("subject|ID|weight", x, invert = TRUE)
    }
    data_cols <- lapply(data_names, detect_data_cols)

    # Producing summary statistics =============================================
    # IDEA: customize this. Add proportions for Ws, intraclass correlations
    for (n in names(data)) {
        for (i in 1:length(data[[n]])) {
            message("Summary statistics for ", n, i)
            df <- data[[n]][[i]][data_cols[[n]]]
            print(summary(df))
            # browser()#TEMP
            # TODO: add polychoric correlation matrix
        }
        cli::cat_rule()
    }
}
