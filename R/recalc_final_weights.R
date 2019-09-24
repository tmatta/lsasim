#' @title Recalculate final weights
#' @description Recalculate final weights given the replicate weights
#' @param data dataset
#' @param w_cols columns containing the weights
#' @param replicate_weight scalar with the replicate weights
#' @param reorder if `TRUE`, reorders the dataset so that the replicate weights appear before the final weights
#' @return input data with recalculated final weights, incorporating the replicate weights
#' @export
recalc_final_weights <- function(data, w_cols, replicate_weight = 1,
                                 reorder = TRUE)
{
    # Separating weight column by type =========================================
    if (missing(w_cols)) w_cols <- names(data)[grep("weight", names(data))]
    data$replicate.weight <- replicate_weight
    new_w_cols <- names(data)[grep("weight", names(data))]
    w_cols_final <- w_cols[length(w_cols)]
    w_cols_inter <- new_w_cols[new_w_cols != w_cols_final]

    # Recalculating final weights ==============================================
    data[w_cols_final] <- apply(data[w_cols_inter], 1, prod)

    # Reordering columns =======================================================
    if (reorder) {
        reordered_w_cols <- c(w_cols_inter, w_cols_final)
        id_col <- names(data)[length(names(data)) - 1]
        data_cols <- !(names(data) %in% c(reordered_w_cols, id_col))
        final_order <- c(names(data)[data_cols], reordered_w_cols, id_col)
        data <- data[final_order]
    }
    
    return(data)
}