#' @title Trim sample
#' @description Makes sure n <= N
#' @param n vector or unranged list corresponding to sample structure
#' @param N vector or unranged list corresponding to population structure
#' @seealso cluster_gen
#' @export
trim_sample <- function(n, N) {
    # Transforming variables ===================================================
    class_n <- check_n_N_class(n)
    class_N <- check_n_N_class(N)
    if (class_N != "list without ranges") {
        N <- convert_vector_to_list(N)
    }
    if (class_n != "list without ranges") {
        n <- convert_vector_to_list(n, N)
    }

    # Restructuring variables ==================================================
    n_lab <- label_respondents(n, add_last_level = TRUE, apply_labels = FALSE)
    N_lab <- label_respondents(N, add_last_level = TRUE, apply_labels = FALSE)
    return(n)
}