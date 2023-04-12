#' @title Check class of n or N
#' @seealso cluster_gen
#' @description Check the class of an object (usually n and N from `cluster_gen`)
#' @param x either n or N from `cluster_gen`
#' @note This function is primarily used as a way to simplify the classification of n and N in the `cluster_gen` function.
check_n_N_class <- function(x)
{
    if (is(x, "select")) {
        class_x <- "select"
    } else {
        if (length(x) == 1) {
            class_x <- "multiplier"
        } else if (mode(x) == "numeric") {   # catches c() (numeric) and : (integer)
            class_x <- "vector"
        } else {
            class_x <- "list"
            if (any(sapply(x, class) == "range")) {
                class_x <- paste(class_x, "with ranges")
            } else {
                class_x <- paste(class_x, "without ranges")
            }
        }
    }
    return(class_x)
}
