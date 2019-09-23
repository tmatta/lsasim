#' @title Attribute Labels in Hierarchical Structure
#' @description Attributes cluster and respondent labels in the context of `cluster_gen`.
#' @seealso cluster_gen
#' @param n numeric vector or list
#' @return list containing appropriate labels for the clusters and their respondents
attribute_cluster_labels <- function(n)
{
    if (!is.null(names(n))) {
        cluster_labels <- names(n)
        resp_labels <- names(n)[-1]
    } else {
        # General lists for all level ranges up to 5 ---------------------------
        cluster_labels <- list(c("school"),
                               c("class", "school"),
                               c("class", "school", "state"),
                               c("class", "school", "city", "state"))
        resp_labels    <- list(c("student", "teacher"),
                               c("student", "teacher", "principal"),
                               c("student", "teacher", "principal", "mayor"),
                               c("student", "teacher", "principal", "governor"))

        # Filtering the labels above according to length(n) --------------------
        cluster_labels <- rev(cluster_labels[[length(n) - 1]])
        resp_labels <- rev(resp_labels[[length(n) - 1]])[-1]
    }
    return(list(cl = cluster_labels, resp = resp_labels))
}
