#' @title Whitelist message
#' @description Prints out the sampled elements when cluster_gen is called with select. This function is analogous to cluster_message, but is more proper for random sampling.
#' @param w whitelist
whitelist_message <- function(w) {
    message("Sampled elements")
    last_element <- ncol(w)
    for (i in seq_len(nrow(w))) {
        # ======================================================================
        # Extracting PSU name
        # ======================================================================
        cluster_names <- paste0(colnames(w), w[i, ])[-last_element]
        cluster_names <- paste(cluster_names, collapse = "_")
        # ======================================================================
        # Adding final element
        # ======================================================================
        final_element <- w[last_element]
        final_name <- paste0(
            cluster_names,
            " (",
            final_element[i, ],
            " ",
            pluralize(colnames(w)[last_element], n = final_element[i, ]),
            ")"
        )
        cat(final_name, "\n")
    }
}