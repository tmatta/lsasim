#' @title Dataset summary statistics
#' @description Creates summary statistics of a dataset
#' @param data Data frame
#' @param digits number of digits for the output
#' @note This function is inspired by base::summary(), but outputs content more relevant to the context of cluster_gen() and summary()
#' @seealso summary()
summary_2 <- function(data, digits = 3) {
    # Basic elements
    var_type <- sapply(data, class)

    # Looping through data variables
    for (x in names(data)) {
        if (var_type[x] %in% c("numeric", "integer")) {
            x_min <- formatC(min(data[, x]), format = "f", digits = digits)
            x_avg <- formatC(mean(data[, x]), format = "f", digits = digits)
            x_max <- formatC(max(data[, x]), format = "f", digits = digits)
            x_std <- formatC(sd(data[, x]), format = "f", digits = digits)
            cat(
                " Min    :", x_min, "\n",
                "Mean   :", x_avg, "\n",
                "Max    :", x_max, "\n",
                "\n",
                "Stdev. :", x_min, "\n\n"
            )
        } else if (var_type[x] == "factor") {
            print(table(data[x]))
        }
    }
}