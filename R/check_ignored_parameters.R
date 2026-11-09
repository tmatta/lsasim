#' Checks if provided parameters are ignored
#'
#' Internal function to match non-null parameters with a vector of ignored
#' parameters
#' @param provided_parameters vector of provided parameters
#' @param ignored_parameters vector of ignored parameters
#'
#' @return Warning message listing ignored parameters
check_ignored_parameters <- function(provided_parameters, ignored_parameters) {
  ignored_provided_parameters <-
    provided_parameters[match(ignored_parameters, provided_parameters,
      nomatch = 0
    )]
  if (length(ignored_provided_parameters) > 0) {
    if (!is.na(ignored_provided_parameters)) {
      warning(
        "Ignored parameters: ",
        paste(ignored_provided_parameters, collapse = ", ")
      )
    }
  }
}
