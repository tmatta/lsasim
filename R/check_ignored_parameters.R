#' Checks if provided parameters are ignored
#'
#' Internal function to match non-null parameters with a vector of ignored
#' parameters
#' @param provided_parameters vector of provided parameters
#' @param ignored_parameters vector of ignored parameters
#'
#' @return Warning message listing ignored parameters
check_ignored_parameters <- function(provided_parameters, ignored_parameters) {
  provided_but_ignored_parameters <-
    provided_parameters[match(ignored_parameters, provided_parameters,
                              nomatch = 0)]
  if (length(provided_but_ignored_parameters) > 0) {
    if (!is.na(provided_but_ignored_parameters)) {
      warning("Ignored parameters: ",
              paste(provided_but_ignored_parameters, collapse = ", "))
    }
  }
}
