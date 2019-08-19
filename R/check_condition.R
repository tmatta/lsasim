#' Check if an error condition is satisfied
#'
#' @param condition logical test which if \code{TRUE} will cause the function to
#'   return an error message
#' @param message error message to be displayed if condition is met.
#' @param fatal if \code{TRUE}, error message is fatal, i.e., it will abort the
#'   parent function which called \code{check_condition}.
#'
check_condition <- function(condition, message, fatal = TRUE) {
  if (length(condition) > 0) {
    if (condition) {
      if (fatal) stop(message, call. = FALSE)
      else warning(message, call. = FALSE)
    }
  }
}
