#' @title Check if List is Valid
#' @description Checks if a list has a proper structure to be transformed into a hierarchical structure
#' @param n list
#' @return Error if the structure is improper. Otherwise, there's no output.
#' @seealso check_condition
check_valid_structure <- function(n)
{
    for (l in seq(length(n) - 1)) {
    check_condition(
      length(n[[l + 1]]) != sum(n[[l]]),
      paste0(
        "Invalid cluster structure on level ", l + 1,  # TODO: add (school)
        ".\nThat level should have ", sum(n[[l]]),
        " elements, but it has ", length(n[[l + 1]]),
        ".\nPlease refer to documentation if necessary."
      )
    )
  }
}