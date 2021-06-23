#' @title Pluralize words
#' @description Pluralize a word
#' @param word vector of characters to be pluralized
#' @param n vector of number of times each word appears (to determine if the plural or single form will be returned)
#' @return `word`, either pluralized or not (depending on `n`)
pluralize <- function(word, n = rep(2, length(word)))
{
  # Define basic dictionaty ====================================================
  singular <- c(
    "country", "region", "state", "city", "neighborhood", "school",
    "class", "teacher", "student", "principal", "people"
  )
  plural <- c(
    "countries", "regions", "states", "cities", "neighborhoods", "schools",
    "classes", "teachers", "students", "principals", "people"
  )

  # Pluralize if necessary =====================================================
  out <- NULL
  for (w in word) {
    position <- match(w, singular)
    position_n <- match(w, word)
    if (n[position_n] > 1) {
      # Deciding between a known plural or a generic one (just adding "s")
      new_w <- ifelse(test = is.na(position) | n[position_n] == 1,
                      yes  = paste0(w, "s"),
                      no   = plural[position])
    } else {
      new_w <- w
    }
    out <- append(out, new_w)
  }

  return(out)
}