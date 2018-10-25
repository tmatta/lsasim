#' Generates cat_prop for questionnaire_gen
#'
#' @param n_X number of continuous variables
#' @param n_W number of categorical variables
#' @param n_cat_W number of categories per categorical variable
gen_cat_prop <- function(n_X, n_W, n_cat_W) {
  n_cat_X <- rep(1, n_X)
  if (is.null(n_cat_W)) {
    # -1 accounts for the last category always being equal to 1
    n_cat_W <- sample(c(2, 3, 4, 5) - 1, size = n_W, replace = TRUE)
  } else {
    n_cat_W <- n_cat_W - 1
  }
  cat_prop <- c(lapply(n_cat_X, function(x) x),
                lapply(n_cat_W,
                       function(x) c(sort(sample(seq(.2, .8, .1), x)), 1)))
  return(cat_prop)
}
