#' Split variables in cat_prop
#'
#' @param cat_prop list corresponding to \code{cat_prop} from
#'   \code{questionnaire_gen}
#' @param keepYX if \code{TRUE}, output will be a list separating cat_prop_YX
#'   and cat_prop_W. IF \code{FALSE}, it will be a list with these objects
#'   combined (just like \code{cat_prop})
split_cat_prop <- function(cat_prop, keepYX = FALSE) {
  cat_prop_YX <- cat_prop[lapply(cat_prop, length) == 1]
  cat_prop_W <- cat_prop[lapply(cat_prop, length) > 1]
  cat_prop_W_p <- lapply(cat_prop_W, function(x) c(x[1], diff(x)))
  cat_prop_W_p_matrix <- cbind(unlist(cat_prop_W_p), 1)
  cat_prop_W <- split(cat_prop_W_p_matrix, seq(nrow(cat_prop_W_p_matrix)))
  cat_prop <- c(cat_prop_YX, cat_prop_W)
  if (keepYX) out <- list(cat_prop_YX, cat_prop_W) else out <- cat_prop
  return(out)
}
