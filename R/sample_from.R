#' @title Sample from population structure
#' @description Generates a sample from a population structure
#' @param N list containing the population sampling structure
#' @param n numeric vector with the number of sampled observations (clusters or subjects) on each level
#' @param labels character vector with the names of the questionnaire respondents on each level
#' @param verbose if `TRUE`, prints output messages
#' @importFrom stats complete.cases
#' @export
sample_from <- function (N, n, labels = names(N), verbose = TRUE) {
  # Basic elements and validation
  N_matrix <- label_respondents(N, add_last_level = TRUE, apply_labels = FALSE)
  check_condition(
    ncol(N_matrix) != length(n),
    "length(n) must match the number of levels of N"
  )

  # Sampling from top level
  id_n <- sample(N[[1]], min(n[1], N[[1]]))
  n_matrix <- N_matrix[N_matrix[, 1] %in% id_n, ]

  # Sampling from next levels (before last, though)
  n_lvl <- ifelse(length(n) > 2, 2:(length(n) - 1), 2)
  for (lvl in n_lvl) {
    parent_id <- apply(
      X      = n_matrix[, 1:(lvl - 1), drop = FALSE],
      MARGIN = 1,
      FUN    = function(x) as.numeric(paste(x, collapse = ""))
    )
    n_submatrix <- n_matrix * NA
    for (parent in unique(parent_id)) {
      N_submatrix <- n_matrix[n_matrix[, lvl - 1] == parent, ]
      id_n <- sample(rownames(N_submatrix), min(n[lvl], nrow(N_submatrix)))
      n_submatrix[id_n, ] <- N_submatrix[rownames(N_submatrix) %in% id_n, ]
    }
  }

  # Sampling from final level
  n_submatrix[, length(n)] <- sapply(X   = n_submatrix[, length(n)],
                                     FUN = function(x) min(x, n[length(n)]))

  # Dropping NAs
  n_submatrix <- n_submatrix[complete.cases(n_submatrix), ]

  return(n_submatrix)
}