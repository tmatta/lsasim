#' @title Sample from population structure
#' @description Generates a sample from a population structure
#' @param N list containing the population sampling structure
sample_from <- function (N, n, labels = names(N), verbose = TRUE)
{
  # Creating basic elements ====================================================
  if (class(N) != "list") N <- convert_vector_to_list(N)
  unit_labels <- label_respondents(N, labels)
  sampled_units <- unit_labels

  # Sampling elements until second-to-last level ===============================
  for (l in seq(from = 1, to = length(n) - 1)) {
    subunit_labels <- unique(unit_labels[, l])
    selected_subunit <- sample(x = subunit_labels, size = n[l])
    selected_rows <- sampled_units[, l] %in% selected_subunit
    sampled_units <- sampled_units[selected_rows, , drop = FALSE]
  }

  # Sampling setup =============================================================
  # TODO: sample from top to bottom
  # TODO: start by selecting elements_1st_lvl
  # TODO: then select second level based on results from 1st. Then rinse and repeat until last level.
  # lapply(sampled_units, 1, function(x) sample(n[length(n))
  if (verbose) message("Sampling from population")
  elements <- draw_cluster_structure(N, labels, output = "text")

  # Sampling from the last level ===============================================
  sampled_elements <- 

  # Retrieving elements from the last level ====================================
  elements_last_lvl <- NULL
  for (i in seq_along(elements)) {
    is_last_lvl <- grepl("\\(", elements[i])
    elements_last_lvl <- append(elements_last_lvl, is_last_lvl)
  }
  elements_last_lvl <- elements[elements_last_lvl]

  # Cleaning up elements from last level =======================================
  elements_last_lvl <- gsub("\\s.+", "", elements_last_lvl)
  elements_last_lvl <- gsub("\\W", "", elements_last_lvl)

  # Sampling from second-to-last level =========================================
  sampled_elements <- append(sample(x = elements_last_lvl,
                                    size = prod(n[1]:n[length(n) - 1])),
                             sampled_elements)

  browser()#TEMP
}