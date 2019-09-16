#' @title Sample from population structure
#' @description Generates a sample from a population structure
#' @param N list containing the population sampling structure
#' @param n numeric vector with the number of sampled observations (clusters or subjects) on each level
#' @param labels character vector with the names of the questionnaire respondents on each level
#' @param verbose if `TRUE`, prints output messages
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
  # lapply(sampled_units, 1, function(x) sample(n[length(n))
  if (verbose) message("Sampling from population")
  # TODO: add txtProgressBar or something...
  elements <- draw_cluster_structure(N, labels, output = "text")

  # Determining elements from the level =======================================
  sample_tree <- function(pop, level, grepl_filter, grepl_cleanup1,
                          grepl_cleanup2 = "")
  {
    elements_in_lvl <- NULL
    for (i in seq_along(pop)) {
      is_in_lvl <- grepl(grepl_filter, pop[i])
      elements_in_lvl <- append(elements_in_lvl, is_in_lvl)
    }
    elements_in_lvl <- pop[elements_in_lvl]

    # Cleaning up elements ------------------------
    elements_in_lvl <- gsub(grepl_cleanup1, "", elements_in_lvl)
    elements_in_lvl <- gsub(grepl_cleanup2, "", elements_in_lvl)

    # Sampling ------------------------
    sampled_elements <- sample(x = elements_in_lvl, size = n[level])
    elements_left <- pop[elements_in_lvl %in% sampled_elements]
    return(elements_left)
  }

  # Sampling from the top level ================================================
  el_top <- sample_tree(elements, 1, "^\\\033", "\\\033\\[\\d\\dm")

  # Sampling from the intermediate levels ======================================
  if (length(n) > 3) {
    # This only applies in cases where intermediate levels exist
    stop("Sampling not yet implemented for length(n) this large")
    el_lvl <- el_top
    for (l in 2:length(n) - 1) {
      # TODO: implement for intermediate levels
      # el_lvl <- sample(tree, el_lvl, 
    }
  }

  # Sampling from second-to-last level =========================================
  el_lvl <- sample_tree(el_top, n[length(n) - 1], "\\_", "\\s.+", "\\W")

  # Sampling from last level ===================================================
  el_lvl <- gsub("\\(\\d+", paste0("\\(", n[length(n)]), el_lvl)

  # Printing sampled elements ==================================================
  if (verbose) cat(el_lvl, sep = "\n")
}