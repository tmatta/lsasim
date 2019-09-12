#' @title Sample from population structure
#' @description Generates a sample from a population structure
sampleFrom <- function (N, n, labels = names(N))
{
  # Creating basic elements ====================================================
  if (class(N) != "list") N <- convertVectorToList(N)
  unit_labels <- labelRespondents(N, labels)
  sampled_units <- unit_labels

  # Sampling elements until second-to-last level ===============================
  for (l in seq(from = 1, to = length(n) - 1)) {
    subunit_labels <- unique(unit_labels[, l])
    selected_subunit <- sample(x = subunit_labels, size = n[l])
    selected_rows <- sampled_units[, l] %in% selected_subunit
    sampled_units <- sampled_units[selected_rows, , drop = FALSE]
  }

  # Sampling from last level ===================================================
  browser()#TEMP
  # lapply(sampled_units, 1, function(x) sample(n[length(n))
  drawClusterStructure(N, labels)
  drawClusterStructure(N, labels, output = "text")
}