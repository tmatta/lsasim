#' @title Convert Vector to Expanded List
#' @description Converts a vector to list where each element is replicated a certain number of times depending on the previous vector. Also work for ranged lists
#' @param x vector or ranged list to be converted
#' @param x_max reference vector or ranged list with max values for x
#' @param verbose if `TRUE`, sends messages to user about what's being done
#' @return expanded/replicated version of x
#' @export
convert_vector_to_list <- function(x, x_max = x, verbose = TRUE) {
  # Defining basic elements ====================================================
  x_list <- as.list(x)
  x_max_list <- as.list(x_max)

  # Defining top-level element =================================================

  # For x_max ------------------------------------------------------------------
  if (class(x_max[[1]]) == "range") {
    x_max_list[[1]] <- sample(x_max_list[[1]][1]:x_max_list[[1]][2], size = 1)
  }

  # For x ----------------------------------------------------------------------
  trimmed_x <- FALSE  # defines if x was trimmed by x_max
  if (class(x_list[[1]]) == "range") {
    if (x_max_list[[1]] == 1) {
      x_list[[1]] <- 1
    } else {
      limit <- min(x_list[[1]][2], x_max_list[[1]])
      x_list[[1]] <- sample(x_list[[1]][1]:limit, size = 1)
    }
  } else {
    if (x_list[[1]] > x_max_list[[1]] & !identical(x, x_max)) {
      trimmed_x <- TRUE
    }
    x_list[[1]] <- min(x_list[[1]], x_max_list[[1]])
  }

  # Defining elements for other levels of x_max ================================
  for (l in 2:length(x_max)) {
      if (class(x_max_list[[l]]) == "range") {
        x_max_list[[l]] <- sample_within_range(x_max[[l]], sum(x_max_list[[l - 1]]))
      } else {
      if (length(x_max_list[[l]]) < sum(x_max_list[[l - 1]])) {
        x_max_list[[l]] <- rep(x_max[[l]], sum(x_max_list[[l - 1]]))
      }
    }
  }

  # Defining elements for other levels of x ====================================
  for (l in 2:length(x)) {
    if (class(x_list[[l]]) == "range") {
      if (l < length(x)) {
        limit <- min(sum(x_list[[l - 1]]), sum(x_max_list[[l - 1]]))
        x_list[[l]] <- sample_within_range(x_list[[l]], limit)
      } else {
        limit <- sum(x_list[[l - 1]])
        x_list[[l]] <- sample_within_range(x_list[[l]], limit)
      }
    } else {
      x_list[[l]] <- rep(x[[l]], length = sum(x_list[[l - 1]]))
    }

    ## Trim x_list[[l]] by comparing it to the max value -----------------------
    for (e in seq_along(x_list[[l]])) {
      if (x_list[[l]][e] > x_max_list[[l]][e] & !identical(x, x_max)) {
        trimmed_x <- TRUE
      }
      x_list[[l]][e] <- min(x_list[[l]][e], x_max_list[[l]][e])
    }
  }

  # Returning converted x ======================================================
  if (verbose & trimmed_x) {
      warning(
        "Some elements of the sample structure provided were larger ",
        "than their counterparts in the population",
        "; they were trimmed accordingly."
      )
  }
  return(x_list)
}