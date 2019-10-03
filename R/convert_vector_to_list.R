#' @title Convert Vector to Expanded List
#' @description Converts a vector to list where each element is replicated a certain number of times depending on the previous vector. Also work for ranged lists
#' @param x vector or ranged list to be converted
#' @param x_max reference vector or ranged list with max values for x
#' @return expanded/replicated version of x
#' @export
convert_vector_to_list <- function(x, x_max = x) {
  # Defining basic elements ====================================================
  x_list <- as.list(x)
  x_max_list <- as.list(x_max)

  # Classifying x and x_max ====================================================
  class_x <- check_n_N_class(x)
  class_x_max <- check_n_N_class(x_max)

  # Defining top-level element =================================================

  # For x_max ------------------------------------------------------------------
  if (class(x_max[[1]]) == "range") {
    x_max_list[[1]] <- sample(x_max_list[[1]][1]:x_max_list[[1]][2], size = 1)
  }

  # For x ----------------------------------------------------------------------
  if (class(x_list[[1]]) == "range") {
    if (x_max_list[[1]] == 1) {
      x_list[[1]] <- 1
    } else {
      x_list[[1]] <- sample(x_list[[1]][1]:x_max_list[[1]], size = 1)
    }
  } else {
    x_list[[1]] <- min(x_list[[1]], x_max_list[[1]])
  }

  # Defining elements for other levels of x_max ================================
  if (class_x_max == "list with ranges") {
    for (l in 2:length(x_max)) {
      if (class(x_max_list[[l]]) == "range") {
        x_max_list[[l]] <- sample_within_range(x_max[[l]], sum(x_max_list[[l - 1]]))
      } else {
        x_max_list[[l]] <- rep(x_max[[l]], sum(x_max_list[[l - 1]]))
      }
    }
  }

  # Defining elements for other levels =========================================
  # browser()#TEMP
  for (l in 2:length(x)) {
    if (class(x_list[[l]]) == "range") {
      if (l < length(x)) {
        limit <- min(sum(x_list[[l - 1]]), sum(x_max_list[[l - 1]]))
        x_list[[l]] <- sample_within_range(x_list[[l]], limit)

        # Trim x_list[[l]] by comparing it to the max value --------------------
        for (e in seq_along(x_list[[l]])) {
          x_list[[l]][e] <- min(x_list[[l]][e], x_max_list[[l]][e])
        }
      } else {
        limit <- sum(x_list[[l - 1]])
        x_list[[l]] <- sample_within_range(x_list[[l]], limit)
        # Trim x_list[[l]] by comparing it to the max value --------------------
        for (e in seq_along(x_list[[l]])) {
          x_list[[l]][e] <- min(x_list[[l]][e], x_max_list[[l]][e])
        }
      }
    } else {
      x_list[[l]] <- rep(x[[l]], sum(x_list[[l - 1]]))
      # Trim x_list[[l]] by comparing it to the max value --------------------
      for (e in seq_along(x_list[[l]])) {
        x_list[[l]][e] <- min(x_list[[l]][e], x_max_list[[l]][e])
      }
    }
  }

  # Returning converted x ======================================================
  return(x_list)
}