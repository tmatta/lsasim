#' @title Generate cluster sample
#' @param n numeric vector with the number of sampled observations (clusters or subjects) on each level
#' @param cluster_labels character vector with the names of each cluster level
#' @param resp_labels character vector with the names of the questionnaire respondents on each level
#' @param collapse if `TRUE`, function output contains only one data frame with all answers
#' @param separate_questionnaires if `TRUE`, each level will have its own questionnaire
#' @param N list of numeric vector with the population size of each *sampled* cluster element on each level
#' @param calc_weights if `TRUE`, sampling weights are calculated
#' @param sum_pop total population at each level (sampled or not)
#' @param n_X list of `n_X` per cluster level
#' @param n_W list of `n_W` per cluster level
#' @param c_mean vector of means for the continuous variables or list of vectors for the continuous variables for each level
#' @param sampling_method can be "SRS" for Simple Random Sampling or "PPS" for Probabilities Proportional to Size
#' @param verbose if `TRUE`, prints output messages
#' @param ... Additional parameters to be passed to `questionnaire_gen()`
#' @return list with background questionnaire data, grouped by level or not
#' @details This function relies heavily in two subfunctions---`cluster_gen_separate` and `cluster_gen_together`---which can be called independently. This does not make `cluster_gen` a simple wrapper function, as it performs several operations prior to calling its subfunctions, such as randomly generating `n_X` and `n_W` if they are not determined by user.
#'   `n` can have unitary length, in which case all clusters will have the same size.
#'   `N` is *not* the population size across all elements of a level, but the population size for each element of one level.
#'   Regarding the additional parameters to be passed to `questionnaire_gen()`, they can be passed either in the same format as `questionnaire_gen()` or as more complex objects that contain information for each cluster level.
#' @note For the purpose of this function, levels are counted starting from the top nesting/clustering level. This means that, by default, countries are the nexting level, schools are the first cluster level, classes are the second, and students are the third and final level.
#'
#' labeling c_mean has no effect, it's for the user.
#' @seealso cluster_estimates cluster_gen_separate cluster_gen_together
#' @export
cluster_gen <- function(
  n,
  cluster_labels = c("country", "school", "class")[seq(length(n) - 1)],
  # ASK: drop countries?
  resp_labels = c("principal", "teacher", "student")[seq(length(n))],
  n_X = NULL,
  n_W = NULL,  # TODO: allow different proportions for Ws
  c_mean = NULL,
  separate_questionnaires = TRUE,
  collapse = "none",
  N = n,
  sum_pop = sapply(N, sum),
  calc_weights = TRUE,
  sampling_method = "mixed",
  # TODO: Replicate weights
  # TODO: Control over inter-class correlation (intra-class handled by quest_gen?). Add correlations (within, between). Cheap solution: add random value to means and proportions before calling questionnaire_gen
  verbose = TRUE,
  ...
)
{
  # Validating =================================================================
  check_condition(!identical(N, n), "N != n not yet implemented") # TEMP
  check_condition(any(sapply(n, class) == "range"),  # TEMP
                 "Data generation for ranges not yet implemented") # TEMP
  check_condition(class(n) == "select", "Select not yet implemented") # TEMP
  check_condition(
    !separate_questionnaires & length(n_X) > 1,
    "Unique questionnaire requested. n_X must therefore be a scalar."
  )
  check_condition(
    !separate_questionnaires & length(n_W) > 1,
    "Unique questionnaire requested. n_W must therefore be a scalar or a list."
  )
  check_condition(length(n) == 1, "n must have length larger than 1")
  check_condition(
    length(n) > length(cluster_labels) + 1,
    "cluster_labels has insufficient length"
  )
  check_condition(
    !separate_questionnaires & collapse == "partial",
    paste("Partial collapsing unavailable for unique questionnaires.",
          "Treated as 'full'."), FALSE
  )
  check_condition(
    !(all(sampling_method %in% c("SRS", "PPS", "mixed"))),
    "Invalid sampling method"
  )
  check_condition(
    class(n) == "select" & class(N) == "select",
    "If n is select, N must be explicitly defined"
  )

   # Checking valid n-N combinations and reclassifying them ====================
   # vector + vector := ok
   # list + (blank) := ok
   # sample + vector or list := ok
   # list + vector or list := error ("use sample")
   if (class(n) == "select") {
     check_condition(class(N) == "select",
                     "If n is 'select', N must be a vector or a list")
     n <- sample_from(N, n)
   } else {
     if (class(n) == "list") {
      #  check_condition(!identical(N, n),
      #                  paste("If n and N are lists, they must be must be",      
      #                        "identical. Perhaps you mean to use 'select'",
      #                        "for n."))
       check_valid_structure(n)
       if (any(sapply(n, class) == "range")) {
         N <- convert_vector_to_list(N)
         check_valid_structure(N)
       } else {
         N <- convert_vector_to_list(N, n)
       }
     } else if (mode(n) == "numeric") {  # mode catches "numeric" and "integer"
      #  check_condition(class(N) %in% c("list", "sample"),
      #                  paste("If n is a vector, N must be identical to it.",
      #                        "Perhaps you mean to use 'select' for n"))
      #  check_condition(!identical(N, n),
      #                  paste("If n and N are vectors, they must be must be",
      #                        "identical. Perhaps you mean to use 'select'",
      #                        "for n."))
       n <- convert_vector_to_list(n)
       if (any(sapply(n, class) == "range")) {
         N <- convert_vector_to_list(N)
         check_valid_structure(N)
       } else {
         N <- convert_vector_to_list(N, n)
       }
       check_valid_structure(n)
      #  check_valid_structure(N)
     }
   }

  # Calculating useful arguments ===============================================
  n_levels <- length(n)
  if (!is.null(names(n))) {
    cluster_labels <- names(n)
    resp_labels <- c(names(n)[-1], "respondent")
  }

  # Treating NAs in labels =====================================================
  if (length(cluster_labels[!is.na(cluster_labels)]) < length(n)) {
    cluster_labels[is.na(cluster_labels)] <- "unknowncluster"
  }
  if (length(resp_labels[!is.na(resp_labels)]) < length(n)) {
    resp_labels[is.na(resp_labels)] <- "unknownrespondent"
  }

  # Removing accents ===========================================================
  names(n)       <- iconv(names(n), to = "ASCII//TRANSLIT")
  cluster_labels <- iconv(cluster_labels, to = "ASCII//TRANSLIT")
  resp_labels    <- iconv(resp_labels, to = "ASCII//TRANSLIT")

  # Adapting additional parameters to questionnaire_gen (n_X and n_W) ==========
  if (n_levels > 1 & separate_questionnaires) {
    if (length(n_X) == 1) n_X <- rep(n_X, n_levels)
    if (length(n_W) == 1 & class(n_W) == "numeric") {
      n_W <- rep(n_W, n_levels)
    } else if (length(n_W) > 1 & class(n_W) == "list") {
      n_W <- rep(list(n_W), n_levels)
    }
  }

  # Generating messages and data ===============================================
  if (separate_questionnaires) { # questionnaires administered at all levels
    # Generates unique questionnaires for each level

    # Message explaining cluster scheme ----------------------------------------
    if (verbose) {
      print(cli::rule(left = cli::col_blue("Hierarchical structure")))
      cluster_message(n, resp_labels, cluster_labels, n_levels,
                     separate_questionnaires, 1)
      if (identical(N, n)) {
        draw_cluster_structure(n, cluster_labels, resp_labels)
      } else {
        message("Sample structure")
        draw_cluster_structure(n, cluster_labels, resp_labels)
        # message("Population structure")
        # draw_cluster_structure(N, cluster_labels, resp_labels)
      }
    }

    # Defining n_X and n_W -----------------------------------------------------
    if (is.null(n_X)) {
      n_X <- list()
      for (l in seq(n_levels)) {
        n_X[[l]] <- rzeropois(1.5) # a positive number of Xs
      }
    }
    if (is.null(n_W)) {
      n_W <- list()
      for (l in seq(n_levels)) {
        n_W[[l]] <- as.list(replicate(rzeropois(5), 2)) # all Ws are binary
      }
    }
    
    # Questionnaire generation -------------------------------------------------
    if (verbose & calc_weights) {
      print(cli::rule(left = cli::col_blue("Information on sampling weights")))
    }
    sample <- cluster_gen_separate(
      n_levels, n, N, sum_pop, calc_weights, sampling_method,
      cluster_labels, resp_labels, collapse,
      n_X, n_W, c_mean, verbose, ...
    )
  } else { # questionnaires administered only at the bottom level
    # Message explaining cluster scheme ----------------------------------------
    if (verbose) {
      print(cli::rule(left = cli::col_blue("Hierarchical structure")))
      cluster_message(n, resp_labels, cluster_labels, n_levels,
                    separate_questionnaires, 2)
      if (identical(N, n)) {
        draw_cluster_structure(n, cluster_labels, resp_labels)
      } else {
        message("Sampled structure")
        draw_cluster_structure(n, cluster_labels, resp_labels)
        # message("Population structure")
        # draw_cluster_structure(N, cluster_labels, resp_labels)
      }
    }

    # Generating variable numbers ----------------------------------------------
    if (is.null(n_X)) n_X <- rzeropois(1.5) # a positive number of Xs
    if (is.null(n_W)) n_W <- as.list(replicate(rzeropois(5), 2)) # all binary

    # Questionnaire generation -------------------------------------------------
    if (verbose & calc_weights) {
      print(cli::rule(left = cli::col_blue("Information on sampling weights")))
    }
    sample <- cluster_gen_together(
      n_levels, n, N, sum_pop, calc_weights, sampling_method,
      cluster_labels, resp_labels, collapse,
      n_X, n_W, c_mean, verbose, ...
    )
  }

  return(sample)
}