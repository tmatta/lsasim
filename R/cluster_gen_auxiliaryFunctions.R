drawClusterStructure <- function(n, labels, resp) {
  # This function creates a visual representation of the hierarchical structure
  structure_table <- as.matrix(labelRespondents(n, labels))
  toplvl_labels <- unique(structure_table[, 1])
  for (toplvl in toplvl_labels) {
    submatrix <- structure_table[structure_table[, 1] == toplvl, , drop = FALSE]

    # Create all nodes
    nodes <- vector()
    for (row in seq(nrow(submatrix))) {
      for (col in 2:ncol(submatrix)) {
        nodes <- append(nodes,
                        paste(as.vector(submatrix[row, 1:col]), collapse = "_"))
      }
    }
    nodes <- c(toplvl, unique(nodes))

    # Retrieve children
    children_list <- list()
    for (n1 in seq(nodes)) {
      chars_n1 <- nchar(nodes[n1])
      level_n1 <- nchar(gsub("[A-Za-z0-9]", "", nodes[n1])) + 1
      children <- vector()
      for (n2 in seq(nodes)) {
        parent <- substr(nodes[n2], 1, chars_n1)
        level_n2 <- nchar(gsub("[A-Za-z0-9]", "", nodes[n2])) + 1
        if (parent == nodes[n1] & (level_n1 + 1) == level_n2) {
          children <- append(children, nodes[n2])
        }
      }
      children_list[[n1]] <- children
    }

    # Adding final-level observations
    toplvl_tree <- data.frame(nodes, I(children_list))

    submx_collapsed <- apply(submatrix, 1, function(x) paste(x, collapse = "_"))
    structure_collapsed <- apply(structure_table, 1,
                                 function(x) paste(x, collapse = "_"))
    obs <- n[[length(n)]][match(submx_collapsed, structure_collapsed)]
    obs <- data.frame(submx_collapsed, obs)
    obs <- merge(data.frame(nodes), obs,
                 by.x = "nodes", by.y = "submx_collapsed", all = TRUE)
    parenthesis <- vector()
    for (node in seq(nodes)) {
      if (!is.na(obs$obs[node])) {
        parenthesis <- append(parenthesis,
                              paste(nodes[node],
                              cli::style_dim(paste0("(",
                                                    obs$obs[node], " ",
                                                    resp[length(resp) - 1],
                                                    ")"))))
      } else {
        parenthesis <- append(parenthesis, cli::col_white(nodes[node]))
      }
    }

    # Printing tree for this particular toplvl
    print(cli::tree(data.frame(toplvl_tree, parenthesis), root = toplvl))
  }
}

clusterMessage <- function(n_obs, resp_labels, cluster_labels, n_levels,
                           separate_questionnaires, type) {
  # This function prints messages about the cluster scheme before generating questionnaire responses. All arguments are from cluster_gen except for "type", which is numeric and changes the way the first line is printed.
  if (type == 1) {
    # Comma-separated multiple questionnaires
    message("Generating questionnaires for ",
            paste(cluster_labels, collapse = ", "))
  } else {
    # Questionnaires only for the lowest level
    message("Generating questionnaires for ", resp_labels[n_levels - 1])  
  }

  tot_resp <- 0
  operands <- NULL
  for (l in seq(length(n_obs) - 1)) { # Final row of messages is different
    n_obs_print <- n_obs

    # Printing top level
    if (l == 1) {
      message("Top level: ", cluster_labels[l], " (", n_obs_print[l], ")")
    }

    # Printing second to second-to-last levels
    if (class(n_obs) == "list" & length(n_obs[[l + 1]]) > 1) {
      n_obs_print[[l]] <- paste0(paste(n_obs[[l]], collapse = " and "),
                                 ", respectively")
      n_obs_print[[l + 1]] <- paste0(paste(n_obs[[l + 1]], collapse = " and "),
                                     ", respectively")
    }
    if (l < length(n_obs) - 1) {
      message("Each ", cluster_labels[l], " sampled ",  cluster_labels[l + 1],
              " (", n_obs_print[l + 1], ")")
    }
    if (l > 1 & class(n_obs) != "list" & separate_questionnaires) {
      tot_resp <- tot_resp + prod(n_obs[1:l])
      operands <- c(operands, prod(n_obs[1:l]))
    }
  }

  # Final level
  message("Each ", cluster_labels[n_levels - 1], " sampled ",
          resp_labels[n_levels - 1], " (", n_obs_print[n_levels], ")")

  # Total respondents
  if (class(n_obs) == "list") {
    if (separate_questionnaires) {
      tot_resp <- sum(unlist(n_obs)[-1])
      operands <- unlist(n_obs[-1])
      operator <- " + "
    } else {
      tot_resp <- sum(unlist(n_obs[[n_levels]]))
      operands <- unlist(n_obs[[n_levels]])
      operator <- " + "
    }
  } else {
    if (separate_questionnaires) {
      tot_resp <- tot_resp + prod(n_obs)
      operands <- c(operands, prod(n_obs))
      operator <-  " + "
    } else {
      tot_resp <- prod(n_obs)
      operands <- n_obs
      operator <- " * "
    }
  }
  message("Total respondents: ", paste0(tot_resp, " (",
            paste(operands, collapse = operator), ")"))
}

labelRespondents <- function (n_obs, cluster_labels) {
  # This function nerated level label combinations for each respondent

  n_levels <- length(n_obs)
	level_combos <- list()  # will store ID combinations

  if (class(n_obs) == "list") {
    n_combos <- sum(n_obs[[n_levels - 1]])
    second_last_level <- unlist(sapply(n_obs[[n_levels - 1]], seq))
    id_combos <- matrix(second_last_level, ncol = n_combos)
    if (n_levels - 1 >= 2) {
      for (row in 2:(n_levels - 1)) {
        id_level <- max(n_levels - row, 1)
        if (length(n_obs[[id_level]]) > 1 & all(n_obs[[id_level]] == 1)) {
          n_obs[[id_level]] <- sum(n_obs[[id_level]])
        }
        expanded_level <- as.vector(unlist(sapply(n_obs[[id_level]], seq)))
        expanded_level_col <- 1
        new_row <- matrix(ncol = n_combos)
        for (col in seq(n_combos)) {
          if (all(id_combos[, col] == 1)) {
            new_row[col] <- expanded_level[expanded_level_col]
            expanded_level_col <- expanded_level_col + 1
          } else {
            new_row[col] <- new_row[col - 1]
          }
        }
        id_combos <- rbind(id_combos, new_row)
      }
    }
    id_combos <- t(id_combos)
  } else {
    for (l in 1:(n_levels - 1)) {
      level_combos[[n_levels - l]] <- seq(from = 1, to = n_obs[l])
    }
    id_combos <- expand.grid(level_combos)
  }
  id_combos <- id_combos[, ncol(id_combos):1]  # so first level comes first
  id_combos <- as.data.frame(id_combos)  # prevents bug with list n. don't ask.
  names(id_combos) <- cluster_labels[-n_levels]
  for (l in seq(ncol(id_combos))) {
    id_combos[, l] <- paste0(cluster_labels[l], id_combos[, l])
  }
  return(id_combos)
}

weightResponses <- function(cluster_bg, n_obs, N, lvl, sublvl, previous_sublvl, 
                            sampling_method, cluster_labels, resp_labels,
                            sum_pop, verbose) {
  # This function calculates sampling weight for the questionnaire responses
  if (length(sampling_method) > 1) {
    sampling_method <- sampling_method[lvl - 1]
  } else if (sampling_method == "mixed") {
    # Reassigns sampling method. PPS for schools, SRS for otherwise

    sampling_method <- ifelse(test = cluster_labels[lvl - 1] == "school",
                              yes  = "PPS",
                              no   = "SRS")
  }

  # Variable names
  label_1_i <- paste0(cluster_labels[lvl - 1], ".weight")
  label_2_ij <- paste0("within.", cluster_labels[lvl - 1], ".weight")
  label_ij <- paste0("final.", resp_labels[lvl - 1], ".weight")

  # Messages to user
  if (verbose) {
    if (sublvl == 1) {
      message("\nCalculating ", sampling_method, " weights at the ",
              cluster_labels[lvl - 1], " level")
      if (sampling_method == "SRS") {
        message(label_1_i, " should add up to the population size (",
                sum_pop[lvl - 1], ") across all ", cluster_labels[lvl - 1],
                " (repeated measures excluded)")
      } else {
        message(label_ij, " should add up to the population size (",
                sum_pop[lvl] * length(N[[lvl - 1]]), ") across all ",
                cluster_labels[lvl - 1])
      }
    }
  }

  # Probabilities (school and within school)
  if (sampling_method == "SRS") {
    if (class(n_obs) == "list") {
      p_1_i <- n_obs[[lvl - 1]] / N[[lvl - 1]]
      p_2_ij <- n_obs[[lvl]] / N[[lvl]]
      if (length(p_1_i) > 1) p_1_i <- p_1_i[previous_sublvl]
      if (length(p_2_ij) > 1) p_2_ij <- p_2_ij[sublvl]
    } else {
      p_1_i <- n_obs[lvl - 1] / N[lvl - 1]
      p_2_ij <- n_obs[lvl] / N[lvl]
    }
  } else if (sampling_method == "PPS") {
    if (class(n_obs) == "list") {
      p_1_i <- n_obs[[lvl - 1]] * N[[lvl]][sublvl] / sum_pop[lvl]
      p_2_ij <- n_obs[[lvl]] / N[[lvl]]
      if (length(p_1_i) > 1) p_1_i <- p_1_i[previous_sublvl]
      if (length(p_2_ij) > 1) p_2_ij <- p_2_ij[sublvl]
    } else {
      p_1_i <- n_obs[lvl - 1] * N[lvl] / sum_pop[lvl]
      p_2_ij <- n_obs[lvl] / N[lvl]
    }
  }

  # Final student probabilities
  p_ij <- p_1_i * p_2_ij

  # Weights
  w_1_i <- 1 / p_1_i  # school weight
  w_2_ij <- 1 / p_2_ij  # within-school weight
  w_ij <- 1 / p_ij  # final student weight

  # Final assignments
  cluster_bg[label_1_i] <- w_1_i
  cluster_bg[label_2_ij] <- w_2_ij
  cluster_bg[label_ij] <- w_ij

  return(cluster_bg)
}