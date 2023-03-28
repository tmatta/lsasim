#' @title Draw Cluster Structure
#' @param n same from cluster_gen
#' @param labels corresponds to cluster_labels from cluster_gen
#' @param resp corresponds to resp_labels from cluster_gen
#' @param output "tree" draws a tree-like structure on the console, "text" prints the structure as a character vector
#' @description This function creates a visual representation of the hierarchical structure
#' @return Prints structure to console.
#' @note This function is useful for checking how a `list()` object looks as a hierarchical structure, usually to be used as the  `n` and/or `N` arguments of the `cluster_gen` function.
#' @examples
#' n <- c(2, 4, 3)
#' draw_cluster_structure(n)
#' draw_cluster_structure(n, output="text")
#' @export
draw_cluster_structure <- function(
  n, labels = NULL, resp = NULL, output = "tree"
)
{
  # Check if list structure is correct =========================================
  if (!is(n, "list")) n <- convert_vector_to_list(n)
  if (any(sapply(n, class) == "range")) n <- convert_vector_to_list(n)
  check_valid_structure(n)

  # Create labels ==============================================================
  if (is.null(labels)) labels <- attribute_cluster_labels(n)$cl
  if (is.null(resp)) resp <- attribute_cluster_labels(n)$resp

  # Create all nodes ===========================================================
  out <- NULL
  structure_table <- as.matrix(label_respondents(n, labels))
  toplvl_labels <- unique(structure_table[, 1])
  for (toplvl in toplvl_labels) {
    submatrix <- structure_table[structure_table[, 1] == toplvl, , drop = FALSE]
    nodes <- vector()
    for (row in seq(nrow(submatrix))) {
      if (ncol(submatrix) > 1) {
        for (col in 2:ncol(submatrix)) {
          nodes <- append(
            nodes, paste(as.vector(submatrix[row, 1:col]), collapse = "_")
          )
        }
      } else {
        nodes <- append(
          nodes, paste(as.vector(submatrix[row, 1]), collapse = "_")
        )
      }
    }
    nodes <- c(toplvl, unique(nodes))

    # Retrieve children --------------------------------------------------------
    children_list <- list()
    for (n1 in seq(nodes)) {
      chars_n1 <- nchar(nodes[n1])
      level_n1 <- nchar(gsub("[A-Za-z0-9]", "", nodes[n1])) + 1  # counts "_"
      children <- vector()
      for (n2 in seq(nodes)) {
        parent <- substr(nodes[n2], 1, chars_n1)
        level_n2 <- nchar(gsub("[A-Za-z0-9]", "", nodes[n2])) + 1  # counts "_"
        if (parent == nodes[n1] & (level_n1 + 1) == level_n2) {
          children <- append(children, nodes[n2])
        }
      }
      children_list[[n1]] <- children
    }

    # Adding final-level observations ------------------------------------------
    toplvl_tree <- data.frame(nodes, I(children_list))
    submx_collapsed <- apply(submatrix, 1, function(x) paste(x, collapse = "_"))
    structure_collapsed <- apply(structure_table, 1,
                                 function(x) paste(x, collapse = "_"))
    obs <- n[[length(n)]][match(submx_collapsed, structure_collapsed)]
    obs <- data.frame(submx_collapsed, obs)
    obs <- merge(
      x = data.frame(nodes), y = obs, by.x = "nodes", by.y = "submx_collapsed",
      all = TRUE
    )
    obs <- obs[match(nodes, obs$nodes), ]  # fixes order for 10+ PSUs
    parenthesis <- vector()
    for (node in seq(nodes)) {
      if (!is.na(obs$obs[node])) {
        parenthesis <- append(parenthesis,
                              paste(nodes[node],
                              cli::style_dim(paste0("(",
                                                    obs$obs[node], " ",
                                                    pluralize(resp[length(resp)], obs$obs[node]),
                                                    ")"))))
      } else {
        parenthesis <- append(parenthesis, cli::col_white(nodes[node]))
      }
    }

    # Printing tree for this particular toplvl ---------------------------------
    if (output == "tree") {
      print(cli::tree(data.frame(toplvl_tree, parenthesis), root = toplvl))
    } else if (output == "text") {
      out <- append(
        out,
        as.character(
          cli::tree(data.frame(toplvl_tree, parenthesis), root = toplvl)
        )
      )
    }
  }
  if (output == "text") return(out)
}
