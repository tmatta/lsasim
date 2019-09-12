#' @title Draw Cluster Structure
#' @param n same from cluster_gen
#' @param labels corresponds to cluster_labels from cluster_gen
#' @param resp corresponds to resp_labels from cluster_gen
#' @param output "tree" or "vector"
#' @description This function creates a visual representation of the hierarchical structure
#' @return Prints structure to console.
#' @export
drawClusterStructure <- function(
  n, labels =  c("country", "school", "class")[seq(length(n) - 1)],
  resp =  c("principal", "teacher", "student")[seq(length(n))],
  output = "tree"
)
{
  # Convert n to list if necessary =============================================
  if (class(n) != "list") {
      n <- convertVectorToList(n)
  }
  out <- NULL

  # Create all nodes ===========================================================
  structure_table <- as.matrix(labelRespondents(n, labels))
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

    # Printing tree for this particular toplvl ---------------------------------
    if (output == "tree") {
      print(cli::tree(data.frame(toplvl_tree, parenthesis), root = toplvl))
    } else if (output == "text") {
      out <- append(out, as.character(cli::tree(data.frame(toplvl_tree, parenthesis), root = toplvl)))  #TEMP
    }
  }
  return(out)
}