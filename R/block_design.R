
block_design <- function(n_blocks = NULL, item_parameters, item_block_matrix = NULL){

  if (is.null(n_blocks)) n_blocks <- ncol(item_block_matrix)
  n_items <- nrow(item_parameters)

  #--- build default block / item matrix
  if (is.null(item_block_matrix)){
    item_block_vec <- rep(1:n_blocks, length.out = n_items)
    item_no <- rep(1:n_items)
    item_block_matrix <- matrix(NA, nrow = n_items, ncol = n_blocks)
  
    for(j in 1:n_blocks){
      for(i in 1:n_items){
        item_block_matrix[i, j] <- ifelse(item_block_vec[i] == j, 1, 0)
      }
    }
  }
  
  block_vec <- paste0("b", 1:n_blocks)
  colnames(item_block_matrix) <- block_vec

  item_block_df <- data.frame(item_block_matrix)
  item_block_df$item <- rownames(item_block_df)
  block_assignment <- merge(item_parameters, item_block_df, by = "item")

  #--- Build block descriptives table
  block_b <- block_len <- numeric()
  for(i in 1:n_blocks) {
    block_b[i] <- round(mean(block_assignment[which(block_assignment[, paste0("b", i)] == 1), "b"]), 3)
    block_len[i] <- sum(block_assignment[, paste0("b", i)])
  }
  block_descriptives <- rbind(block_len, block_b)
  rownames(block_descriptives) <- c("block length", "average difficulty")
  colnames(block_descriptives) <- block_vec

  #--- Build block assignment matrix
  item_per_block <- matrix(NA, ncol = n_blocks, nrow = max(block_len))

  for(i in 1:n_blocks){
    block_items <- item_block_df[which(item_block_df[, paste0("b", i)] == 1), "item"]
    block_i <- numeric()
    block_i <- as.numeric(c(block_items, rep(0, (max(block_len) - length(block_items)))))
    item_per_block[, i] <- block_i
  }
  colnames(item_per_block) <- block_vec
  item_vec <- paste0("i", 1:nrow(item_per_block))
  rownames(item_per_block) <- item_vec


  return(list(block_assignment = item_per_block, block_descriptives = t(block_descriptives)))

}
