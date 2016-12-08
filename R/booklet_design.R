booklet_design <- function(item_block_assignment, book_design = NULL){

  n_block <- ncol(item_block_assignment)
  max_items <- nrow(item_block_assignment)

  if (n_block <= 2 & is.null(book_design)){
    stop("Default booklet assembly requires more than 2 forms", call. = FALSE)
  }

  #--- Default book_design matrix
  if (is.null(book_design)){
    n_book <- ncol(item_block_assignment)

    book_design <- matrix(NA, nrow = n_block, ncol = 2)
    spiral <- rep(seq(from = 1, to = n_block, by = 1), 2)[1:(n_block + 1)]

    for (i in 1:n_block) {
      book_design[i, ] <- spiral[i:(i + 1)]
    }

    book_block_mat <- matrix(0, nrow = n_block, ncol = n_block)
    for (i in 1:nrow(book_design)) {
      for (j in 1:ncol(book_design)) {
        book_block_mat[i, book_design[i, j]] <- 1
      }
    }
    book_design <- book_block_mat
  } 


  #--- number of book is determined by the user specified book design
  if (!is.null(book_design)) n_book <- nrow(book_design)
  
  #--- Assign items to booklets
  max_items_per_book <- max(rowSums(book_design)) * max_items

  item_matrix <- matrix(NA, nrow = max_items_per_book, ncol = n_book)

  for (k in 1:n_book){
    book_k_blocks <- which(book_design[k, ]==1)
    book_k_items <- as.vector(item_block_assignment[, book_k_blocks])
    book_k_items <- book_k_items[which(book_k_items != 0)]
    if (nrow(item_matrix) - length(book_k_items) > 0) {
      zero_fill <- rep(0, nrow(item_matrix) - length(book_k_items))
    } else if (nrow(item_matrix) - length(book_k_items) == 0) {
      zero_fill <- NULL
    }
    book_k <- c(book_k_items, zero_fill) 
    item_matrix[, k] <- book_k
  }

  #--- Create row and column lables
  book_lab <- paste0("B", 1:n_book)
  item_lab <- paste0("i", 1:max_items_per_book)

  colnames(item_matrix) <- book_lab
  rownames(item_matrix) <- item_lab

  if(any(rowSums(item_matrix) == 0)){
    item_matrix <- item_matrix[-which(rowSums(item_matrix) == 0), ]
  }

  booklet <- data.frame(item_matrix)
  return(booklet)

}