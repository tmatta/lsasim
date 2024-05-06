#' Assignment of item blocks to test booklets
#'
#' \code{block_design} creates a data frame that identifies which items corresponds to which booklets.
#'
#' @param item_block_assignment a matrix that identifies which items correspond to which block.
#' @param book_design a matrix of indicators to assign blocks to booklets.
#'
#' @section Details:
#' If using \code{booklet_design} in tandem with \code{block_design}, \code{item_block_assignment}
#' is the the first element of the returned list of \code{block_design}.
#'
#' The columns of \code{item_block_assignment} represent each block while the rows
#' represent the number of items in each block.  Because the number of items per
#' block can vary, the number of rows represents the block with the most items. The
#' contents of \code{item_block_assignment} is the actual item numbers. The remainder of
#' shorter blocks are filled with zeros.
#'
#' The columns of \code{book_design} represent each book while the rows
#' represent each block.
#'
#' The default \code{book_design} assigns two blocks to every booklet in a spiral design.
#' The number of default booklets is equal to the number of blocks and must be >= 3.
#' If \code{ncol(item_block_assignment)} < 3, \code{book_design} must be specified.
#'
#' @examples
#' i_blk_mat <- matrix(seq(1:40), ncol = 5)
#' blk_book <- matrix(c(1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1,
#'                      0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0),
#'                      ncol = 5, byrow = TRUE)
#' booklet_design(item_block_assignment = i_blk_mat, book_design = blk_book)
#' booklet_design(item_block_assignment = i_blk_mat)
#'
#' @export
booklet_design <- function(item_block_assignment, book_design = NULL){

  n_block <- ncol(item_block_assignment)
  max_items <- nrow(item_block_assignment)

  if (n_block <= 2 & is.null(book_design)){
    stop("Default booklet assembly requires more than 2 blocks", call. = FALSE)
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
