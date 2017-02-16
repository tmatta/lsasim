#' Assignment of test booklets to test takers
#'
#' \code{booklet_sample} randomly assigns test booklets to test takers.
#' 
#' @param n_subj an integer, the number of subjects (test takers).
#' @param book_item_design a data frame containing the items that belong to each 
#'        booklet with booklets as columns and booklet item numbers as rows.  See 'Details'.
#' @param book_prob a vector of probability weights for obtaining the booklets being sampled.
#'        The default equally weights all books.
#' @param resample logical indicating if booklets should be re-sampled to minimize differences.  
#'        Can only be used when \code{book_prob = NULL}.
#' @param e a number between 0 and 1 exclusive, re-sampling stopping criteria, 
#'        the difference between the most sampled and least sampled booklets.
#' @param iter an integer defining the number of iterations to reach e.
#' 
#' @section Details:
#' If using \code{booklet_sample} in tandem with \code{booklet_design}, \code{book_item_design}
#' is the the first element of the returned list of \code{block_design}.
#' 
#' @examples
#' it_bk <- matrix(c(1, 2, 1, 4, 5, 4, 7, 8, 7, 10, 3, 10, 2, 6, 3, 5, 9, 6, 8, 0, 9), 
#'            ncol = 3, byrow = TRUE)
#' booklet_sample(n_subj = 10, book_item_design = it_bk, book_prob = c(.2, .5, .3))
#' 
#' @export
#' 
booklet_sample <- function(n_subj, book_item_design, book_prob = NULL, 
                           resample = FALSE, e = .1, iter = 20){

  if (!is.null(book_prob) & resample == TRUE) stop("Cannot specify probability weights when re-sampling.", call. = FALSE)
  if (!is.null(book_prob) & length(book_prob) != ncol(book_item_design)) stop("The number of probability weights must equal the number of booklets", call. = FALSE)
  if (!is.null(book_prob) & sum(book_prob) != 1) stop("Probability weights must sum to 1.", call. = FALSE)
  subject <- 1 : n_subj

  #--- distribute books randomly to subject
  x <- 0
  subj_book_start <- sample(1:ncol(book_item_design), size = n_subj, replace = T, prob = book_prob)
  book_dist_start <- prop.table(table(subj_book_start))
  pr_dist <- pr_dist_start <- max(book_dist_start) - min(book_dist_start)
  
  if (resample == FALSE){
    subj_book <- subj_book_start
    book_dist <- book_dist_start
    # print(book_dist_start)  
  }

  if (resample == TRUE){
    #--- If initial sampling meets stopping criteria
    if (pr_dist <= e) {
      subj_book <- subj_book_start
      book_dist <- book_dist_start
      message("Initial difference in booklet distribution ", round(pr_dist, 2))     
    }
  
    #--- Else, make sure books are distributed equally
    while (pr_dist >= e) { 
      x <- x + 1
      subj_book_2 <- sample(1:ncol(book_item_design), size = n_subj, replace = T, prob = book_prob)
      book_dist_2 <- prop.table(table(subj_book_2))
      pr_dist_2 <- max(book_dist_2) - min(book_dist_2)
    
      if (pr_dist_2 < pr_dist){
        subj_book <- subj_book_2
        book_dist <- book_dist_2
        pr_dist <- pr_dist_2
      } 
      message("iteration ", x, ": difference in booklet distribution ", pr_dist)  
        
      if (x == iter & pr_dist_2 >= pr_dist ){
         subj_book <- subj_book_2
         book_dist <- book_dist_2
         pr_dist <- pr_dist_2
         message("Sampling terminated")
         break
       }
    }
  }

  #--- merge subject id with book id
  book_assign <- cbind(subject, book = subj_book) 
  booklet_mat <- t(book_item_design)   # each row rw is a book column is item 

  #--- merge item numbers with subject id and book id
  row.names(booklet_mat) <- 1:nrow(booklet_mat) # specify row names to use for merge

  item_assign <- merge(book_assign, booklet_mat, by.x = "book", by.y = "row.names")
  item_assign <- item_assign[order(item_assign[, "subject"]), ]

  #--- reshape to process through irt_gen()
  item_assign_l <- reshape(item_assign, 
                           varying = colnames(item_assign[, 3:ncol(item_assign)]), 
                           v.names = "item",
                           timevar = NULL, 
                           times = seq(3:ncol(item_assign)), 
                           direction = "long")
  
  #--- Sort data and remove excess 
  item_assign_l <- item_assign_l[order(item_assign_l$subject),]
  rownames(item_assign_l) <- item_assign_l$id <- NULL
  item_assign_l <- item_assign_l[, c("subject", "book", "item")]
  item_assign_l <- item_assign_l[which(item_assign_l$item != 0), ] 

  return(item_assign_l)

}
