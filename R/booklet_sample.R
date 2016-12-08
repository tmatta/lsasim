booklet_sample <- function(n_subj, book_item_design, e = .1, iter = 20){

  subject <- 1 : n_subj

  #--- distribute books randomly to subject
  x <- 0
  subj_book_start <- sample(1:ncol(book_item_design), size = n_subj, replace = T)
  book_dist_start <- prop.table(table(subj_book_start))
  pr_dist <- pr_dist_start <- max(book_dist_start) - min(book_dist_start)

  #--- make sure books are distributed equally
  while (pr_dist > e) { 
    x <- x + 1
    subj_book_2 <- sample(1:ncol(book_item_design), size = n_subj, replace = T)
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
