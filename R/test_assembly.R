
# depends on booklets()

test_assembly <- function(n_subj, n_forms = 1, form_length, 
                          book_design = NULL, 
                          e = .1, iter = 20){

  booklets <- book_gen(n_forms = n_forms, form_length = form_length, book_design = book_design)

  #if (n_forms > 2){
  #  booklets <- book_gen(n_forms = n_forms, form_length = form_length, book_design = book_design)
  #} else {
  #  booklets <- lapply(1:n_forms, function(x){ 
  #                              seq(from = ((form_len[x] * x) - form_len[x] + 1), 
  #                                  to = (form_len[x] * x), 
  #                                  by = 1)
  #                            }
  #                  )
  #
  #}

  #--- distribute books randomly to subject
  pr_dist <- .2
  x <- 0

  #--- make sure books are distributed equally
  while (pr_dist > e) { 
    subj_book <- sample(1:length(booklets), size = n_subj, replace = T)
    book_dist <- prop.table(table(subj_book))
    pr_dist <- max(book_dist) - min(book_dist)
  
    x <- x + 1
    if (x < iter) {
       cat("iteration ", x, " difference in booklet distribution ", pr_dist, "\n" )  
    } else if (x == iter){
       cat("iteration ", x, " difference in booklet distribution ", pr_dist, "\n 
             Sampling terminated \n" )
       break
     }
  }

  #--- merge subject id with book id
  book_assign <- cbind(subject = 1:n_subj, book = subj_book) 
  booklet_mat <- do.call(rbind, booklets)   # each row rw is a book column is item 

  #--- merge item numbers with subject id and book id
  row.names(booklet_mat) <- 1:length(booklets) # specify row names to use for merge

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

  return(list(item_assign = item_assign_l, 
              book_assign = book_assign,
              items_per_book = booklet_mat))

}



