
#==============================================================================#
# Function to create booklet design
# Results in a list of vectors containing the item numbers associated with that booklet
#==============================================================================#

# n_forms is a number 
# form_length is a number
# book_design is a matrix

book_gen <- function(n_forms, form_length, book_design = NULL){

  if (n_forms <= 2 & is.null(book_design)) stop("Default booklet assembly requires more than 2 forms", call. = FALSE)
  if (length(form_length) < n_forms & length(form_length) != 1) stop("Must specify a length for each form", call. = FALSE)

  if (length(form_length) == 1){
    f_length <- rep(form_length, n_forms)
  } else {
    f_length <- form_length.
  }

  n_items <- sum(f_length)
  items <- seq(1, n_items)
   
  # Default book_design matrix
  if (is.null(book_design)){
    book_design <- matrix(NA, nrow = n_forms, ncol = 2)
    spiral <- rep(seq(from = 1, to = n_forms, by = 1), 2)[1:(n_forms + 1)]

    for (i in 1:n_forms) {
      book_design[i, ] <- spiral[i:(i + 1)]
    }
  } 
  

  # Number of items are determined my form length and number of forms

  #--- Can add an extra row of 1s to link all forms.
  #--- empty matrix
  item_matrix <- matrix(NA, nrow = max_length, ncol = n_forms)

  #--- Balanced incomplete design 
  for (k in 1:n_forms){
    # items in forms x
    form <- seq(k, f_length, f_length[i])
    item_matrix[form, k] <-  form
    item_matrix[, k] <- ifelse(is.na(item_matrix[, k]), 0, item_matrix[, k])
  }

  #--- Create booklets
  booklet <- list()

  for (i in 1:nrow(book_design)){
    bb <- as.vector(item_matrix[, book_design[i, ] ])
    booklet[[i]] <- sort(bb[which(bb > 0)])
  }
  
  return(booklet)

}


