#==============================================================================#
# test_3.R
# This syntax utilizes a user defined matrix sampling design
#==============================================================================#

#--- Set directory ------------------------------------------------------------#

#--- Windows
setwd("Dropbox\\Research\\ilsasim")

#--- OSX
setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R\\book_gen.R")            # generate booklet design
source("R\\test_assembly.R")       # generate response data

#=== Parameters ===============================================================#

#--- Input booklet design 
spiral <- c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,1, 1,5, 2,6, 3,7, 4,8)
books <- matrix(spiral, ncol = 2, byrow = T)

n_subj   <- 10                  # number of students
n_forms  <- max(spiral)         # number of test forms, safe to take from spiral
form_len <- 5                  # number of items per form
n_items  <- n_forms * form_len  # number of total items

#--- Generate test assembly ---------------------------------------------------#
test3 <- test_assembly(n_subj      = n_subj, 
                       n_forms     = n_forms, 
                       form_length = form_len, 
                       book_design = books)


str(test3)
head(test3$item_assign)
head(test3$book_assign)
test3$items_per_book

#=== END Test_3.R =============================================================#


