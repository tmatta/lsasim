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

#=== Test 3.1 =================================================================#
#--- User defined booklet assembly 

#--- Input booklet design 
spiral <- c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,1, 1,5, 2,6, 3,7, 4,8)
books <- matrix(spiral, ncol = 2, byrow = T)

n_subj   <- 5             # number of students
n_forms  <- max(spiral)    # number of test forms, safe to take from spiral
form_len <- 5              # number of items per form

#--- Generate test assembly ---------------------------------------------------#
test3 <- test_assembly(n_subj      = n_subj, 
                       n_forms     = n_forms, 
                       form_length = form_len, 
                       book_design = books)


str(test3)
head(test3$item_assign)
head(test3$book_assign)
test3$items_per_book



#=== Test 3.2 =================================================================#
#--- Will get error if using the default booklet generation with less than 2 forms
n_subj   <- 5                  # number of students
form_len <- 5                  # number of items per form

#--- Generate test assembly ---------------------------------------------------#
test4 <- test_assembly(n_subj      = n_subj, 
                       n_forms     = 2, 
                       form_length = form_len)
# + Error: Default booklet assembly requires more than 2 forms

#--- You can use less than three forms by specifying your own book design:
#--- Two forms:
books <- matrix(c(1, 2), ncol = 1, byrow = T)
test5 <- test_assembly(n_subj      = n_subj, 
                       n_forms     = 2, 
                       form_length = form_len, 
                       book_design = books)


#--- One form to everyon: 
books <- matrix(c(1), ncol = 1, byrow = T)
test5 <- test_assembly(n_subj      = n_subj, 
                       n_forms     = 1, 
                       form_length = form_len, 
                       book_design = books)


#=== END Test_3.R =============================================================#

