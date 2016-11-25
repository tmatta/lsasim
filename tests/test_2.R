#==============================================================================#
# test_2.R
# This syntax utilizes the default settings to generate cognitive data
#==============================================================================#

#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R/book_gen.R")            # generate booklet design
source("R/test_assembly.R")       # generate response data
source("R/irt_gen.R")             # generate responses based on theta and item parameters
source("R/item_gen.R")            # generate item parameters 
source("R/response_gen.R")        # generate response data


#=== Parameters ===============================================================#
n_subj   <- 100                  # number of students
n_forms  <- 12                   # number of test forms
form_len <- 10                  # number of items per form
n_items  <- n_forms * form_len  # number of total items

#=== Cognitive data ===========================================================#

#--- Generate theta
theta <- rnorm(n_subj, 0, 1)

#--- Generate test assembly
test2 <- test_assembly(n_subj = n_subj, n_forms = n_forms, form_length = form_len)


#--- Rasch --------------------------------------------------------------------#
#--- Generate item parameters  
n <- 10
gen1PL <- item_gen(n_items  = n, 
                   b_bounds = c(-2, 2))

#--- Generate item responses 
dat1PL <- response_gen(subject = test2$item_assign$subject, 
                       item    = test2$item_assign$item, 
                       theta   = theta, 
                       b_par   = gen1PL$b_par)



#--- Rasch partial credit -----------------------------------------------------#
#--- Generate item parameters 
n <- c(5, 5, 5)
genRPCM <- item_gen(n_items       = n, 
                    b_bounds      = c(-2, 2),
                    k_options     = 1:3)

#--- Generate item responses 
datRPCM <- response_gen(subject = test2$item_assign$subj, 
                        item    = test2$item_assign$item, 
                        theta   = theta, 
                        b_par   = genRPCM$b_par,
                        a_par   = genRPCM$a_par)


#--- 2PL ----------------------------------------------------------------------#
#--- Generate item parameters  
n <- 10
gen2PL <- item_gen(n_items  = n, 
                   b_bounds = c(-2, 2),
                   a_bounds = c(.75, 1.75))

#--- Generate item responses 
dat2PL <- response_gen(subject = test2$item_assign$subj, 
                       item    = test2$item_assign$item, 
                       theta   = theta, 
                       b_par   = gen2PL$b_par,
                       a_par   = gen2PL$a_par)


#--- 3PL ----------------------------------------------------------------------#
#--- Generate item parameters  
n <- 10
gen3PL <- item_gen(n_items  = n, 
                   b_bounds = c(-2, 2),
                   a_bounds = c(.75, 1.75),
                   c_bounds = c(0, .2))

#--- Generate item responses 
dat3PL <- response_gen(subject = test2$item_assign$subj, 
                       item    = test2$item_assign$item, 
                       theta   = theta, 
                       b_par   = gen3PL$b_par,
                       a_par   = gen3PL$a_par,
                       c_par   = gen3PL$c_par)

#--- General partial credit ---------------------------------------------------#
#--- Generate item parameters  
n <- c(5, 5, 5)
genGPCM <- item_gen(n_items   = n, 
                    b_bounds  = c(-2, 2),
                    a_bounds  = c(.75, 1.75), 
                    k_options = 1:3)

#--- Generate item responses 
datGPCM <- response_gen(subject = test2$item_assign$subj, 
                        item    = test2$item_assign$item, 
                        theta   = theta, 
                        b_par   = genGPCM$b_par,
                        a_par   = genGPCM$a_par)


#--- Generalized partial credit / 3PL -----------------------------------------#
#--- Generate item parameters 
n <- c(5, 5, 5)
genGPCM <- item_gen(n_items   = n, 
                    b_bounds  = c(-2, 2),
                    a_bounds  = c(.75, 1.75),
                    c_bounds  = c(0, .2), 
                    k_options = 1:3)

#--- Generate item responses 
datGPCM <- response_gen(subject = test2$item_assign$subject, 
                        item    = test2$item_assign$item, 
                        theta   = theta, 
                        b_par   = genGPCM$b_par,
                        a_par   = genGPCM$a_par,
                        c_par   = genGPCM$c_par)


#=== END Test_2.R =============================================================#



# example. 12 books each with 2 blocks, 1 block is multi-choice, 1 block is gpc
# multi-choice block is 10 items
# gpc bloc is 2 items
# 


form_l <- c(3, 3, 4, 4, 3, 3)
n_forms <- length(form_l)
n_items <- sum(form_l)

items <- seq(1, n_items)
#this works but doesn't distribute the item types 
form_item <- rep(1:n_forms, times = form_l)


seq(1, length(items), length.out = form_l[1])

form_item <- rep(1:length(form_l), each = form_l)

item_matrix <- matrix(NA, nrow = max(form_l), ncol = length(form_l))


  for (k in 1:n_forms){
    # items in forms x
    form <- seq(k, form_l[k] + , f_length[i])
    item_matrix[form, k] <-  form
    item_matrix[, k] <- ifelse(is.na(item_matrix[, k]), 0, item_matrix[, k])
  }
