#==============================================================================#
# test_2_gen_cog_dta.R
# This syntax tests the functions required to generate cognitive data
#==============================================================================#
rm(list = ls())

#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/ilsasim")

source("R/item_gen.R")  
source("R/block_design.R")  
source("R/booklet_design.R")  
source("R/booklet_sample.R")  
source("R/response_gen.R")  
source("R/irt_gen.R")  

#------------------------------------------------------------------------------#
#--- Example using the defaults
#------------------------------------------------------------------------------#

#--- Generate theta
theta <- rnorm(20, 0, 1)

#--- Generate item pool
item_pool <- item_gen(n_2pl = 10,
                       n_3pl = 27, 
                       thresholds = 2, 
                       b_bounds = c(-2, 2),
                       a_bounds = c(.75, 1.25),
                       c_bounds = c(0, .25))

#--- Assign items to blocks
blocks <- block_design(n_blocks = 5, item_parameters = item_pool)

#--- Assign blocks to booklets
booklets <- booklet_design(item_block_assignment = blocks$block_assignment)

#--- Assign booklets to subjects 
subj_booklets <- booklet_sample(n_subj = 20, book_item_design = booklets)

#--- Generate item responses 
item_responses <- response_gen(subject = subj_booklets$subject, 
                               item    = subj_booklets$item, 
                               theta   = theta, 
                               b_par   = item_pool$b,
                               a_par   = item_pool$a,
                               c_par   = item_pool$c,
                               d_par   = list(item_pool$d1, item_pool$d2))



#------------------------------------------------------------------------------#
#--- Example with PISA data
#------------------------------------------------------------------------------#

load("data/pisa2012_math_item_pars.RData")
load("data/pisa2012_math_blocks.RData")
load("data/pisa2012_math_booklets.RData")

#--- Generate theta
pisa_theta <- rnorm(50, 0, 1)

#--- create item - block design matrix
pisa2012_math_block_mat <- as.matrix(pisa2012_math_block[, -1])

pisa_blocks <- block_design(item_parameters = pisa2012_math_item_pars, 
                            item_block_matrix = pisa2012_math_block_mat)

#--- create block - booklet design matrix
pisa2012_math_book_mat <- as.matrix(pisa2012_math_booklet[, -1])

pisa_books <- booklet_design(item_block_assignment = pisa_blocks$block_assignment,
                             book_design = pisa2012_math_book_mat)

subj_pisa_booklets <- booklet_sample(n_subj = 50, book_item_design = pisa_books)

pisa_responses <- response_gen(subject = subj_pisa_booklets$subject, 
                               item    = subj_pisa_booklets$item, 
                               theta   = pisa_theta, 
                               b_par   = pisa2012_math_item_pars$b,
                               d_par   = list(pisa2012_math_item_pars$d1, pisa2012_math_item_pars$d2))

head(pisa_responses, 25)