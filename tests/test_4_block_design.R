#==============================================================================#
# test_5.R
# This syntax test the block_assembly() function
#==============================================================================#
rm(list = ls())

#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/lsasim")

source("R/item_gen.R")  
source("R/block_design.R")  
source("R/booklet_design.R")  

#------------------------------------------------------------------------------#
#--- Example using the defaults
#------------------------------------------------------------------------------#
#--- Generate items for example
gen3PL_GPCM <- item_gen(n_2pl = 10,
                        n_3pl = 36, 
                        thresholds = 2, 
                        b_bounds = c(-2, 2),
                        a_bounds = c(.75, 1.25),
                        c_bounds = c(0, .25))

block_1 <- block_design(n_blocks = 5, item_parameters = gen3PL_GPCM)
book_1 <- booklet_design(item_block_assignment = block_1$block_assignment)
class(book_1)

matrix(seq(1:40), ncol = 5, byrow = TRUE)
matrix(c(1, 0, 1, 0, 0, 0, 1, 0, 1, 0, 0, 0, 1, 0, 1,
         0, 0, 0, 1, 1, 0, 1, 1, 0, 0, 1, 0, 0, 1, 0), ncol = 5, brow = TRUE)
#------------------------------------------------------------------------------#
#--- Example with user-specified item_block_matrix
#------------------------------------------------------------------------------#

#--- Generate items for example
gen2PL <- item_gen(n_2pl = 20,
                   b_bounds = c(-2, 2),
                   a_bounds = c(.75, 1.25))


item_block_mat <- matrix(nrow = nrow(gen2PL), ncol = 5, byrow = T,
  c(1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 
    1, 0, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 1, 0, 0, 0, 0, 
    1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 
    0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 0, 0, 0, 0, 1, 
    0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 0, 0, 0, 0, 1, 0, 1, 0, 0, 0))

block_2 <- block_design(item_parameters = gen2PL, item_block_matrix = item_block_mat)


book_2 <- booklet_design(item_block_assignment = block_2$block_assignment)

#------------------------------------------------------------------------------#
#--- Example with PISA data
#------------------------------------------------------------------------------#
load("data/pisa2012_math_item_pars.RData")
load("data/pisa2012_math_blocks.RData")
load("data/pisa2012_math_booklets.RData")

#--- create item - block assignment matrix
pisa2012_math_block_mat <- as.matrix(pisa2012_math_block[, -1])

pisa_blocks <- block_design(item_parameters = pisa2012_math_item_pars, 
                            item_block_matrix = pisa2012_math_block_mat)


pisa2012_math_book_mat <- as.matrix(pisa2012_math_booklet[, -1])

pisa_books <- booklet_design(item_block_assignment = pisa_blocks$block_assignment,
                             book_design = pisa2012_math_book_mat)


head(pisa_books)




