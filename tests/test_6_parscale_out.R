

rm(list = ls())

#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/ilsasim")

source("R/item_gen.R")  
source("R/block_design.R")  
source("R/booklet_design.R")  
source("R/booklet_sample.R")  
source("R/response_gen.R")  
source("R/irt_gen.R")  
source("R/write_parscale.R")  


#------------------------------------------------------------------------------#
#--- Example for 1 booklet with 1 form
#------------------------------------------------------------------------------#
options(width = 150)
#--- Generate items for example
gen2PL <- item_gen(n_2pl = c(10, 10),
                   thresholds = c(1, 3), 
                   b_bounds = c(-2, 2),
                   a_bounds = c(.75, 1.25))

item_block_mat <- matrix(nrow = nrow(gen2PL), ncol = 1, byrow = T, rep(1, 20))

block_2 <- block_design(item_parameters = gen2PL, item_block_matrix = item_block_mat)

book_2 <- booklet_design(item_block_assignment = block_2$block_assignment,
          book_design = matrix(1, nrow = 1, ncol = 1))

assign_booklets <- booklet_sample(n_subj = 25, book_item_design = book_2)

theta_vec <- rnorm(25, 0, 1)
item_responses <- response_gen(subject = assign_booklets$subject, 
                               item    = assign_booklets$item, 
                               theta   = theta_vec, 
                               b_par   = gen2PL$b,
                               a_par   = gen2PL$a,
                               c_par   = gen2PL$c,
                               d_par   = list(gen2PL$d1, gen2PL$d2))


#--- Writing data for parscale
test_dat <- cbind(theta = theta_vec, item_responses)

write.parscal(id_var = "subject", theta_var = "theta", file = "test1.txt", sep = "   ")




gen3PL_GPCM <- item_gen(n_2pl = 15,
                        n_3pl = 30, 
                        thresholds = 2, 
                        b_bounds = c(-2, 2),
                        a_bounds = c(.75, 1.25),
                        c_bounds = c(0, .25))

block_1 <- block_design(n_blocks = 5, item_parameters = gen3PL_GPCM)

book_1 <- booklet_design(item_block_assignment = block_1$block_assignment)

assign_booklets <- booklet_sample(n_subj = 100, book_item_design = book_1)

theta_vec <- rnorm(100, 0, 1)

item_responses <- response_gen(subject = assign_booklets$subject, 
                               item    = assign_booklets$item, 
                               theta   = theta_vec, 
                               b_par   = gen3PL_GPCM$b,
                               a_par   = gen3PL_GPCM$a,
                               c_par   = gen3PL_GPCM$c,
                               d_par   = list(gen3PL_GPCM$d1, gen3PL_GPCM$d2))

test_dat2 <- cbind(item_responses, theta = theta_vec)

write.parscal(dat = test_dat2, id_var = "subject", theta_var = "theta", file = "test1.txt", sep = "  ")
