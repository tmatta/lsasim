#==============================================================================#
# test_7.R
# This syntax utilizes the saved PISA 2012 data. 
#==============================================================================#

#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R/population_pars.R")     
source("R/questionnaire_gen.R")   
source("R/item_gen.R")  
source("R/block_design.R")  
source("R/booklet_design.R")  
source("R/booklet_sample.R")  
source("R/response_gen.R")  
source("R/irt_gen.R")  

#--- Load PISA data -----------------------------------------------------------#
load("data/pisa2012_cat_pr.RData")
load("data/pisa2012_cor.RData")
load("data/pisa2012_math_item_pars.RData")
load("data/pisa2012_math_blocks.RData")
load("data/pisa2012_math_booklets.RData")

set.seed(5656)

#--- Specify sample size
# nn <- 10000
nn <- 50

#--- Background Questionnaire data 
pisa_backgroud <- questionnaire(n = nn, cat_prop = pisa2012_cat_pr, cor_matrix = pisa2012_cor)

#--- Create item - block assignment matrix
pisa2012_math_block_mat <- as.matrix(pisa2012_math_block[, -1])

#--- Assign items to blocks
pisa_blocks <- block_design(item_parameters = pisa2012_math_item_pars, 
                            item_block_matrix = pisa2012_math_block_mat)

#--- Specify block - booklet assignment matrix
pisa2012_math_book_mat <- as.matrix(pisa2012_math_booklet[, -1])

#--- Assign items to booklets
pisa_books <- booklet_design(item_block_assignment = pisa_blocks$block_assignment,
                             book_design = pisa2012_math_book_mat)

#--- Distribute bookelets to students
subj_booklets <- booklet_sample(n_subj = nn, book_item_design = pisa_books, e = .025)

#--- Generate item responses 
pisa_ir <- response_gen(subject = subj_booklets$subject, 
                        item    = subj_booklets$item, 
                        theta   = pisa_backgroud$PV1MATH, 
                        b_par   = pisa2012_math_item_pars$b,
                        d_par   = list(pisa2012_math_item_pars$d1, pisa2012_math_item_pars$d2))

#--- Rename item names to reflect PISA names 
colnames(pisa_ir)[1:(ncol(pisa_ir)-1)] <- pisa2012_math_item_pars$item_name[sort(unique(subj_booklets$item))]

#--- Merge survey data with cognative data
pisa_data <- merge(pisa_backgroud, pisa_ir, by = "subject")

str(pisa_data)
