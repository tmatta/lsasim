#==============================================================================#
# test_1.R
# This syntax utilizes the default settings on all the functions. 
# If parameters are unchanged, all functions should execute without error.
#==============================================================================#

#--- Set directory ------------------------------------------------------------#
set.seed(5656)

setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R/population_pars.R")     # generate population parameter for questionnaire_gen 
source("R/questionnaire_gen.R")   # generate questionnaire data, including theta
source("R/item_gen.R")  
source("R/block_design.R")  
source("R/booklet_design.R")  
source("R/booklet_sample.R")  
source("R/response_gen.R")  
source("R/irt_gen.R")  

#=== Parameters ===============================================================#
nn   <- 25                  # number of students

#=== Background Questionnaire data ============================================#
resp_typs <- c(1, 3, 5)
n_typs <- c(1, 5, 5)
n_vars   <- length(rep(resp_typs, n_typs))  

#--- Generate marginal probabilities 
cat_pr1 <- gen_proportions(cat_options = resp_typs, n_cat_options = n_typs)

#--- Generate correlation matrix 
q1 <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data 
backgroud_q <- questionnaire(n = nn, cat_prop = cat_pr1, cor_matrix = q1, theta = TRUE)


#=== Cognitive data ===========================================================#

#--- Generate item pool
item_pool <- item_gen(n_2pl = 10,
                       n_3pl = 20, 
                       thresholds = 2, 
                       b_bounds = c(-2, 2),
                       a_bounds = c(.75, 1.25),
                       c_bounds = c(0, .25))

#--- Assign items to blocks
blocks <- block_design(n_blocks = 5, item_parameters = item_pool)

#--- Assign blocks to booklets
booklets <- booklet_design(item_block_assignment = blocks$block_assignment)

#--- Assign booklets to subjects 
subj_booklets <- booklet_sample(n_subj = nn, book_item_design = booklets)

#--- Generate item responses 
item_responses <- response_gen(subject = subj_booklets$subject, 
                               item    = subj_booklets$item, 
                               theta   = backgroud_q$theta, 
                               b_par   = item_pool$b,
                               a_par   = item_pool$a,
                               c_par   = item_pool$c,
                               d_par   = list(item_pool$d1, item_pool$d2))


#=== Combine Survey data and Cognitive data ===================================#
final_data <- merge(backgroud_q, item_responses, by = "subject")

str(final_data)

#=== END Test_1.R =============================================================#