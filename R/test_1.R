#==============================================================================#
# test_1.R
# This syntax utilizes the default settings on all the functions. 
# If parameters are unchanged, all functions should execute without error.
#==============================================================================#

#--- Set directory ------------------------------------------------------------#

#--- Windows
setwd("Dropbox\\Research\\ilsasim")

#--- OSX
setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R\\population_pars.R")     # generate population parameter for questionnaire_gen 
source("R\\questionnaire_gen.R")   # generate questionnaire data, including theta
source("R\\item_gen.R")            # generate item parameters (if you don't have your own!)
source("R\\irt_gen.R")             # generate responses based on theta and item parameters
source("R\\book_gen.R")            # generate booklet design (can provide your own boo)
source("R\\response_gen.R")        # generate response data
source("R\\test_assembly.R")       # generate response data

#=== Parameters ===============================================================#
n_subj   <- 10                  # number of students
n_vars   <- 20                  # number of questionnaire variables
n_forms  <- 5                   # number of test forms
form_len <- 10                  # number of items per form
n_items  <- n_forms * form_len  # number of total items

#=== Survey data ==============================================================#

#--- Generate marginal probabilities 
cat_pr1 <- rand_cum_proportions(n_var = n_vars, 
                                cat_options = 2:3, 
                                cat_proportions = c(.75, .25))

#--- Generate correlation matrix 
q1 <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data 
surv1 <- questionnaire(n = n_subj, cat_prop = cat_pr1, cor_matrix = q1)


#=== Cognitive data ===========================================================#

#--- Generate test assembly
test1 <- test_assembly(n_subj = n_subj, n_forms = n_forms, form_length = form_len)

#--- Generate item parameters (generalized partial credit / 3PL)
genGPCM <- item_gen(n_items   = n_items, 
                    b_bounds  = c(-2, 2),
                    a_bounds  = c(-.5, 1.75),
                    c_bounds  = c(0, 1), 
                    k_options = 1:3, 
                    k_proportions = c(.5, .3, .2))

#--- Generate item responses 
datGPCM <- response_gen(subject = test1$item_assign$subject, 
                        item    = test1$item_assign$item, 
                        theta   = surv1$theta, 
                        b_par   = genGPCM$b_par,
                        a_par   = genGPCM$a_par,
                        c_par   = genGPCM$c_par)

#=== Combine Survey data and Cognitive data ===================================#
final_data <- merge(surv1, datGPCM, by = "subject")

str(final_data)

#=== END Test_1.R =============================================================#