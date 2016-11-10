#==============================================================================#
# test_4.R
# This syntax utilizes the default settings to generate backgroud data
#==============================================================================#

#--- Set directory ------------------------------------------------------------#

#--- Windows
setwd("Dropbox\\Research\\ilsasim")

#--- OSX
# setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R\\population_pars.R")     # generate population parameter for questionnaire_gen 
source("R\\questionnaire_gen.R")   # generate questionnaire data, including theta

#=== Parameters ===============================================================#
n_subj   <- 10                              # number of students
resp_typs <- c(4, 3, 5)
n_typs <- c(6, 2, 2)
n_vars   <- length(rep(resp_typs, n_typs))  # number of questionnaire variables

#==============================================================================#
#=== Test 1 ===================================================================#
#--- Generate marginal proportions 
cat_pr1 <- NULL
cat_pr1 <- gen_proportions(cat_options = resp_typs, 
                           n_cat_options = n_typs)

#--- Generate correlation matrix 
q1 <- rand_pd_corr(n_var = n_vars)


names(cat_pr1)
#--- Generate questionnaire data 
questionnaire(n = n_subj, cat_prop = cat_pr1, cor_matrix = q1, theta = TRUE)
questionnaire(n = n_subj, cat_prop = cat_pr1, cor_matrix = q1, theta = FALSE)

#==============================================================================#
#=== Test 2 ===================================================================#
#--- Generate marginal probabilities 
cat_pr2 <- gen_proportions(cat_options = 2, 
                           n_cat_options = 10)

#--- Generate correlation matrix 
q2 <- rand_pd_corr(n_var = 10)

#--- Generate questionnaire data 
questionnaire(n = n_subj, cat_prop = cat_pr2, cor_matrix = q2, theta = FALSE)
questionnaire(n = n_subj, cat_prop = cat_pr2, cor_matrix = q2, theta = TRUE)

table(surv1[, 20])
#=== Speed tests ==============================================================#










min(.99, rbeta(1, 2, 2) + rbeta(1, 2, 2))


plot(dbeta(x = seq(from = 0, to = 1, by = .01), 15, 1), type = "l")
plot(dbeta(x = seq(from = 0, to = 1, by = .01), 1, 15), type = "l")



ptm <- proc.time()
# CODE HERER
proc.time() - ptm