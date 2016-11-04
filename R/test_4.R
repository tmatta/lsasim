#==============================================================================#
# test_4.R
# This syntax utilizes the default settings to generate backgroud data
#==============================================================================#

#--- Set directory ------------------------------------------------------------#

#--- Windows
setwd("Dropbox\\Research\\ilsasim")

#--- OSX
setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R\\population_pars.R")     # generate population parameter for questionnaire_gen 
source("R\\questionnaire_gen.R")   # generate questionnaire data, including theta

#=== Parameters ===============================================================#
n_subj   <- 20                  # number of students
n_vars   <- 10                  # number of questionnaire variables

#=== Survey data ==============================================================#

#--- Generate marginal probabilities 
cat_pr1 <- rand_cum_proportions(n_var = n_vars, 
                                cat_options = c(1, 3, 5), 
                                cat_proportions = c(.2, .6, .2))

#--- Generate correlation matrix 
q1 <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data 
surv1 <- questionnaire(n = n_subj, cat_prop = cat_pr1, cor_matrix = q1)
table(surv1[, 20])
#=== Speed tests ==============================================================#




min(.99, rbeta(1, 2, 2) + rbeta(1, 2, 2))


plot(dbeta(x = seq(from = 0, to = 1, by = .01), 15, 1), type = "l")
plot(dbeta(x = seq(from = 0, to = 1, by = .01), 1, 15), type = "l")



ptm <- proc.time()
# CODE HERER
proc.time() - ptm