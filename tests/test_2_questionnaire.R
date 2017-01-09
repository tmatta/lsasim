#==============================================================================#
# test_4.R
# This syntax tests the functionality of the questionnaire() function
#==============================================================================#

#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/ilsasim")

set.seed(5656)
options(width = 100)

#--- Source function ----------------------------------------------------------#
source("R/population_pars.R")   # generate population parameter for questionnaire() 
source("R/questionnaire_gen.R") # generate questionnaire data, including theta


#==============================================================================#
# Test 1: 
# Demonstrates the theta argument when a continuous outcome is present 
#==============================================================================#

#--- parameters
n_subj    <- 10                             
resp_typs <- c(1, 3, 5)
n_typs    <- c(6, 2, 2)
n_vars    <- length(rep(resp_typs, n_typs))

#--- Generate marginal proportions 
cat_pr <- NULL
cat_pr <- gen_proportions(cat_options = resp_typs, n_cat_options = n_typs)

#--- Generate correlation matrix 
q <- NULL
q <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data, default theta
questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q)

#--- Generate questionnaire data, theta = TRUE
questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q, theta = TRUE)

#--- Generate questionnaire data, theta = FALSE (explicit)
questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q, theta = FALSE)


#==============================================================================#
# Test 2: rescaling continuous variables 
# Demonstrates the rescaling arguments for continuous variables
#==============================================================================#

#--- parameters
n_subj    <- 100                          
resp_typs <- c(1, 3)
n_typs    <- c(6, 2)
n_vars    <- length(rep(resp_typs, n_typs)) 

#--- Generate marginal proportions 
cat_pr <- NULL
cat_pr <- gen_proportions(cat_options = resp_typs, n_cat_options = n_typs)

#--- Generate correlation matrix 
q <- NULL
q <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data, default theta
t2 <- questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q, 
  c_mean = c(1, 2, 3, 4, 5, 6), c_sd = c(1, 1.5, 2, 2.5, 3, 3.5))

colMeans(t2[, 2:7])
apply(t2[, 2:7], 2, sd)
round(cor(t2[, 2:7]) - q[1:6, 1:6], 3)

#--- Generate questionnaire data, theta = TRUE
questionnaire(n = n_subj, cat_prop = cat_pr1, cor_matrix = q1, theta = TRUE)

#--- Generate questionnaire data, theta = FALSE (explicit)
questionnaire(n = n_subj, cat_prop = cat_pr1, cor_matrix = q1, theta = FALSE)


#==============================================================================#
# Test 3: 
# Demonstrates use of the theta argument when no continuous outcomes are present 
#==============================================================================#

#--- parameters
n_subj    <- 10                             
resp_typs <- c(1, 3, 5)
n_typs    <- c(6, 2, 2)
n_vars    <- length(rep(resp_typs, n_typs))

#--- Generate marginal probabilities, only discrete variables 
cat_pr <- NULL
cat_pr <- gen_proportions(cat_options = 2, n_cat_options = n_vars)

#--- Generate correlation matrix 
q <- NULL
q <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data, default theta
questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q, theta = FALSE)

#--- Generate questionnaire data, theta = TRUE
questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q, theta = TRUE)
# 11/24/2016: works:
# Error: Cannot assign theta, all data are discrete


#==============================================================================#
# Test 4: named lists                        
# Demonstrates how the questionnaire() function handles a partially named list
#==============================================================================#

#--- parameters
n_subj    <- 100                          
resp_typs <- c(1, 3)
n_typs    <- c(6, 2)
n_vars    <- length(rep(resp_typs, n_typs)) 

#--- Generate marginal probabilities 
cat_pr <- NULL
cat_pr <- gen_proportions(cat_options = resp_typs, n_cat_options = n_typs)
names(cat_pr)[1] <- "var1"
names(cat_pr)[4] <- "var4"
names(cat_pr)[7] <- "var7"

#--- Generate correlation matrix 
q <- NULL
q <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data
questionnaire(n = 10, cat_prop = cat_pr, cor_matrix = q)

#--- Generate questionnaire data with named list when theta = TRUE
questionnaire(n = 10, cat_prop = cat_pr, cor_matrix = q, theta = TRUE)
# 11/24/2016: works.  Ignores theta = TRUE. 


#==============================================================================#
# Test 5: 
# Demonstrates the handling of structured zeros in marginal probabilities
#==============================================================================#

#--- parameters
n_subj    <- 25                          
resp_typs <- 10
n_typs    <- 3
n_vars    <- length(rep(resp_typs, n_typs)) 

#--- Generate marginal probabilities 
cat_pr <- NULL
cat_pr <- gen_proportions(cat_options = n_typs, n_cat_options = n_typs)

#--- Generate correlation matrix 
q <- NULL
q <- rand_pd_corr(n_var = n_vars)

#--- Generate questionnaire data
t5 <- questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q)
table(t5[, 2])
# It you continue to execute the two lines above, you will see that there are
# no responses in the categories with zero probabilities. 
