#==============================================================================#
# test_4.R
# This syntax tests the functionality of the questionnaire() function
#==============================================================================#
library(polycor)
#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/lsasim")

set.seed(5656)
options(width = 100)

#--- Source function ----------------------------------------------------------#
source("R/proportion_gen.R")   
source("R/cor_gen.R")   
source("R/questionnaire_polycor.R") 

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
cat_pr <- proportion_gen(cat_options = resp_typs, n_cat_options = n_typs)

#--- Generate correlation matrix 
q <- NULL
q <- cor_gen(n_var = n_vars)

#--- Generate questionnaire data, default theta
dat <- questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q)
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
cat_pr <- proportion_gen(cat_options = resp_typs, n_cat_options = n_typs)

#--- Generate correlation matrix 
q <- NULL
q <- cor_gen(n_var = n_vars)

#--- Generate questionnaire data, default theta
t2 <- questionnaire(n = n_subj, cat_prop = cat_pr, cor_matrix = q, 
  c_mean = c(1, 2, 3, 4, 5, 6), c_sd = c(1, 1.5, 2, 2.5, 3, 3.5))

colMeans(t2[, 2:7])
apply(t2[, 2:7], 2, sd)
round(cor(t2[, 2:7]) - q[1:6, 1:6], 3)

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
cat_pr <- proportion_gen(cat_options = 2, n_cat_options = n_vars)

#--- Generate correlation matrix 
q <- NULL
q <- cor_gen(n_var = n_vars)

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
cat_pr <- proportion_gen(cat_options = resp_typs, n_cat_options = n_typs)
names(cat_pr)[1] <- "var1"
names(cat_pr)[4] <- "var4"
names(cat_pr)[7] <- "var7"

#--- Generate correlation matrix 
q <- NULL
q <- cor_gen(n_var = n_vars)

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



#==============================================================================#
# Test 6: 
# Simultation to show recovery
#==============================================================================#

library(polycor)
#--- parameters

set.seed(5775)
reps <- 100

samp_list <- c(100, 500, 1000)
resp_typs <- c(1, 2, 3)
n_typs    <- c(2, 5, 5)
n_vars    <- length(rep(resp_typs, n_typs))
max_lt <- ((n_vars * (n_vars + 1) / 2) - n_vars)

sim_cor <- array(NA, dim = c(reps, length(samp_list), max_lt))

#--- Generate marginal probabilities 
cat_pr <- NULL
cat_pr <- proportion_gen(cat_options = resp_typs, n_cat_options = n_typs)

#--- Generate correlation matrix 
q <- NULL
q <- cor_gen(n_var = n_vars)
cor_lt <- q[lower.tri(q)]
true_cor <- cor_lt

for (kk in 1:length(samp_list)) {

  for (ii in 1:reps){

    df <- questionnaire(n = samp_list[kk], cat_prop = cat_pr, cor_matrix = q)
     
    df_hetcor <- hetcor(df[, -1], std.err = FALSE)
    df_cor <- df_hetcor$correlations[lower.tri(df_hetcor$correlations)]
  
    sim_cor[ii, kk, ] <- df_cor
  }

message(kk, " of ", length(samp_list), " samp_list complete")
} 



round(colMeans(sim_cor[, 2, ]) - true_cor, 2)


#---  
# N    q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12
# 500   c  c  2  2  2  2  2  5  5   5   5   5
#--
#   [1]  0.00 -0.01 -0.01  0.01 -0.02 -0.03  0.00 -0.01  0.01  0.02 -0.02  0.00
#  [13] -0.01  0.08  0.09  0.09  0.00  0.00  0.00 -0.01 -0.02  0.01  0.03  0.01
#  [25]  0.04 -0.01 -0.01  0.00  0.01  0.00 -0.02 -0.02 -0.05 -0.01  0.01 -0.02
#  [37]  0.02  0.00 -0.05 -0.24  0.06  0.04 -0.01 -0.09 -0.04 -0.01  0.05  0.00
#  [49]  0.00 -0.07 -0.02  0.03  0.03 -0.01  0.20  0.07  0.00 -0.01 -0.05  0.02
#  [61] -0.01 -0.02  0.00  0.01  0.00 -0.05

#---  
# N    q1 q2 q3 q4 q5 q6 q7 q8 q9 q10 q11 q12
# 500   c  c  2  2  2  2  2  3  3   3   3   3
#--
#   [1]  0.01  0.00  0.01  0.03  0.00  0.00  0.06  0.01 -0.01 -0.02 -0.02  0.00
#  [13] -0.01  0.01 -0.05 -0.09  0.01  0.01 -0.03  0.00  0.01  0.00  0.01  0.01
#  [25]  0.03  0.00  0.00  0.03 -0.03 -0.01  0.07  0.05  0.09  0.11  0.04  0.01
#  [37] -0.04 -0.03 -0.02 -0.05 -0.15  0.04  0.01  0.11 -0.01 -0.20 -0.05  0.02
#  [49]  0.06  0.03 -0.10 -0.17  0.06 -0.09  0.05  0.01  0.08  0.03 -0.12 -0.04
#  [61]  0.00 -0.03 -0.01 -0.04 -0.06  0.04