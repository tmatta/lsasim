#==============================================================================#
# test_4.R
# This syntax utilizes the default settings to generate backgroud data
#==============================================================================#

#--- Set directory ------------------------------------------------------------#

#--- Windows
setwd("Dropbox/Research/ilsasim")

#--- OSX
# setwd("Dropbox/Research/ilsasim")

#--- Source function ----------------------------------------------------------#
source("R/population_pars.R")     # generate population parameter for questionnaire_gen 
source("R/questionnaire_gen.R")   # generate questionnaire data, including theta

#=== Parameters ===============================================================#
n_subj   <- 10                              # number of students
resp_typs <- c(1, 3, 5)
n_typs <- c(6, 2, 2)
n_vars   <- length(rep(resp_typs, n_typs))  # number of questionnaire variables

#==============================================================================#
#=== Test 1 ===================================================================#
#--- Generate marginal proportions 
cat_pr1 <- NULL
cat_pr1 <- gen_proportions(cat_options = resp_typs, 
                           n_cat_options = n_typs)

#--- Generate correlation matrix 
(q1 <- round(rand_pd_corr(n_var = 4),2))


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

matrix(c(1, .6, .6, 1), nrow = 2)

questionnaire(n = 5, cat_prop = list(c(1), c(.25, .6, 1)), 
              cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2), theta = TRUE)

ptm <- proc.time()
# CODE HERER
proc.time() - ptm




(q1 <- round(rand_pd_corr(n_var = 4),2))
df <- questionnaire(n = 50, 
                    cat_prop = list(c(1), 
                                    c(1),
                                    c(.25, .6, .8, 1),
                                    c(.45, 1)), 
                    cor_matrix = q1)

df$p1 <- round((df$p1 * 10) + 50, 2)
df$p2 <- round((df$p2 * 15) + 50, 2)
df$p3 <- factor(df$p3, levels = c(1, 2, 3, 4), labels = c("strongly disagree", "disagree", "agree", "strongly agree"))
df$p4 <- factor(df$p4, levels = c(1, 2), labels = c("male", "female"))

write.csv2(df, file = "finals_data.csv", row.names = FALSE)

mean(df$p1)
sd(df$p1)

mean(df$p2)
sd(df$p2)

cor(df$p1, df$p2)
table(df$p3, df$p4)

summary(df$p1[which(df$p4 == "female")])
summary(df$p1[which(df$p4 == "male")])