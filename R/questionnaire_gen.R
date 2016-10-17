
library(polycor)

#--- Create 1 categorical variable from standard normal ---#
x1 <- rnorm(10, 0, 1)

t1 <- qnorm(.2, 0, 1)
t2 <- qnorm(.6, 0, 1)
t3 <- qnorm(.8, 0, 1)

z1 <- ifelse(x1 <= t1, 1, 
	    ifelse(x1 <= t2, 2,
		  ifelse(x1 <= t3, 3, 4)))

cbind(x1, z1)

prop.table(table(z1))

#--- Create 2 correlated categorical variables from standard normals ---#
x1 <- rnorm(1000, 0, 1)
x2 <- rnorm(1000, 0, 1)
xx <- cbind(x1, x2)
cor(xx)

cor_mat <- matrix(c(1, .75, .75, 1), byrow = TRUE, nrow = 2)
l <- chol(cor_mat)

xx2 <- xx %*% l
cor(xx2)

mean(xx2[, 1]) - mean(x1)
var(xx2[, 1]) - var(x1)

mean(xx2[, 2]) - mean(x2)
var(xx2[, 1]) - var(x2)

t11 <- qnorm(.25, 0, 1)
t12 <- qnorm(.25+.25, 0, 1)
t13 <- qnorm(.25+.25+.45, 0, 1)

t21 <- qnorm(.35, 0, 1)

z1 <- ifelse(xx2[, 1] <= t11, 1, 
	    ifelse(xx2[, 1] <= t12, 2,
		  ifelse(xx2[, 1] <= t13, 3, 4)))

z2 <- ifelse(xx2[, 2] <= t21, 1, 2)

zz <- cbind(z1, z2)
margin.table(prop.table(table(z1, z2)),1)
margin.table(prop.table(table(z1, z2)),2)

polychor(z1, z2,  ML = FALSE, std.err = FALSE)
polychor(z1, z2,  ML = TRUE, std.err = TRUE)
cor(z1, z2, method="kendall", use="pairwise") 


#------------------------------------------------------------------------------#
#--- Create n correlated categorical and continuous variables -----------------#

questionnaire <- function(n, cat_prop, rho_vec){
# n is the number of observations
# cat_prop is a list() with a vector of cumulative probabilities ending with 1
# rho_vec is a vector of correlations where rho_vec = vechs(R)

#--- Load functions -----------------------------------------------------------#
  #--- generate data
  mvsn <- function(n, num_x){
    uncor_mat <- matrix(NA, nrow = n, ncol = num_x)
    for (i in 1:num_x) uncor_mat[, i] <- rnorm(n, 0, 1)
    return(uncor_mat)	
  }

  # build correlation matrix from strict row vectorization
  cor_mat <- function(lower_tri_cor, num_x){
    cor_mat <- diag(1, num_x)  
    cor_mat[lower.tri(cor_mat)] <- lower_tri_cor
    cor_mat <- cor_mat + t(cor_mat) - diag(diag(cor_mat))
    return(cor_mat) 
  }
#------------------------------------------------------------------------------#
  col_names <- names(cat_prop)
  # Generate uncorrelated standard normals   
  n_vars <- length(cat_prop)
  uncor_dat <- mvsn(n = n, num_x = n_vars)

  # construct correlation matrix from rho_vec
  cor_matrix <- cor_mat(lower_tri_cor = rho_vec, num_x = n_vars)
  chol_cor_matrix <- chol(cor_matrix)

  # transform to correlated data
  cor_dat <- uncor_dat %*% chol_cor_matrix
  # cor(mvn)
  # mean(mvn[,1])

  # number of categories for each variable in cor_dat
  n_cats <- unlist(lapply(cat_pr, function(x) length(x)))

  # which variables are discrete
  discrete_var <- which(n_cats > 1)

  # replace cumulative pr = 1 with .9999 for computation
  cat_prop <- lapply(cat_prop[discrete_var], function(x) ifelse(x == 1, .9999, x))
  
  # find thresholds assuming standard normal
  var_thresholds <- lapply(cat_prop[discrete_var], function(x) qnorm(x, 0, 1))

  #--- coarsen variables based on thresholds
  discrete_dat <- cor_dat

  for (i in 1:length(discrete_var)) {
    for (j in rev(1:length(var_thresholds[[i]]))) {
      discrete_dat[which(cor_dat[, discrete_var[i]] <= var_thresholds[[i]][j]), 
        discrete_var[i]] <- j
    }
  }  

  discrete_df <- data.frame(discrete_dat)
  colnames(discrete_df) <- col_names
  return(discrete_df)  # consider returning thresholds, latent vars, correlations...
}

#--- Test questionnaire function ----------------------------------------------#

# strict half vectorization of correlation matrix
rho_v <- c(.6, .75, .5)

#  Named list containing cumulative proportions for each category of each covariate
cat_pr <- list(ethicity = c(.3, .7, 1), 
	           gender =  c(.4, 1), 
	           theta = c(1))

questionnaire(n = 100, cat_prop = cat_pr, rho_vec = rho_v)
