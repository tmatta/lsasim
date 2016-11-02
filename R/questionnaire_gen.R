


#------------------------------------------------------------------------------#
#--- Create n correlated categorical and continuous variables -----------------#

questionnaire <- function(n_subj, cat_prop, cor_matrix){
# n is the number of observations
# cat_prop is a list() with a vector of cumulative probabilities ending with 1
# cor_matrix is a correlation matrix

#--- Load functions -----------------------------------------------------------#
  #--- generate data
  mvsn <- function(n, num_x){
    uncor_mat <- matrix(NA, nrow = n, ncol = num_x)
    for (i in 1:num_x) uncor_mat[, i] <- rnorm(n, 0, 1)
    return(uncor_mat)	
  }

#------------------------------------------------------------------------------#
  
  if (is.null(names(cat_prop))) names(cat_prop) <- paste0("q", 1:length(cat_prop)) 
  col_names <- names(cat_prop)

  # Generate uncorrelated standard normals   
  n_vars <- length(cat_prop)
  uncor_dat <- mvsn(n = n_subj, num_x = n_vars)

  # transform to correlated data
  chol_cor_matrix <- chol(cor_matrix)
  cor_dat <- uncor_dat %*% chol_cor_matrix

  # number of categories for each variable in cor_dat
  n_cats <- unlist(lapply(cat_prop, function(x) length(x)))

  # which variables are discrete
  discrete_var <- which(n_cats > 1)

  # replace cumulative pr = 1 with .9999 for computation
  cat_prop <- lapply(cat_prop[discrete_var], function(x) ifelse(x == 1, .9999, x))
  
  # find thresholds assuming standard normal
  var_thresholds <- lapply(cat_prop, function(x) qnorm(x, 0, 1))

  #--- coarsen variables based on thresholds
  discrete_dat <- cor_dat

  for (i in 1:length(discrete_var)) {
    for (j in rev(1:length(var_thresholds[[i]]))) {
      discrete_dat[which(cor_dat[, discrete_var[i]] <= var_thresholds[[i]][j]), 
        discrete_var[i]] <- j
    }
  }  

  discrete_df <- data.frame(discrete_dat)

  # Call first non-integer variable theta. 
  # This could be smarter, perhaps coming in at the top and ensuring it stays continuous. 
  col_names[which(discrete_df[1, ]%%1 != 0)[1]] <- "theta"
  
  colnames(discrete_df) <- col_names
  discrete_df$subject <- 1:nrow(discrete_df)
  return(discrete_df)  # consider returning thresholds, latent vars, correlations...
}

