
#--- NOTE:  ASSUMES THE FIRST FLOATING VAR IS THETA IF UNLABELED
#--- CHECK TO SEE LAST VAR ROUNDS TO INTEGER
#---  TRY WITH RANDOM 0s in the cululative probabilities. 

#------------------------------------------------------------------------------#
#--- Create n correlated categorical and continuous variables -----------------#

questionnaire <- function(n_subj, cat_prop, cor_matrix, theta = FALSE){
# n is the number of observations
# cat_prop is a list() with a vector of cumulative probabilities ending with 1
# cor_matrix is a correlation matrix

#--- Load functions -----------------------------------------------------------#
  #--- generates data from num_x indpendent standard normals
  mvsn <- function(n, num_x){
    uncor_mat <- matrix(NA, nrow = n, ncol = num_x)
    for (i in 1:num_x) uncor_mat[, i] <- rnorm(n, 0, 1)
    return(uncor_mat)	
  }

#------------------------------------------------------------------------------#
  #--- Generate uncorrelated standard normals   
  n_vars <- length(cat_prop)
  uncor_dat <- mvsn(n = n_subj, num_x = n_vars)

  #--- Correlate data
  chol_cor_matrix <- chol(cor_matrix)
  cor_dat <- uncor_dat %*% chol_cor_matrix

  #--- Number of categories for each variable in cor_dat
  n_cats <- unlist(lapply(cat_prop, function(x) length(x)))

  #--- which variables are discrete
  discrete_var <- which(n_cats > 1)

  #--- Replace cumulative pr = 1 with .9999 for computation
  cat_prop <- lapply(cat_prop[discrete_var], function(x) ifelse(x == 1, .9999, x))
  
  #--- Find thresholds assuming standard normal
  var_thresholds <- lapply(cat_prop, function(x) qnorm(x, 0, 1))

  #--- Coarsen variables based on thresholds
  discrete_dat <- cor_dat

  for (i in 1:length(discrete_var)) {
    for (j in rev(1:length(var_thresholds[[i]]))) {
      discrete_dat[which(cor_dat[, discrete_var[i]] <= var_thresholds[[i]][j]), 
        discrete_var[i]] <- j
    }
  }  

  discrete_df <- data.frame(discrete_dat)
  if (any(discrete_df %% 1 != 0) & theta == TRUE){
    stop("Cannot assign theta, all data are discrete", call. = FALSE)
  }

  #--- If the list cat_prop is not labeld, give it labels q1, ..., qX
  if (is.null(names(cat_prop))) {
  
    if (theta == TRUE){

      #  Call first non-integer variable theta. 
      colnames(discrete_df)[which(discrete_df[1, ] %% 1 != 0)[1]] <- "theta"
      xtra_cols <- paste0("p", 1:(ncol(discrete_df)-1) )
      colnames(discrete_df)[which(colnames(discrete_df) != "theta")] <- xtra_cols
      discrete_df$subject <- 1:nrow(discrete_df)
      discrete_df <- discrete_df[, c("subject", "theta", xtra_cols)]

    } else {

      xtra_cols <- paste0("p", 1:(ncol(discrete_df)) )
      colnames(discrete_df) <- xtra_cols
      discrete_df$subject <- 1:nrow(discrete_df)
      discrete_df <- discrete_df[, c("subject", xtra_cols)]

    }
  
  } else {
  
    discrete_df$subject <- 1:nrow(discrete_df)

  }

  return(discrete_df)  # consider returning thresholds, latent vars, correlations...
}

