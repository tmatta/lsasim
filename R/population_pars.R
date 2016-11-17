

rand_pd_corr <- function(n_var){ 
  #--- random pos-semi-def corr matrix
  R <- matrix(runif(n_var*n_var, -1, 1), ncol=n_var) 
  RxR <- R %*% t(R) 
  Q <- cov2cor(RxR) 
  return(Q)
}

gen_proportions <- function(cat_options, n_cat_options){
  #- n_var: number of variables
  #- cat_options: list of response options in the survey
  #- n_cat_options: list of how many of each response option is in the survey

  if ( length(cat_options) > length(n_cat_options) | length(cat_options) < length(n_cat_options) ) {
      stop("cat_options and n_cat_options are not the same length", call. = FALSE)
  }
  if (any(cat_options %% 1 != 0)) stop("Elements of cat_options must be integers", call. = FALSE)
  if (any(n_cat_options %% 1 != 0)) warning("Elements of n_cat_options less than 1 and will be treated as 0", call. = FALSE)

  #--- random cumulative proportions
  cat_pr <- list()
  
  var_response_options <- c(rep(cat_options, n_cat_options))
  n_var <- length(var_response_options)

  for(i in 1: n_var){

    rand_pr <- list()
  
    if (var_response_options[i] != 1){  
      
      rand_pr[[1]] <- round(runif(1, min = 0, max = rbeta(1, 10, 1)), 2)  
      
      for (j in 2 : var_response_options[i]){
        k <- j - 1
        rand_pr[[j]] <- round(runif(1, min = rand_pr[[k]], max =.99), 2)
      }
      
      rand_pr[[var_response_options[i]]] <- 1
    
    } else {
    
      rand_pr[[1]] <- 1
    
    }
    
    cat_pr[[i]] <- unlist(rand_pr)
  }
  return(cat_pr)
}
