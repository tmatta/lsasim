# i_num: number of items
# a_range: bounds of the discrimination parameters 
# k_options: vector: number of thresholds
# k_proportions: vector: proportion of each item types
# b_range:  bounds of the difficulty parameter

# proportion of each k (should sum to 1)

item_gen <- function(n_items, 
                     b_bounds, a_bounds = NULL, c_bounds = NULL, 
                     k_options = 1, k_proportions = NULL){

  #--- Number of items
  i <- n_items             

  #----------------------------------------------------------------------------#
  #--- Discrimination parameters
  if (is.null(a_bounds)) {
  	 a_par <- rep(1, i)
    } else {
      a_par <- round(runif(i, a_bounds[1], a_bounds[2]), 2)
    }

  #-- Number of thresholds and their proportions
  k <- sample(x = k_options, size = i, replace = T, prob = k_proportions)
  
  #----------------------------------------------------------------------------#
  #--- Guessing parameter, 0 if k > 1 
  if (is.null(c_bounds)) {
    c_par <- rep(0, i)
  } else {
    c_par <- ifelse(k == 1, round(runif(i, c_bounds[1], c_bounds[2]), 2), 0)
  }
  
  #----------------------------------------------------------------------------#
  #--- Difficulty parameter(s)
  b_par <- list()

  for (p in 1: i) {
    b_i <- list()
  
    if (k[p] != 1){  
      
      b_i[[1]] <- runif(1, min = b_bounds[1], max = b_bounds[2])
      
      for (j in 2 : k[p]){
        b_i[[j]] <- b_i[[(j - 1)]] + runif(1, min = b_bounds[1] + .5, max = (b_bounds[2] / 2))
      }
    
    } else {
  
      b_i[[1]] <- runif(1, min = b_bounds[1], max = b_bounds[2])
    
    }
    
    b_par[[p]] <- unlist(b_i)
  
  }

  b_mean <- mean(unlist(b_par))
  b_center <- lapply(b_par, function(x) round(x - b_mean, 2))

  #----------------------------------------------------------------------------#
  return(list(b_par = b_center, a_par = a_par, c_par = c_par))

}

