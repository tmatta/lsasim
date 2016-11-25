#==============================================================================#
# Generates item parameters from uniform distribution
# User specified the bounds of the parameters 
# Currently, b parameter is constrained to keep partial credit items in range
#==============================================================================#

# n_items: number of items.  vector of numbers of items when k_oprions is used
# b_bounds: bounds of the difficulty parameter
# a_bounds: bounds of the discrimination parameters 
# c_bounds: bounds of the guessing parameter
# k_options: vector: number of thresholds

# proportion of each k (should sum to 1)

item_gen <- function(n_items, 
                     b_bounds, a_bounds = NULL, c_bounds = NULL, 
                     k_options = 1){

  #--- Number of items
  i <- sum(n_items)             
  #-- Number of thresholds per item
  k <- c(rep(k_options, n_items))
  
  #----------------------------------------------------------------------------#
  #--- Discrimination parameters
  if (is.null(a_bounds)) {
  	 a_par <- rep(1, i)
    } else {
      a_par <- round(runif(i, a_bounds[1], a_bounds[2]), 2)
    }

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
      # dividing the b_bounds[2] by 5 helps keep partial credit items from getting too big.
      b_i[[1]] <- runif(1, min = b_bounds[1], max = (b_bounds[2] * 0.2)) 
      
      for (j in 2 : k[p]){
        d <- runif(1, min = .1, max = (b_bounds[2] * 0.8))  # must be positive
        b_i[[j]] <- b_i[[(j - 1)]] + d
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

