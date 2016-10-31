# i_num: number of items
# a_range: bounds of the discrimination parameters 
# k_options: vector: number of thresholds
# k_proportions: vector: proportion of each item types
# b_range:  bounds of the difficulty parameter

# proportion of each k (should sum to 1)

item_gen <- function(n_items, b_range, a_range = NULL, k_options = 1, k_proportions = NULL){

  #--- Number of items
  i <- n_items             

  #--- Discrimination parameters
  if (is.null(a_range)) {
  	 a <- rep(1, i)
    } else {
      a <- round(runif(i, a_range[1], a_range[2]), 2)
    }

  #-- Number of thresholds and their proportions
  k <- sample(x = k_options, size = i, replace = T, prob = k_proportions)

  #--- Difficulty parameter(s)
  b <- list()

  for(p in 1: i){
    b_i <- list()
  
    if (k[p] != 1){  
      
      b_i[[1]] <- runif(1, min = b_range[1], max = b_range[2])
      
      for (j in 2 : k[p]){
        b_i[[j]] <- b_i[[(j - 1)]] + runif(1, min = b_range[1] + .5, max = (b_range[2] / 2))
      }
    
    } else {
  
      b_i[[1]] <- runif(1, min = b_range[1], max = b_range[2])
    
    }
    
    b[[p]] <- unlist(b_i)
  
  }

  b_mean <- mean(unlist(b))
  b_center <- lapply(b, function(x) round(x - b_mean, 2))

  return(list(b = b_center, a = a))

}

