# i_num: number of items
# a_range: bounds of the discrimination parameters 
# k_options: vector: number of thresholds
# k_proportions: vector: proportion of each item types
# b_range:  bounds of the difficulty parameter

# proportion of each k (should sum to 1)

item_gen <- function(i_num, b_range, a_range = NULL, k_options = 1, k_proportions = NULL){

  #--- Number of items
  i <- i_num             

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

#=== Some tests ===============================================================#
#--- Rasch
item_gen(i_num = 15, 
	     b_range = c(-2, 2))

#--- Rasch partial credit
item_gen(i_num = 15, 
	     b_range = c(-2, 2),
	     k_options = 1:3, 
	     k_proportions = c(.5, .3, .2))

#-- 2PL
item_gen(i = 15, 
         b_range = c(-2, 2),
	     a_range = c(-.5, 1.75))

#--- General partial credit
item_gen(i = 15, 
         b_range = c(-2, 2),
	     a_range = c(-.5, 1.75), 
	     k_options = 1:3, 
	     k_proportions = c(.5, .3, .2))


