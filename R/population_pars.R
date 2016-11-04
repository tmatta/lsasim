

rand_pd_corr <- function(n_var){ 
  #--- random pos-semi-def corr matrix
  R <- matrix(runif(n_var*n_var), ncol=n_var) 
  RxR <- R %*% t(R) 
  Q <- cov2cor(RxR) 
  return(Q)
}



rand_cum_proportions <- function(n_var, cat_options, cat_proportions){
  #--- random cumulative proportions
  cat_pr <- list()

  rand_n_cats <- sample(x = cat_options, size = n_var, replace = T, prob = cat_proportions)
  rand_n_cats[1] <- 1  # make first 1 always. 

  for(i in 1: n_var){

    rand_pr <- list()
  
    if (rand_n_cats[i] != 1){  
      
      rand_pr[[1]] <- round(runif(1, min = 0, max = rbeta(1, 10, 1)), 2)  
      
      for (j in 2 : rand_n_cats[i]){
        k <- j - 1
        rand_pr[[j]] <- round(runif(1, min = rand_pr[[k]], max =.99), 2)
      }
      
      rand_pr[[rand_n_cats[i]]] <- 1
    
    } else {
    
      rand_pr[[1]] <- 1
    
    }
    
    cat_pr[[i]] <- unlist(rand_pr)
  }
  return(cat_pr)
}
