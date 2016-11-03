

rand_pd_corr <- function(n_var){ 
  #--- random pos-semi-def corr matrix
  R <- matrix(runif(n_var*n_var), ncol=n_var) 
  RxR <- R %*% t(R) 
  Q <- cov2cor(RxR) 
  return(Q)
}



rand_cum_proportions <- function(n_var, max_category){
  #--- random cumulative proportions
  cat_pr <- list()
  rand_n_cats <- matrix(NA, nrow = n_var) 
  max_pr <- c(.49, .48, .38, .28, .18)

  rand_n_cats[1] <- 1  # make first 1 always. 

  for(i in 1: n_var){

    if (i == 1){
     rand_n_cats[i] <- 1  # make first 1 always. 
    } else {
    rand_n_cats[i] <- round(runif(1, min = 1, max = max_category))
    }

    rand_pr <- list()
  
    if (rand_n_cats[i] != 1){  
      
      rand_pr[[1]] <- round(runif(1, min = .1, max = rand_n_cats[i]*.1), 2)  
      
      for (j in 2 : rand_n_cats[i]){
        k <- j - 1
        rand_pr[[j]] <- rand_pr[[k]] + round(runif(1, min = .1, max = max_pr[rand_n_cats[i]]), 2)
      }
      
      rand_pr[[rand_n_cats[i]]] <- 1
    
    } else {
    
      rand_pr[[1]] <- 1
    
    }
    
    cat_pr[[i]] <- unlist(rand_pr)
  }
  return(cat_pr)
}
