response_data <- function(n_subj, n_items, theta, alpha = NULL, beta){

  if (is.null(alpha)) alpha <- rep(1, n_items)

  obs <- n_subj * n_items 
  i  <- rep(1 : n_items, times = n_subj)   # item i
  j  <- rep(1 : n_subj, each = n_items)    # subject j
  
  y <- numeric(obs)
  
  for (n in 1 : obs) y[n] <- simulate_response(theta = theta[j[n]], 
                                               alpha = alpha[i[n]], 
                                               beta  = beta[[i[n]]])

  df <- data.frame(item = i, subject = j, y)
  return(y = df)
}
