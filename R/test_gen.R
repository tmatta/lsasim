response_gen <- function(subject, item, theta, alpha = NULL, beta){

  if (is.null(alpha)) alpha <- rep(1, length(item))
 
  y <- numeric(length(subject))
  
  for (n in 1 : length(subject)) {
    y[n] <- gpcm_gen(theta = theta[subject[n]], 
                     alpha = alpha[item[n]], 
                     beta  = beta[[item[n]]])
  }

  df <- data.frame(item = item, subject = subject, response = y)
  return(y = df)
}

