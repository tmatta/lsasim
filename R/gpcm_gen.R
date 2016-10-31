
#=============================================================================#
## TO DO!  
##  Generate betas based on a random number of responses (some will have 2, some will have 3)
##    that can also use "known thresholds"
##  Simulate data 
#=============================================================================#


  # Function to simulate responses
  simulate_response <- function(theta, alpha = 1, beta) {
      unsummed <- c(0, alpha * (theta - beta))
      numers <- exp(cumsum(unsummed))
      denom <- sum(numers)
      response_pr <- numers/denom
      y <- sample(1:length(response_pr) - 1, size = 1, prob = response_pr)
      return(y)
  }
  
#-----------------------------------------------------------------------------#
# How the GPCM works:
#-----------------------------------------------------------------------------#

# unsummed = 0.000000 1.544215 1.369215
# unsummed <- c(0, a[i[1]] * (t[j[1]] - b_center[[i[1]]]))

# numerators = exp( c(0, 0 + 1.544215, 0 + 1.544215 + 1.369215)
# numerators <- exp(cumsum(unsummed))

# denominator = sum(exp( c(0, 0 + 1.544215, 0 + 1.544215 + 1.369215)) = sum(c(1, 4.68, 18.42))
# denominator <- sum(numerators)

# response_probs = c(1 / 24.10418, 4.68 / 1 / 24.10418, 18.42 / 24.10418)
# response_probs <- numerators/denominator

# 1. Sample size is all possible outcomes: (0, 1, ..., m)
# 2. Keep only 1 since there can only be one score
# 3. Use the subjects probablity of response for each category as a weight for keeping that response

# 1:length(response_probs) - 1 = (1, 2, 3) - 1 =  0 1 2
# p(y_ij = 0) = response_probs[1] = 0.04148658, 
# p(y_ij = 1) = response_probs[2] = 0.19433537 , 
# p(y_ij = 2) = response_probs[2] = 0.76417805
# simulated_y <- sample(1:length(response_probs) - 1, size = 1, prob = response_probs)


