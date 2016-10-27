
# Person covariates and abilities
J <- 50                  # number of persons
t <- rnorm(J, 0, 1)  # latent ability for each person

# Item parameters
I <- 10                 # number of items
a <- runif(I, .75, 1.25) # item discrimination parameter


#==============================================================================#
#  TO DO!
#  Generalize b_uncentered to accomoate k thersholds
#==============================================================================#

# determines the number of thesholds per item
k <- sample(x = 1:3, size = I, replace = T, prob = c(.5, .35, .15))


# bs are the thresholds, for three outcomes, there are 2 betas (step functions)
# Empty I * K matrix where I is number of items and K is number of thresholds
# For varied numbed of responses, this will need to be a list.
b_uncentered <- matrix(NA, nrow = I, ncol = k)

# First threholds gets increasingly more difficulty 
# Thresholds could be sampled from some distribution e.g., rnorm().
b_uncentered[, 1] <- rnorm(I, 0, 1) 

# The 2nd threshold is the first threshold + a difference .
# Could be also be sampled from a distribution e.g., runif(). 
b_uncentered[, 2] <- b_uncentered[, 1] + runif(I, .2, .7)
mean(b_uncentered)

# Cumulative proabilities such that the average is 0
b <- b_uncentered - mean(b_uncentered)
round(mean(b), 3)

b <- list(i01 = c(-.3, .2), 
	      i02 = c(.25),
	      i03 = c(-.75, -.1, .3),
	      i04 = c(.4),
	      i05 = c(-.2, .3, .65), 
	      i06 = c(1.2),
	      i07 = c(-1.1),
	      i08 = c(.1, .7, 1.4),
	      i09 = c(-1.4, -.9, -.2),
	      i10 = c(4., .9, 1.5) )

b_mean <- mean(unlist(b))

b_center <- lapply(b, function(x) x - b_mean
round(mean(unlist(b_center)))
#=============================================================================#
## TO DO!  
##  Generate betas based on a random number of responses (some will have 2, some will have 3)
##    that can also use "known thresholds"
##  Simulate data 
#=============================================================================#

obs <- I * J 
i  <- rep(1 : I, times = J)  
j  <- rep(1 : J, each = I)

# Function to simulate responses
simulate_response <- function(theta, alpha, beta) {
    unsummed <- c(0, alpha * (theta - beta))
    numers <- exp(cumsum(unsummed))
    denom <- sum(numers)
    response_pr <- numers/denom
    y <- sample(1:length(response_pr) - 1, size = 1, prob = response_pr)
    return(y)
}

y <- numeric(obs)

for (n in 1 : obs) y[n] <- simulate_response(theta = t[j[n]], 
	                                         alpha = a[i[n]], 
	                                         beta = b_center[[i[n]]])



#-----------------------------------------------------------------------------#
# How the GPCM works:
#-----------------------------------------------------------------------------#

# unsummed = 0.000000 1.544215 1.369215
unsummed <- c(0, a[i[1]] * (t[j[1]] - b_center[[i[1]]]))

# numerators = exp( c(0, 0 + 1.544215, 0 + 1.544215 + 1.369215)
numerators <- exp(cumsum(unsummed))

# denominator = sum(exp( c(0, 0 + 1.544215, 0 + 1.544215 + 1.369215)) = sum(c(1, 4.68, 18.42))
denominator <- sum(numerators)

# response_probs = c(1 / 24.10418, 4.68 / 1 / 24.10418, 18.42 / 24.10418)
response_probs <- numerators/denominator

# 1. Sample size is all possible outcomes: (0, 1, ..., m)
# 2. Keep only 1 since there can only be one score
# 3. Use the subjects probablity of response for each category as a weight for keeping that response

# 1:length(response_probs) - 1 = (1, 2, 3) - 1 =  0 1 2
# p(y_ij = 0) = response_probs[1] = 0.04148658, 
# p(y_ij = 1) = response_probs[2] = 0.19433537 , 
# p(y_ij = 2) = response_probs[2] = 0.76417805
simulated_y <- sample(1:length(response_probs) - 1, size = 1, prob = response_probs)


