# Number of items
# Number of responses per item
# Difficulty of items
# Discrimination of items
# Sampling of items



#--- Person covariates and abilities
j <- 50                  # number of persons
t <- rnorm(J, 0, 1)  # latent ability for each person


#--- Item parameters
i <- 10                 # number of items
a <- runif(i, .75, 1.25) # item discrimination parameter
k <- sample(x = 1:3, size = i, replace = T, prob = c(.5, .35, .15))

2