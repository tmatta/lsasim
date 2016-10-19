# Some Item Reponse Models

# Generate multiple blocks
# Assign blocks
# Common items / common block

# Generate item parameters for i items
i <- 25
item_pars <- cbind(rnorm(i, 0, 1), runif(i, .5, 1.5))
theta <- rnorm(50, 0, 1)

# a = 1, c = 0 returns the rasch model 
# D = 1.7 transforms the reponse function to be normal
irt_resp <- function(t, a = 1, b, c = 0, d = 1, D = 1) {
   return( c + (d-c) / (1 + exp(-a * D * (t - b))) )
   }

# Generate response data
resp <- lapply(theta, 
	           function(x) round(irt_resp(t = x, 
	   	                                  b = item_pars[, 1], 
	   	                                  a = item_pars[, 2]))
	   )
