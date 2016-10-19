library(polycor)

setwd("C:\\Users\\progi_adm\\Dropbox\\Research\\ilsasim")


#------------------------------------------------------------------------------#
#--- Create 1 categorical variable from standard normal -----------------------#
x1 <- rnorm(10, 0, 1)

t1 <- qnorm(.2, 0, 1)
t2 <- qnorm(.6, 0, 1)
t3 <- qnorm(.8, 0, 1)

z1 <- ifelse(x1 <= t1, 1, 
	    ifelse(x1 <= t2, 2,
		  ifelse(x1 <= t3, 3, 4)))

cbind(x1, z1)

prop.table(table(z1))

#------------------------------------------------------------------------------#
#--- Create 2 correlated categorical variables from standard normals ----------#
x1 <- rnorm(1000, 0, 1)
x2 <- rnorm(1000, 0, 1)
xx <- cbind(x1, x2)
cor(xx)

cor_mat <- matrix(c(1, .75, .75, 1), byrow = TRUE, nrow = 2)
l <- chol(cor_mat)

xx2 <- xx %*% l
cor(xx2)

mean(xx2[, 1]) - mean(x1)
var(xx2[, 1]) - var(x1)

mean(xx2[, 2]) - mean(x2)
var(xx2[, 1]) - var(x2)

t11 <- qnorm(.25, 0, 1)
t12 <- qnorm(.25+.25, 0, 1)
t13 <- qnorm(.25+.25+.45, 0, 1)

t21 <- qnorm(.35, 0, 1)

z1 <- ifelse(xx2[, 1] <= t11, 1, 
	    ifelse(xx2[, 1] <= t12, 2,
		  ifelse(xx2[, 1] <= t13, 3, 4)))

z2 <- ifelse(xx2[, 2] <= t21, 1, 2)

zz <- cbind(z1, z2)
margin.table(prop.table(table(z1, z2)),1)
margin.table(prop.table(table(z1, z2)),2)

polychor(z1, z2,  ML = FALSE, std.err = FALSE)
polychor(z1, z2,  ML = TRUE, std.err = TRUE)
cor(z1, z2, method="kendall", use="pairwise") 


#------------------------------------------------------------------------------#
#--- Test questionnaire function ----------------------------------------------#
source("R\\questionnaire_gen.R")

#--- number of variables and observations
n_var <- 200
n_obs <- 20000

#--- random pos-semi-def corr matrix
R <- matrix(runif(n_var*n_var), ncol=n_var) 
RxR <- R %*% t(R) 
Q <- cov2cor(RxR) 

#--- random cumulative proportions
cat_pr <- list()
rand_n_cats <- matrix(NA, nrow = n_var) 
max_pr <- c(.49, .48, .38, .28, .18)

for(i in 1: n_var){
  rand_n_cats[i] <- round(runif(1, min = 1, max = 5))
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

ptm <- proc.time()
df1 <- questionnaire(n = n_obs, cat_prop = cat_pr, cor_matrix = Q)
proc.time() - ptm

# >    user  system elapsed 
#      1.71    0.00    1.73 

str(df1)