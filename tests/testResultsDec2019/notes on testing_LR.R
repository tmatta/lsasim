library(lsasim)
lsasim::cluster_gen

n1 <- c(2, 10)
draw_cluster_structure(n1) 

set.seed(1234)
cluster_gen(n1)

set.seed(2345)
data <- cluster_gen(n1, n_X = 0, n_W = list(list(2, 2), 5))
    # This throws an error about non-conformable matrices

data <- cluster_gen(n1, n_X = 0, n_W = list(2, 5))
    # This produces data, but only at the student level - 2
    # question responses

n2 <- c(2, 3, 5)
set.seed(2345)
cluster_gen(n2)

n_s <- 5
n_st <- 20
N_s <- 150
N_st <- 40
n_x <- 2
c_mean <- c(1,2)
sigma <- c(1.25, 1.50)
n_w <- 2
cat_prop <- list(c(.25, .50, 1.00), c(.15, .65, 1.00))
cormat <- matrix(c(1.00, .75, .50, .25, 
                   .75, 1.00, .75, .50,
                   .50,  .75, 1.00, .45,
                   .25, .50, .45, 1.00), 
                 nrow = 4, ncol = 4)


dat <- cluster_gen(n = c(n_s, n_st), 
                   N = c(N_s, N_st))
    # Documentation states that N should
    # be a list. 
    # If N_st is equal across schools, then
    # it should be a vector
    # If N_st varies by school, then the 
    # argument should be list(N_s, c(n_st1,..., n_stn))

# We can reliably produce observations based on a 
# specified cluster structure. But we are struggling
# to produce item responses and data for variables
# at different levels in a reliable way
# The distinction between when to use a vector
# and when to use a list isn't clear. Related,
# it is a challenge to produce a set number
# of variables with data according to a 
# predefined number of variables with specified
# distributions assigned to different levels
# We can't sort out how to change the number
# of questions that are assigned at each level
n2 <- c(2,3,5)
set.seed(2345)
cluster_gen(n2, n_W = c(2, 4, 3))
cluster_gen(n2, n_X = c(0, 0, 1), n_W = list(2, 4, 3))
cluster_gen(n2, n_X = list(0, 0, 1), n_W = list(2, 4, 3))
