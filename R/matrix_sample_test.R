
home_dir <- "tylerm"
setwd(paste0("C:\\Users\\", home_dir, "\\Dropbox\\Research\\ilsasim"))

home_dir <- "tmatta"
setwd(paste0("/Users/", home_dir, "/Dropbox/Research/ilsasim"))

source("R\\item_gen.R")
source("R\\gpcm_gen.R")
source("R\\test_gen.R")

#=== Some tests ===============================================================#


# Person covariates and abilities
jj <- 50    # number of persons
nn <- 100    # number of items

# theta
theta <- rnorm(jj, 0, 1)  # latent ability for each person

#--- Rasch
gen1 <- item_gen(n_items = nn, 
                 b_range = c(-2, 2))

rep1 <- response_data(n_subj = jj, 
                      n_items = nn, 
                      theta = theta, 
                      beta = gen1$b)
        
head(rep1)


#--- Rasch partial credit
item_gen(i_num = 15, 
         b_range = c(-2, 2),
         k_options = 1:3, 
         k_proportions = c(.5, .3, .2))

#-- 2PL
item_gen(i = 15, 
         b_range = c(-2, 2),
         a_range = c(-.5, 1.75))

#--- General partial credit
item_gen(i = 15, 
         b_range = c(-2, 2),
         a_range = c(-.5, 1.75), 
         k_options = 1:3, 
         k_proportions = c(.5, .3, .2))



n_forms <- 5
form_len <- 20
n_items <- n_forms*form_len

form <- seq(1, n_items, n_forms)

#--- Can add an extra row of 1s to link all forms.
#--- empry matrix
item_matrix <- matrix(NA, nrow = n_items, ncol = n_forms)

#--- Spiral Design
for (k in 1:n_forms){
  # items in forms x
  form <- seq(k, n_items, n_forms)
  item_matrix[form, k] <-  1
  item_matrix[, k] <- ifelse(is.na(item_matrix[, k]), 0, item_matrix[, k])
}

# Number of items that are not used in the test. 
sum(rowMeans(item_matrix) == 0)

