
home_dir <- "tylerm"
setwd(paste0("C:\\Users\\", home_dir, "\\Dropbox\\Research\\ilsasim"))

### EXPLORE TAM

#--- source function ----------------------------------------------------------#
source("R\\population_pars.R")     # generate population parameter for questionnaire_gen 
source("R\\questionnaire_gen.R")   # generate questionnaire data, including theta
source("R\\item_gen.R")            # generate item parameters (if you don't have your own!)
source("R\\irt_gen.R")             # generate responses based on theta and item parameters
source("R\\book_gen.R")            # generate booklet design (can provide your own boo)
source("R\\response_gen.R")        # generate response data
source("R\\test_assembly.R")       # generate response data

#=== Small test ===============================================================#
n_subj   <- 10                  # number of students
n_vars   <- 20                  # number of questionnaire variables
n_forms  <- 16                   # number of test forms
form_len <- 10                   # number of items per form
n_items  <- n_forms * form_len  # number of total items

ptm <- proc.time()
#--- survey data
cat_pr1 <- rand_cum_proportions(n_var = n_vars, max_category = 5)
q1 <- rand_pd_corr(n_var = n_vars)

surv1 <- questionnaire(n = n_subj, cat_prop = cat_pr1, cor_matrix = q1)

#--- test_assembly(), all default
test1 <- test_assembly(n_subj = n_subj, n_forms = n_forms, form_length = form_len)

#--- item parameter generation partial credit
genGPCM <- item_gen(n_items   = n_forms * form_len, 
                    b_bounds  = c(-2, 2),
                    a_bounds  = c(-.5, 1.75),
                    c_bounds  = c(0, 2), 
                    k_options = 1:3, 
                    k_proportions = c(.5, .3, .2))

#--- item responses 
datGPCM <- response_gen(subject = test1$item_assign$subj, 
                        item    = test1$item_assign$item, 
                        theta   = surv1$theta, 
                        b_par   = genGPCM$b,
                        a_par   = genGPCM$a)

final_data <- merge(surv1, datGPCM, by = "subject")
proc.time() - ptm

str(final_data)

#=== Large Test ===============================================================#


#--- Large, Input booklet design 
spiral <- c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,1, 1,5, 2,6, 3,7, 4,8)
books <- matrix(spiral, ncol = 2, byrow = T)

test3 <- test_assembly(n_subj = 1000, n_forms = 8, form_length = 10, book_design = books)
str(test3)
head(test3$item_assign)
head(test3$book_assign)
test3$items_per_book



#=== IRT Models ===============================================================#

n_items  <- 50  # number of total items

#--- Rasch
#--- Only use item gen if you do not have item parameters. 
gen1PL <- item_gen(n_items = n_items, 
                   b_bounds = c(-2, 2))

dat1PL <- response_gen(subject = test1$item_assign$subject, 
                       item  = test1$item_assign$item, 
                       theta = theta, 
                       b_par = gen1PL$b_par)
str(dat1PL)


#--- Rasch partial credit
genRPCM <- item_gen(n_items       = n_forms * form_len, 
                    b_bounds       = c(-2, 2),
                    k_options     = 1:3, 
                    k_proportions = c(.5, .3, .2))

datRPCM <- response_gen(subject = item_assign_l$subj, 
                        item    = item_assign_l$item, 
                        theta   = theta, 
                        b_par  = genRPCM$b,
                        a_par  = genRPCM$a)

#--- 2PL
gen2PL <- item_gen(n_items = n_forms * form_len, 
                   b_bounds = c(-2, 2),
                   a_bounds = c(-.5, 1.75))

dat2PL <- response_gen(subject = item_assign_l$subj, 
                       item    = item_assign_l$item, 
                       theta   = theta, 
                       b_par  = gen2PL$b,
                       a_par  = gen2PL$a)

#--- 3PL
gen3PL <- item_gen(n_items = n_forms * form_len, 
                   b_bounds = c(-2, 2),
                   a_bounds = c(-.5, 1.75),
                   c_bounds = c(0, 2))

dat3PL <- response_gen(subject = item_assign_l$subj, 
                       item    = item_assign_l$item, 
                       theta   = theta, 
                       b_par = gen3PL$b_par,
                       a_par = gen3PL$a_par,
                       c_par = gen3PL$c_par)

#--- General partial credit
genGPCM <- item_gen(n_items   = n_forms * form_len, 
                    b_bounds  = c(-2, 2),
                    a_bounds  = c(-.5, 1.75),
                    c_bounds  = c(0, 2), 
                    k_options = 1:3, 
                    k_proportions = c(.5, .3, .2))

datGPCM <- response_gen(subject = item_assign_l$subj, 
                        item    = item_assign_l$item, 
                        theta   = theta, 
                        b_par  = genGPCM$b,
                        a_par  = genGPCM$a)




#=== Speed tests ==============================================================#

ptm <- proc.time()
surv1 <- questionnaire(n_subj = n_obs, cat_prop = cat_pr1, cor_matrix = q1)
proc.time() - ptm











mydf <- data.frame(matrix(1:12, ncol = 4))
colnames(mydf)[2] <- "theta"
colnames(mydf)[which(colnames(mydf)!="theta")] <- paste0("p", 1:(ncol(mydf)-1))

mydf <- mydf[, c("theta", paste0("p", 1:(length(mydf)-1)))]

sub <- mydata[!(mydata$group %in% c("A", "B", "E", "G")),] 