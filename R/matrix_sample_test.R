
home_dir <- "tylerm"
setwd(paste0("C:\\Users\\", home_dir, "\\Dropbox\\Research\\ilsasim"))

home_dir <- "tmatta"
setwd(paste0("/Users/", home_dir, "/Dropbox/Research/ilsasim"))

source("R\\item_gen.R")
source("R\\gpcm_gen.R")
source("R\\test_gen.R")
source("R\\book_gen.R")

#=== Some tests ===============================================================#

#--- [THIS WILL COME FROM QUESTIONNAIRE] ---#
n_subj <- 50
theta <- rnorm(n_subj, 0, 1)  # latent ability for each person


#--- Construct booklets 
n_forms <- 8
form_len <- 5
spiral <- c(1,2, 2,3, 3,4, 4,5, 5,6, 6,7, 7,8, 8,1, 1,5, 2,6, 3,7, 4,8)

books <- matrix(spiral, ncol = 2, byrow = T)
booklets <- book_gen(n_forms = n_forms, form_length =form_len5, book_design = books)


#--- distribute books randomly to subject
pr_dist <- .1
x <- 0

#--- make sure books are distributed equally
while (pr_dist > .02) { 
  subj_book <- sample(1:length(booklets), size = n_subj, replace = T)
  book_dist <- prop.table(table(subj_book))
  pr_dist <- max(book_dist) - min(book_dist)

  x <- x + 1
  if (x < 20) {
     cat("iteration ", x, " pr_dist ", pr_dist, "\n" )  
  } else if (x == 20){
     cat("iteration ", x, " pr_dist ", pr_dist, "\n Sampling terminated \n" )
     break
   }
}

#--- merge subject id with book id
book_assign <- cbind(subj = 1:n_subj, book = subj_book) 
booklet_mat <- do.call(rbind, booklets) # turn list into matrix


#--- merge item numbers with subject id and book id
row.names(booklet_mat) <- 1:length(booklets) # specify row names to use for merge

item_assign <- merge(book_assign, booklet_mat, by.x = "book", by.y = "row.names")
item_assign <- item_assign[order(item_assign[, "subj"]), ]

#--- reshape to process through response_gen()
item_assign_l <- reshape(item_assign, 
                         varying = colnames(item_assign[, 3:ncol(item_assign)]), 
                         v.names = "item",
                         timevar = NULL, 
                         times = seq(3:ncol(item_assign)), 
                         direction = "long")

item_assign_l <- item_assign_l[order(item_assign_l$subj),]
rownames(item_assign_l) <- item_assign_l$id <- NULL



#=== TESTS ====================================================================#
#--- Rasch
gen1PL <- item_gen(n_items = n_forms * form_len, 
                   b_range = c(-2, 2))

dat1PL <- response_gen(subject = item_assign_l$subj, 
                       item    = item_assign_l$item, 
                       theta   = theta, 
                       beta    = gen1PL$b)
        
head(rep1, 25)


#--- Rasch partial credit
genRPCM <- item_gen(n_items       = n_forms * form_len, 
                    b_range       = c(-2, 2),
                    k_options     = 1:3, 
                    k_proportions = c(.5, .3, .2))

datRPCM <- response_gen(subject = item_assign_l$subj, 
                        item    = item_assign_l$item, 
                        theta   = theta, 
                        beta    = genRPCM$b,
                        alpha   = genRPCM$a)

#--- 2PL
gen2PL <- item_gen(n_items = n_forms * form_len, 
                   b_range = c(-2, 2),
                   a_range = c(-.5, 1.75))

dat2PL <- response_gen(subject = item_assign_l$subj, 
                       item    = item_assign_l$item, 
                       theta   = theta, 
                       beta    = gen2PL$b,
                       alpha   = gen2PL$a)


#--- General partial credit
genGPCM <- item_gen(n_items       = n_forms * form_len, 
                    b_range       = c(-2, 2),
                    a_range       = c(-.5, 1.75), 
                    k_options     = 1:3, 
                    k_proportions = c(.5, .3, .2))

datGPCM <- response_gen(subject = item_assign_l$subj, 
                        item    = item_assign_l$item, 
                        theta   = theta, 
                        beta    = genGPCM$b,
                        alpha   = genGPCM$a)


