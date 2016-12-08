#==============================================================================#
# test_5.R
# This syntax test the item_gen() function
#==============================================================================#
rm(list = ls())

#--- Set directory ------------------------------------------------------------#
setwd("Dropbox/Research/ilsasim")

source("R/item_gen.R")  

#--- 1PL ----------------------------------------------------------------------#
gen1PL <- item_gen(n_1pl = 10, 
                   b_bounds = c(-2, 2))

#--- 2PL ----------------------------------------------------------------------#
gen2PL <- item_gen(n_2pl = 10, 
                   b_bounds = c(-2, 2),
                   a_bounds = c(.75, 1.25))

hist(gen2PL$b)
#--- 3PL ----------------------------------------------------------------------#
gen3PL <- item_gen(n_3pl = 10, 
                   b_bounds = c(-2, 2),
                   a_bounds = c(.75, 1.25),
                   c_bounds = c(0, .25))

#--- PCM ----------------------------------------------------------------------#
genPCM <- item_gen(n_1pl = c(5, 5), 
                   thresholds = c(1, 2), 
                   b_bounds = c(-2, 2))


#--- GPCM ---------------------------------------------------------------------#
genGPCM <- item_gen(n_2pl = 5, 
                    thresholds = 2, 
                    b_bounds = c(-2, 2),
                    a_bounds = c(.75, 1.25))

#--- 1PL/ PCM -----------------------------------------------------------------#
gen1PL_PCM <- item_gen(n_1pl = c(5, 5), 
                       thresholds = c(1, 2), 
                       b_bounds = c(-2, 2))

#--- 2PL/ GPCM ----------------------------------------------------------------#
gen2PL_GPCM <- item_gen(n_2pl = c(5, 10), 
                        thresholds = c(1, 2), 
                        b_bounds = c(-2, 2),
                        a_bounds = c(.75, 1.25))

#--- 3PL/ GPCM ----------------------------------------------------------------#
gen3PL_GPCM <- item_gen(n_2pl = 5,
                        n_3pl = 5, 
                        thresholds = 2, 
                        b_bounds = c(-2, 2),
                        a_bounds = c(.75, 1.25),
                        c_bounds = c(0, .25))

#--- 1PL/ 2PL -----------------------------------------------------------------#
gen1PL_2PL <- item_gen(n_1pl = 5,
                       n_2pl = 5, 
                       b_bounds = c(-2, 2),
                       a_bounds = c(.75, 1.25))


#--- 2PL/ 3PL -----------------------------------------------------------------#
gen2PL_3PL <- item_gen(n_2pl = 5,
                       n_3pl = 5, 
                       b_bounds = c(-2, 2),
                       a_bounds = c(.75, 1.25),
                       c_bounds = c(0, .25))

#--- 1PL/ PCM / GPCM ----------------------------------------------------------#
gen1PL_PCM_GPCM <- item_gen(n_1pl = c(5, 5),
                            n_2pl = c(0, 5), 
                            thresholds = c(2, 3), 
                            b_bounds = c(-2, 2),
                            a_bounds = c(.75, 1.25))


#--- 1PL/ PCM 2 and 3 k/ 2PM / GPCM 3 and 4 k / 3PL ---------------------------#
gen1PL_PCM_GPCM <- item_gen(n_1pl = c(5, 5, 0, 0),
                            n_2pl = c(5, 0, 5, 5),
                            n_3pl = 5, 
                            thresholds = c(1, 2, 3, 4), 
                            b_bounds = c(-2, 2),
                            a_bounds = c(.75, 1.25),
                            c_bounds = c(0, .25))


#---- Testing seed.  Rereun to test -------------------------------------------#
item_gen(n_1pl = c(5),
         n_2pl = c(5), 
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25),
         seed = 34546)

#--- Testing error / warning messages -----------------------------------------#

item_gen(n_1pl = c(5, 5),
         n_2pl = 5, 
         thresholds = c(1, 2), 
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25))
# Error: Must specify the number of 2PL items for each threshold.


item_gen(n_1pl = 5,
         n_2pl = c(2, 5), 
         thresholds = c(1, 2), 
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25))
# Error: Must specify the number of 1PL items for each threshold.


item_gen(n_3pl = c(5, 4),
         thresholds = c(1, 2), 
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25),
         c_bounds = c(0, .25))
# Error: 3PL items can only have 1 threshold.


item_gen(n_3pl = 5,
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25))
# Warning message:
# Generated 3PL items without setting bounds for the c parameter. All c parameters will be 0. 

item_gen(n_2pl = 5,
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25),
         c_bounds = c(0, .25))
# Warning message:
# No 3PL items are specified. Bounds for the c parameter will be ignored. 
 

item_gen(n_3pl = 5,
         thresholds = c(1, 2), 
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25))
# Error: 3PL items can only have 1 threshold.


item_gen(n_3pl = 5,
         b_bounds = c(-2, 2))
# Warning messages:
# 1: Generated 3PL items without setting bounds for the c parameter. All c parameters will be 0. 
# 2: Generated 3PL items without setting bounds for the a parameter. All a parameters will be 1. 


item_gen(n_2pl = 5,
         b_bounds = c(-2, 2))
# Warning message:
# Generated 2PL items without setting bounds for the a parameter. All a parameters will be 1. 


item_gen(n_1pl = 5,
         b_bounds = c(-2, 2),
         a_bounds = c(.75, 1.25))
# Warning message:
# No 2PL or 3PL items are specified. Bounds for the a parameter will be ignored. 

