#' Generation of item parameters from uniform distributions
#'
#' Creates a data frame of item parameters.  
#'
#' @param b_bounds a vector containing the bounds of the the uniform distribution for sampling the difficulty parameters.
#' @param a_bounds a vector containing the bounds of the the uniform distribution for sampling the discrimination parameters. 
#' @param c_bounds a vector containing the bounds of the the uniform distribution for sampling the guessing parameters.
#' @param thresholds if numeric, number of thresholds for 1- and/or 2- parameter dichotomous items, if vector, 
#'        each element is the number of thresholds corresponding to the vector of n_1pl and/or n_2pl. 
#' @param n_1pl if integer, number of 1-parameter dichotomous items, if vector, 
#'        each element is the number of partial credit items corresponding to thresholds number. 
#' @param n_2pl, if integer, number of 2-parameter dichotomous items, if vector, 
#'        each element is the number of generalized partial credit items corresponding to thresholds number.
#' @param n_3pl integer, number of 3-parameter items.
#' 
#' @section Details:
#' The data frame includes two variables \code{p} and \code{k} which indicate the 
#' number of parameters and the number of thresholds, respectively 
#' 
#' @examples
#' item_gen(b_bounds = c(-2, 2), a_bounds = c(.75, 1.25), 
#'   thresholds = c(1, 2, 3), n_1pl = c(5, 5, 5), n_2pl = c(0, 0, 5))
#' item_gen(b_bounds = c(-2, 2), a_bounds = c(.75, 1.25), c_bounds = c(0, .25), 
#'   n_2pl = 5, n_3pl = 5)
#' 
#' @export

item_gen <- function(b_bounds, a_bounds = NULL, c_bounds = NULL, 
                     thresholds = 1,
                     n_1pl = NULL, n_2pl = NULL, n_3pl = NULL){

  #--- ERRORS -----------------------------------------------------------------#
  if (!is.null(n_1pl) & length(n_1pl) < length(thresholds)) stop("Must specify the number of 1PL items for each threshold.", call. = FALSE)
  if (!is.null(n_1pl) & length(n_1pl) > length(thresholds)) stop("Too few thresholds specified for n_1pl.", call. = FALSE)
  if (!is.null(n_2pl) & length(n_2pl) < length(thresholds)) stop("Must specify the number of 2PL items for each threshold.", call. = FALSE)
  if (!is.null(n_2pl) & length(n_2pl) > length(thresholds)) stop("Too few thresholds specified for n_2pl.", call. = FALSE)
  if ( length(n_3pl) > 1) stop("3PL items can only have 1 threshold.", call. = FALSE)
  if (!is.null(n_3pl) & is.null(n_1pl) & is.null(n_2pl) & sum(thresholds) > 1) stop("3PL items can only have 1 threshold.", call. = FALSE) 
  #--- WARNINGS ---------------------------------------------------------------#
  if (!is.null(a_bounds) & is.null(n_2pl) & is.null(n_3pl)) warning("No 2PL or 3PL items are specified. Bounds for the a parameter will be ignored.", call. = FALSE) 
  if (!is.null(c_bounds) & is.null(n_3pl)) warning("No 3PL items are specified. Bounds for the c parameter will be ignored.", call. = FALSE)
  if (is.null(c_bounds) & !is.null(n_3pl)) warning("Generated 3PL items without setting bounds for the c parameter. All c parameters will be 0.", call. = FALSE)
  if (is.null(a_bounds) & !is.null(n_2pl)) warning("Generated 2PL items without setting bounds for the a parameter. All a parameters will be 1.", call. = FALSE)
  if (is.null(a_bounds) & !is.null(n_3pl)) warning("Generated 3PL items without setting bounds for the a parameter. All a parameters will be 1.", call. = FALSE)


  #--- Number of items
  i <- sum(n_1pl, n_2pl, n_3pl)

  #--- Item number
  item_no <- seq(1:i)

  #-- Number of thresholds per item
  if (!is.null(n_1pl)) {
    k_1pl <- rep(thresholds, times = n_1pl)
  } else {
    k_1pl <- NULL
  }

  if (!is.null(n_2pl)) {
    k_2pl <- rep(thresholds, times = n_2pl)
  } else {
    k_2pl <- NULL
  }

  if (!is.null(n_3pl)) {
    k_3pl <- rep(1, n_3pl)
  } else {
    k_3pl <- NULL
  }

  #--- Number of thresholds for each item in item_no
  k <- c(k_1pl, k_2pl, k_3pl)
 
  #--- How many of each item type
  no_item_type <- c(sum(n_1pl), sum(n_2pl), sum(n_3pl))

  #--- Item type for each item in item_no
  item_type <- rep(1:3, no_item_type)
  
  # unit test: 
  # length(k) == length(item_no) == length(item_type) == sum(no_item_type) == i

  #--- Discrimination parameters ----------------------------------------------#
  
  #--- Number of items with a parameters 
  no_a_params <- length(item_type[which(item_type >= 2)])

  #--- Generate a parameters
  if (is.null(a_bounds)) {
     a_par <- rep(1, i)
  } else {
     a_par <- ifelse(item_type > 1, 
                      round(runif(no_a_params, a_bounds[1], a_bounds[2]), 2), 1)
  }

  #--- Pseudo-guessing parameter ----------------------------------------------#
  
  #--- Number if items with c parameters 
  no_c_params <- length(item_type[which(item_type == 3)])
  if (is.null(c_bounds)) {
    c_par <- rep(0, i)
  } else {
    c_par <- ifelse(item_type == 3, 
                      round(runif(no_c_params, c_bounds[1], c_bounds[2]), 2), 0)
  }    
  
  #--- Difficulty parameter(s) ------------------------------------------------#
  b_par <- list()

  for (p in 1:i) {
    b_i <- list()
  
    if (k[p] != 1){  
      # dividing the b_bounds[2] by 5 helps keep partial credit items from getting too big.
      b_i[[1]] <- runif(1, min = b_bounds[1], max = (b_bounds[2] * 0.2)) 
      
      for (j in 2 : k[p]){
        d <- runif(1, min = .1, max = (b_bounds[2] * max((1 - (k[p]/10)), 0.2)))  # must be positive
        b_i[[j]] <- b_i[[(j - 1)]] + d
      }
    
    } else {
  
      b_i[[1]] <- runif(1, min = b_bounds[1], max = b_bounds[2])
    
    }
    
    b_par[[p]] <- unlist(b_i)
  
  }

  b_mean <- mean(unlist(b_par))
  b_center <- lapply(b_par, function(x) round(x - b_mean, 2))
  
  #-- b_star is the average difficulty for each item
  #-- For dicotomous items, b_star = b_center
  b_star <- lapply(b_center, function(x) round(mean(x), 2))
  b_par <- do.call("rbind", b_star)

  d <- list()
  for (p in 1:i) {
    d_i <- list()
  
    if (k[p] != 1) {

      d_i[[p]] <- rep(0, max(k))
      for(j in 1:k[p]) d_i[[p]][j] <- b_center[[p]][j] - b_star[[p]]
       
    } else {
  
      d_i[[p]] <- rep(0, max(k))
    
    }
    
    d[[p]] <- unlist(d_i)
  
  }

  d_par <- do.call("rbind", d)
  
  dlabs <- paste0("d", 1:ncol(d_par))

  if (sum(thresholds) > 1){
    item_parameters <- data.frame(item = item_no, b = b_par, d = d_par, a = a_par, c = c_par, k = k, p = item_type)
    colnames(item_parameters) <- c("item", "b", dlabs, "a", "c", "k", "p")
  }
  if (sum(thresholds) == 1){
    item_parameters <- data.frame(item = item_no, b = b_par, a = a_par, c = c_par, k = k, p = item_type)
    colnames(item_parameters) <- c("item", "b", "a", "c", "k", "p")
  }


  #----------------------------------------------------------------------------#
  #return(list(b_par = b_center, a_par = a_par, c_par = c_par))
  return(item_parameters)
}

