
#' Generation of random cumulative proportions. 
#'
#' Creates a list if vectors, each containing the randomly generated cumulative 
#' proportions of a discrete variable.  
#'
#' @param cat_options vector of response types.  
#' @param n_cat_options vector of number of items of the corresponding response 
#' type. 
#' 
#' \code{cat_options} and \code{n_cat_options} must be the same length.  
#' \code{cat_options = 1} is a continuous variable.
#' 
#' @return A \code{sum(n_cat_options)} length list of vectors cumulative proportions 
#' 
#' @examples
#' gen_proportions(cat_options = c(1, 2, 3), n_cat_options = c(2, 2, 2))
#' gen_proportions(cat_options = c(1, 3), n_cat_options = c(4, 5))


proportion_gen <- function(cat_options, n_cat_options){
  #- n_var: number of variables
  #- cat_options: list of response options in the survey
  #- n_cat_options: list of how many of each response option is in the survey

  if ( length(cat_options) > length(n_cat_options) | length(cat_options) < length(n_cat_options) ) {
      stop("cat_options and n_cat_options are not the same length", call. = FALSE)
  }
  if (any(cat_options %% 1 != 0)) stop("Elements of cat_options must be integers", call. = FALSE)
  if (any(n_cat_options %% 1 != 0)) warning("Elements of n_cat_options less than 1 and will be treated as 0", call. = FALSE)

  #--- random cumulative proportions
  cat_pr <- list()
  
  var_response_options <- c(rep(cat_options, n_cat_options))
  n_var <- length(var_response_options)

  for(i in 1: n_var){

    rand_pr <- list()
  
    if (var_response_options[i] != 1){  
      
      rand_pr[[1]] <- round(runif(1, min = 0, max = rbeta(1, 10, 1)), 2)  
      
      for (j in 2 : var_response_options[i]){
        k <- j - 1
        rand_pr[[j]] <- round(runif(1, min = rand_pr[[k]], max =.99), 2)
      }
      
      rand_pr[[var_response_options[i]]] <- 1
    
    } else {
    
      rand_pr[[1]] <- 1
    
    }
    
    cat_pr[[i]] <- unlist(rand_pr)
  }
  return(cat_pr)
}
