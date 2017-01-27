#' Generation of ordinal and continuous variables. 
#'
#' Creates a \code{data.frame} of discrete and continuous variables based on 
#' a latent correlation matrix and marginal proportions.  
#'
#' \itemize{
#'   \item \code{cat_prop} is a list where \code{length(cat_prop)} is the number of
#'         items to be generated.  Each element of the list is a vector containing 
#'         the marginal cumulative proportions for each category, summing to 1.  For
#'         continuous items, the associated element in the list should be 1. 
#'   \item \code{cor_matrix} is correlation matrix that is the same size as 
#'         \code{length(cat_prop)}.  The correlations related to the correlation 
#'         between variables on the latent scale. 
#'   \item \code{c_mean and c_sd} are each vectors whose length is equal to the number of 
#'         continuous variables as specified by \code{cat_prop}.  The default is to 
#'         keep the continuous variables with mean zero and standard deviation of one.  
#'   \item \code{theta} is a logical indicator that determines if the first continuous 
#'         item should be labeled \emph{theta}. If \code{theta = TRUE} but there are no
#'         continuous variables generated, an error will be returned.    
#' }
#' If \code{cat_prop} is a named list, those names will be used as variable names 
#' for the returned \code{data.frame}.  Generic names will be provided to the 
#' variables if \code{cat_prop} is not named.
#'   
#' 
#' @param n_obs number of observations to generate
#' @param cat_prop list of cumulative proportions for each item
#' @param cor_matrix latent correlation matrix
#' @param c_mean is a vector of population means for each continuous variable
#' @param c_sd is a vector of population standard deviations for each continuous variable
#' @param theta if \code{TRUE} will labeled the first continuous variable 'theta'
#' 
#' @return A \code{n_obs} by \code{length(cat_prop)} \code{data.frame} 
#' 
#' @examples
#' questionnaire(n = 10, cat_prop = list(c(1), c(.25, .6, 1)), 
#'              cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2), 
#'              c_mean = 2, c_sd = 1.5, theta = TRUE)
#' 
questionnaire <- function(n_obs, cat_prop, cor_matrix, tmean = NULL, tsd = NULL, v_names = NULL){

  if (is.null(tmean)) tmean <- rep(0, (dim(cor_matrix)[1] - length(cat_prop)))
  if (is.null(tsd)) tsd <- rep(1, (dim(cor_matrix)[1] - length(cat_prop)))

  n_ord <- length(cat_prop)
  n_nor <- length(tmean)

  #--- from OrdNor: computes the correlation of intermediate multivariate normal 
  #---              data before subsequent ordinalization
  cmat_star <- cmat.star(plist = cat_prop, CorrMat = cor_matrix, no.ord = n_ord, no.norm = n_nor)

  #--- from OrdNor: generates a data set with ordinal and normal variables
  discrete_dat <- genOrdNor(n_obs, plist = cat_prop, cmat.star = cmat_star, 
                            mean.vec = tmean, sd.vec = tsd, no.ord = n_ord, 
                            no.norm = n_nor)

  discrete_df <- data.frame(discrete_dat)

  #--- Convert ordinal data to factors
  fac_var_list <- lapply(discrete_df[, 1:n_ord], factor)
  fac_var_mat <- do.call(cbind.data.frame, fac_var_list)
  discrete_df[, 1:n_ord] <- fac_var_mat

  #--- Name variables ---------------------------------------------------------#
  #--- If the list cat_prop is not labeled, give it labels q1, ..., qX
  if (is.null(v_names)) v_names <- paste0("q", seq_along(discrete_df))
  colnames(discrete_df) <- v_names

  #--- Generate subject ID
  discrete_df$subject <- 1:nrow(discrete_df)
  discrete_df <- discrete_df[, c("subject", v_names)]

  return(discrete_df)  
}

