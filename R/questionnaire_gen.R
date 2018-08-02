#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on a latent
#' correlation matrix and marginal proportions. This function is, in effect, a wrapper for
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item.
#' @param cor_matrix latent correlation matrix.
#' @param cov_matrix latent covariance matrix.
#' @param c_mean is a vector of population means for each continuous variable.
#' @param c_sd is a vector of population standard deviations for each continuous
#'   variable.
#' @param theta if \code{TRUE} will label the first continuous variable 'theta'.
#' @param family distribution of the background variables. Can be NULL or
#'   'gaussian'.
#' @param mean_yw vector with the means of the latent trait (Y) and the
#'   background variables (W).
#'
#' @section Details: \code{cat_prop} is a list where \code{length(cat_prop)} is
#'   the number of items to be generated.  Each element of the list is a vector
#'   containing the marginal cumulative proportions for each category, summing
#'   to 1.  For continuous items, the associated element in the list should be
#'   1.
#'
#'   \code{cor_matrix} is correlation matrix that is the same size as
#'   \code{length(cat_prop)}.  The correlations related to the correlation
#'   between variables on the latent scale.
#'
#'   \code{c_mean and c_sd} are each vectors whose length is equal to the number
#'   of continuous variables as specified by \code{cat_prop}.  The default is to
#'   keep the continuous variables with mean zero and standard deviation of one.
#'
#'   \code{theta} is a logical indicator that determines if the first continuous
#'   item should be labeled \emph{theta}. If \code{theta = TRUE} but there are
#'   no continuous variables generated, an error will be returned.
#'
#'   If \code{cat_prop} is a named list, those names will be used as variable
#'   names for the returned \code{data.frame}.  Generic names will be provided
#'   to the variables if \code{cat_prop} is not named.
#'
#'
#' @examples
#' # Using polychoric correlations
#' cum_prop <- list(c(1), c(.25, .6, 1))  # one continuous, one with 3 categories
#' questionnaire_gen(n_obs = 10, cat_prop = cum_prop,
#'                   cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
#'                   c_mean = 2, c_sd = 1.5, theta = TRUE)
#'
#' # Using the multinomial distribution
#' # two categorical variables W: one has 2 categories, the other has 3
#' cum_prop <- list(c(.25, 1), c(.2, .8, 1))
#' yw_cor <- matrix(c(1, .5, .5, .5, 1, .8, .5, .8, 1), nrow = 3)
#' questionnaire_gen(n_obs = 10, cat_prop = cum_prop, cor_matrix = yw_cor,
#'                   family = "gaussian")
#' @export
questionnaire_gen <- function(n_obs, cat_prop, cor_matrix = NULL,
                              cov_matrix = NULL,
                              c_mean = NULL, c_sd = NULL, theta = FALSE,
                              family = NULL, mean_yw = NULL){
  if (is.null(family)) {
    bg <- questionnaire_gen_polychoric(n_obs, cat_prop,
                                       cor_matrix, c_mean, c_sd, theta)
  } else {
    bg <- questionnaire_gen_family(n_obs, cat_prop,
                                   mean_yw, cov_matrix, family, theta)
  }
  return(bg)
}
