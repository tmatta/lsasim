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
#' @param n_fac number of factors
#' @param n_ind number of indicators per factor
#' @param Lambda either a matrix containing the factor loadings or a vector
#'   containing the lower and upper limits for a randomly-generated Lambda
#'   matrix
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
#' yw_cov <- matrix(c(1, .5, .5, .5, 1, .8, .5, .8, 1), nrow = 3)
#' questionnaire_gen(n_obs = 10, cat_prop = cum_prop, cov_matrix = yw_cov,
#'                   family = "gaussian")
#'
#' # Not providing covariance matrix (temp)
#' questionnaire_gen(n_obs = 10,
#'                   cat_prop = list(c(.25, 1), c(.6, 1)),
#'                   family = "gaussian",
#'                   pr_grp_1 = c(.25, .6), n_fac = 4, n_ind = 2)

#' @export
questionnaire_gen <- function(n_obs, cat_prop, cor_matrix = NULL,
                              c_mean = NULL, c_sd = NULL, theta = FALSE,
                              family = NULL, mean_yw = NULL,
                              cov_matrix = NULL, n_fac = NULL, n_ind = NULL,
                              Lambda = 0:1){
  # TODO: keep original order of parameters (keeps retrocompatibility) or change
  # to something more sensible (breaks compatibility)? Influences version number
  # (lsasim 1.1.0 vs. lsasim 2.0.0).

  # TODO: merge pr_grp_1 into cat_prop (pr_grp_1 must be generalized for several
  # groups first).
  if (!is.null(pr_grp_1) & !is.null(n_fac) & !is.null(n_ind)) {
    covs <- cov_gen(pr_grp_1, n_fac, n_ind, Lambda_lims = 0:1)
    index_x <- 2:(n_fac * n_ind + 1)
    cov_matrix <- covs$vcov_yxw[-index_x, -index_x]
  }
  if (is.null(family)) {
    message("Generating background data from cumulative proportions and",
            "correlation matrix")
    bg <- questionnaire_gen_polychoric(n_obs, cat_prop,
                                       cor_matrix, c_mean, c_sd, theta)
  } else {
    message("Generating", family, "-distributed background data")
    bg <- questionnaire_gen_family(n_obs, cat_prop, cov_matrix, family, theta,
                                   mean_yw)
  }
  return(bg)
}
