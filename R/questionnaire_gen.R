#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on a latent
#' correlation matrix and marginal proportions. This function is, in effect, a
#' wrapper for
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item.
#' @param cor_matrix latent correlation matrix. The first row/column corresponds
#'   to the latent trait (Y). The other rows/columns correspond to the
#'   continuous (X) and discrete (W) background variables, in the same order as
#'   \code{cat_prop}.
#' @param cov_matrix latent covariance matrix, formatted as \code{cor_matrix}.
#' @param c_mean is a vector of population means for each continuous variable.
#' @param c_sd is a vector of population standard deviations for each continuous
#'   variable.
#' @param theta if \code{TRUE} will label the first continuous variable 'theta'.
#'
#' @param n_vars number of background variables, continuous (X) and discrete (W)
#' @param n_X number of continuous background variables
#' @param n_W number of categorical background variables
#' @param family distribution of the background variables. Can be NULL or
#'   'gaussian'.
#' @param mean_yw vector with the means of the latent trait (Y) and the
#'   background variables (W).
#' @param n_fac number of factors
#' @param n_ind number of indicators per factor
#' @param Lambda either a matrix containing the factor loadings or a vector
#'   containing the lower and upper limits for a randomly-generated Lambda
#'   matrix
#' @importFrom stats rbinom rpois rbeta rgamma
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
#' # Not providing covariance matrix
#' questionnaire_gen(n_obs = 10,
#'                   cat_prop = list(c(.25, 1), c(.6, 1), c(.2, 1)),
#'                   family = "gaussian", n_fac = 4, n_ind = 3)
#' @export
questionnaire_gen <- function(n_obs, cat_prop = NULL, cor_matrix = NULL,
                              c_mean = NULL, c_sd = NULL, theta = FALSE,
                              n_vars = NULL, n_X = NULL, n_W = NULL,
                              family = NULL, mean_yw = NULL,
                              cov_matrix = NULL, n_fac = NULL, n_ind = NULL,
                              Lambda = NULL){
  # TODO: keep original order of parameters (keeps retrocompatibility) or change
  # to something more sensible (breaks compatibility)?

  # Random generation of unprovided parameters ----------------------------
  if (is.null(n_vars)) {
    if (is.null(cat_prop)) {
      if (is.null(cor_matrix)) {
        if (is.null(cov_matrix)) {
          if (is.null(n_X) & is.null(n_W)) {
            n_X <- rbinom(n = 1, size = n_vars, prob = .2)
            n_W <- rbinom(n = 1, size = n_vars, prob = .8)
          } else {
            if (is.null(n_X)) n_X <- rpois(n = 1, lambda = 2)
            if (is.null(n_W)) n_W <- rpois(n = 1, lambda = 2)
          }
        } else {
          n_vars <- ncol(cov_matrix) - 1
        }
      } else {
        n_vars <- ncol(cor_matrix) - 1
      }
      n_cat_X <- rep(1, n_X)
      n_cat_W <- sample(c(2, 3, 4, 5) - 1, size = n_W, replace = TRUE)
      cat_prop <- c(lapply(n_cat_X, function(x) x),
                    lapply(n_cat_W, function(x) c(sort(sample(seq(.1, .9, .1), x)), 1)))
      n_vars <- n_X + n_W
    } else {
      n_vars <- length(cat_prop)
    }
  } else {
    if (is.null(cat_prop)) {
      if (is.null(n_X)) {
        if (is.null(n_W)) {
          n_X <- rbinom(n = 1, size = n_vars, prob = .2)
        } else {
          n_X <- n_vars - n_W
        }
      }
      if (is.null(n_W)) n_W <- n_vars - n_X
      n_cat_X <- rep(1, n_X)
      n_cat_W <- sample(c(2, 3, 4, 5) - 1, size = n_W, replace = TRUE)
      cat_prop <- c(lapply(n_cat_X, function(x) x),
                    lapply(n_cat_W, function(x) c(sort(sample(seq(.1, .9, .1), x)), 1)))
    }
  }

  if (is.null(n_fac)) n_fac <- 1
  if (is.null(Lambda)) Lambda <- 0:1

  if (is.null(cor_matrix)) {
    if (is.null(cov_matrix)) {
      # neither matrix is provided
      cor_matrix <- cor_gen(1 + n_vars)
      sd_YXW <- rgamma(n = 1 + n_vars, shape = 2, scale = 1)
      cov_matrix <- sweep(sweep(cor_matrix, 1L, sd_YXW, "*"), 2, sd_YXW, "*")
    } else {
      # only cov_matrix is provided
      cor_matrix <- cov2cor(cov_matrix)
    }
  } else if (is.null(cov_matrix)) {
    # only cor_matrix is provided
    sd_YXW <- rgamma(n = ncol(cor_matrix), shape = 2.5, scale = 1)
    cov_matrix <- sweep(sweep(cor_matrix, 1L, sd_YXW, "*"), 2, sd_YXW, "*")
  }

  # Generating background data --------------------------------------------
  if (is.null(family)) {
    message("Generating background data from polychoric correlations")
    if (nrow(cor_matrix) > length(cat_prop)) {
      # Dropping Y
      cor_matrix <- cor_matrix[seq(n_vars) + 1, seq(n_vars) + 1]
    }
    bg <- questionnaire_gen_polychoric(n_obs, cat_prop, cor_matrix,
                                       c_mean, c_sd, theta)
  } else {
    message("Generating ", family, "-distributed background data")
    bg <- questionnaire_gen_family(n_obs, cat_prop, cov_matrix,
                                   family, theta, mean_yw)
  }
  return(bg)
}
