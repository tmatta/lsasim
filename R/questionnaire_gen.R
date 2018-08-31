#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on a latent
#' correlation matrix and marginal proportions. This function is, in effect, a
#' wrapper for
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item. If \code{theta
#'   = TRUE}, the first element of \code{cat_prop} must be a scalar 1, which
#'   corresponds to the \code{theta}.
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
#' @param n_W either a scalar corresponding to the number of categorical
#'   background variables or a vector with the number of categories for each
#'   categorical variable
#' @param family distribution of the background variables. Can be NULL or
#'   'gaussian'.
#' @param n_fac number of factors (currently out of use)
#' @param n_ind number of indicators per factor (currently out of use)
#' @param Lambda either a matrix containing the factor loadings or a vector
#'   containing the lower and upper limits for a randomly-generated Lambda
#'   matrix (currently out of use)
#' @param full_output if \code{TRUE}, output will be a list containing all
#'   function parameters.
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
#' cum_prop <- list(1, c(.25, 1), c(.2, .8, 1))
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
                              family = NULL,
                              cov_matrix = NULL, n_fac = NULL, n_ind = NULL,
                              Lambda = 0:1, full_output = FALSE){
  # TODO: keep original order of parameters (keeps retrocompatibility) or change
  # to something more sensible (breaks compatibility)?

  # Changes n_W to a scalar, if necessary ---------------------------------
  n_cats <- NULL  # number of categories per categorical variable W
  if (length(n_W) > 1) {
    n_cats <- n_W
    n_W <- length(n_W)
  }

  # Initial checks for consistency ----------------------------------------
  check_condition(any(n_cats == 1),
                  "the number of categories in n_W must all be greater than 1")
  check_condition(n_vars != n_X + n_W,
                  "n_X + n_W must equal n_vars")
  check_condition(length(cat_prop) != ncol(cor_matrix),
                  "length(cat_prop) cannot be different from ncol(cor_matrix)")
  check_condition(length(cat_prop) != ncol(cov_matrix),
                  "length(cat_prop) cannot be different from ncol(cor_matrix)")
  check_condition(!is.null(cat_prop) & (!is.null(n_X) | !is.null(n_W)),
                  "cat_prop was provided, so n_X and n_W were ignored", FALSE)
  check_condition(!is.null(cat_prop) & !is.null(n_vars) &
                    length(cat_prop) != n_vars,
                  "n_vars must be NULL or equal to length(cat_prop)")
  check_condition(any(cor_matrix > 1),
                  "Improper correlation matrix")
  check_condition(any(sapply(lapply(cat_prop, diff), function(x) any(x < 0))),
                  "The elements of cat_prop must be non-decreasing")
  check_condition(any(sapply(cat_prop, function(x) any(x > 1))),
                  "cat_prop must not contain values above 1")
  check_condition(any(sapply(cat_prop, max) != 1),
                  "last value of each element of cat_prop must be 1")
  check_condition(length(c_mean) > n_X + theta,
                  "length(c_mean) cannot be larger than n_X + theta")
  check_condition(length(c_mean) > 1 & length(c_mean) != n_X + theta,
                  "c_mean recycled to fit all continuous variables", FALSE)
  check_condition(length(c_sd) > n_X + theta,
                  "length(c_sd) cannot be larger than n_X + theta")
  check_condition(length(c_sd) > 1 & length(c_sd) != n_X + theta,
                  "c_sd  recycled to fit all continuous variables", FALSE)
  check_condition(any(c_sd < 0),
                  "c_sd may not contain negative elements")
  if (!is.null(cor_matrix))
    check_condition(!isSymmetric(cor_matrix), "cor_matrix is not symmetric")
  if (!is.null(cov_matrix))
    check_condition(!isSymmetric(cov_matrix), "cov_matrix is not symmetric")
  if (!is.null(cat_prop))
    check_condition(theta & (cat_prop[[1]][1] != 1),
                    "theta == TRUE, so the first element of cat_prop must be 1")

  # Random generation of unprovided parameters ----------------------------
  # TODO: change conditional structure: use vector of non-null objects and check
  # which of the missing parameters can be calculated from the input provided.
  if (is.null(n_vars)) {
    if (is.null(cat_prop)) {
      if (is.null(cor_matrix)) {
        if (is.null(cov_matrix)) {
          if (is.null(n_X)) n_X <- rpois(n = 1, lambda = 2)
          if (is.null(n_W)) n_W <- rpois(n = 1, lambda = 2)
        } else {
          n_vars <- ncol(cov_matrix) - 1
          if (is.null(n_X) & is.null(n_W)) {
            n_X <- rpois(n = 1, lambda = 2)
            n_W <- rpois(n = 1, lambda = 2)
          } else {
            if (is.null(n_X)) n_X <- n_vars - n_W
            if (is.null(n_W)) n_W <- n_vars - n_X
          }
        }
      } else {
        n_vars <- ncol(cor_matrix) - 1
        if (is.null(n_X) & is.null(n_W)) {
          n_X <- rbinom(n = 1, size = n_vars, prob = .2)
          n_W <- n_vars - n_X
        } else {
          if (is.null(n_X)) n_X <- n_vars - n_W
          if (is.null(n_W)) n_W <- n_vars - n_X
        }
      }
      # TODO: move code below into function (replicated below)
      cat_prop <- gen_cat_prop(n_X, n_W, n_cats)
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
      } else {
        # n_vars and n_X are present
        if (is.null(n_W)) n_W <- n_vars - n_X
      }
      cat_prop <- gen_cat_prop(n_X, n_W, n_cats)
    }
  }
  if (is.null(n_fac)) n_fac <- 1  # TODO: use it as input for cov_gen
  if (is.null(cor_matrix)) {
    if (is.null(cov_matrix)) {
      # neither matrix is provided
      cor_matrix <- cor_gen(theta + n_vars)
      # TODO: reimplement cov_gen here
      sd_YXW <- rgamma(n = theta + n_vars, shape = 2, scale = 1)
      cov_matrix <- sweep(sweep(cor_matrix, 1L, sd_YXW, "*"), 2, sd_YXW, "*")
    } else {
      # only cov_matrix is provided
      cor_matrix <- cov2cor(cov_matrix)
    }
  } else if (is.null(cov_matrix)) {
    # only cor_matrix is provided
    # TODO: reimplement cov_gen here
    sd_YXW <- rgamma(n = ncol(cor_matrix), shape = 2.5, scale = 1)
    cov_matrix <- sweep(sweep(cor_matrix, 1L, sd_YXW, "*"), 2, sd_YXW, "*")
  }

  # Adding Y if necessary
  if (length(cat_prop) != ncol(cor_matrix)) {
    cat_prop <- c(1, cat_prop)
  }

  # Streching c_mean and c_sd if necessary
  n_X <- length(cat_prop[lapply(cat_prop, length) == 1])
  n_W <- length(cat_prop[lapply(cat_prop, length) > 1])
  n_vars <- n_X + n_W
  # TODO: add check to see if these final values above are different from the
  # ones provided by the user
  if (n_X > length(c_mean)) c_mean <- rep(c_mean, n_X)
  if (n_X > length(c_sd))   c_sd   <- rep(c_sd, n_X)

  # Generating background data --------------------------------------------
  if (is.null(family)) {
    message("Generating background data from correlation matrix")
    bg <- questionnaire_gen_polychoric(n_obs, cat_prop, cor_matrix,
                                       c_mean, c_sd, theta)
  } else {
    message("Generating ", family, "-distributed background data")
    bg <- questionnaire_gen_family(n_obs, cat_prop, cov_matrix,
                                   family, theta, c_mean)
  }
  if (full_output) {
    out <- mget(ls())
  } else {
    out <- bg
  }
  return(out)
}
