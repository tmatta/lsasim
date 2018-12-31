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
#' @param n_vars number of total variables, continuous (X), discrete (W) and
#'   theta (Y).
#' @param n_X number of continuous background variables. If not provided, a
#'   random number of continuous variables will be generated.
#' @param n_W either a scalar corresponding to the number of categorical
#'   background variables or a list of scalars representing the number of
#'   categories for each categorical variable. If not provided, a random number
#'   of categorical variables will be generated.
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
  if (!is.null(cat_prop)) {
    n_cats <- sapply(cat_prop, length)[sapply(cat_prop, length) > 1]
  }
  if (class(n_W) == "list") {
    n_cats <- unlist(n_W)
    n_W <- length(n_W)
  } else if (length(n_W) > 1) {
    # traps n_W defined as vector
    stop("n_W must be either a scalar or a list", call. = FALSE)
  }

  # Initial checks for consistency ----------------------------------------
  run_condition_checks(n_cats, n_vars, n_X, n_W, theta, cat_prop, cor_matrix,
                       cov_matrix, c_mean, c_sd)

  # Random generation of unprovided parameters ----------------------------
  # TODO: change conditional structure: use vector of non-null objects and check
  # which of the missing parameters can be calculated from the input provided.
  if (is.null(n_vars)) {
    if (is.null(cat_prop)) {
      if (is.null(cor_matrix)) {
        if (is.null(cov_matrix)) {
          n_tot <- gen_variable_n(n_vars, n_X, n_W, theta)
          n_vars <- n_tot["n_vars"]
        } else {
          # n_vars, cat_prop and cor_matrix are absent; cov_matrix is present
          n_vars <- ncol(cov_matrix) - theta
          n_tot <- gen_variable_n(n_vars, n_X, n_W, theta)
        }
      } else {
        # n_vars and cat_prop are absent; cor_matrix is present
        n_vars <- ncol(cor_matrix) - theta
        n_tot <- gen_variable_n(n_vars, n_X, n_W, theta)
      }
      n_X <- n_tot["n_X"]
      n_W <- n_tot["n_W"]
      cat_prop <- gen_cat_prop(n_X, n_W, n_cats)
      if (any(n_cats > 2) & !is.null(family)) cat_prop <- split_cat_prop(cat_prop)
      n_vars <- n_X + n_W + theta
    } else {
      # n_vars is absent, cat_prop is present
      n_vars <- length(cat_prop)
    }
  } else {
    # n_vars is provided
    if (is.null(cat_prop)) {
      n_tot <- gen_variable_n(n_vars, n_X, n_W, theta)
      n_X <- n_tot["n_X"]
      n_W <- n_tot["n_W"]
      cat_prop <- gen_cat_prop(n_X, n_W, n_cats)
    }
  }
  if (is.null(n_fac)) n_fac <- 1  # TODO: use it as input for cov_gen

  # Adding Y if necessary
  if (length(cat_prop) != n_vars) {
    cat_prop <- c(1, cat_prop)
  }

  if (is.null(cov_matrix)) {
    # TODO: check if this could always be cor_gen(length(cat_prop))
    if (is.null(cor_matrix)) {
      if (is.null(family)) {
        cor_matrix <- cor_gen(n_vars)
      } else {
        cor_matrix <- cor_gen(length(cat_prop))
      }
    }
    # TODO: generalize for poly W
    cat_prop_YX <- cat_prop[lapply(cat_prop, length) == 1]
    cat_prop_W <- cat_prop[lapply(cat_prop, length) > 1]
    cat_prop_W_p <- lapply(cat_prop_W, function(x) c(x[1], diff(x)))
    var_W <- lapply(seq(cat_prop_W_p),
                    function(x) cat_prop_W_p[[x]] * (1 - cat_prop_W_p[[x]]))
    if (is.null(c_sd) & length(cat_prop_YX)) {
      var_YX <- ifelse(is.null(c_sd), 1, c_sd ^ 2)
      var_YX <- rep(var_YX, length(cat_prop_YX))
    } else {
      var_YX <- rep(c_sd, abs(length(c_sd) - length(cat_prop_YX)) + 1)
    }
    if (all(sapply(cat_prop_W, length) == 2)) {
      var_W <- lapply(seq(var_W), function(x) var_W[[x]][1])
    } else {
      #TODO: figure out how to calculate one variance for W given many variances
      #for Z. Create several Ws (i.e., expand cov_matrix)?
      var_W <- lapply(seq(var_W), function(x) var_W[[x]][1])
    }
    sd_YXW <- sqrt(c(var_YX, unlist(var_W)))
    cov_matrix <- sweep(sweep(cor_matrix, 1L, sd_YXW, "*"), 2, sd_YXW, "*")
  }
  if (is.null(cor_matrix)) {
    cor_matrix <- cov2cor(cov_matrix)
  }

  # Recalculating n_X and n_W (is this still necessary?)
  n_X <- length(cat_prop[lapply(cat_prop, length) == 1]) - theta
  n_W <- length(cat_prop[lapply(cat_prop, length) > 1])

  # Streching c_mean and c_sd if necessary
  if (n_X + theta > length(c_mean)) {
    if (is.null(c_mean)) {
      c_mean <- rep(0, n_X + theta)
    } else {
      c_mean <- rep(c_mean, n_X + theta)
    }
  }
  if (n_X + theta > length(c_sd)) {
    if (is.null(c_sd)) {
      c_sd <- rep(1, n_X + theta)
    } else {
      c_sd <- rep(c_sd, n_X + theta)
    }
  }

  # Generating background data --------------------------------------------
  if (is.null(family)) {
    message("Generating background data from correlation matrix")
    bg <- questionnaire_gen_polychoric(n_obs, cat_prop, cor_matrix,
                                       c_mean, c_sd, theta)
  } else {
    message("Generating ", family, "-distributed background data")
    bg <- questionnaire_gen_family(n_obs, cat_prop, cov_matrix,
                                   family, theta, c_mean, n_cats)
  }

  # Labeling the matrices
  label_YXW <- names(bg)[-1]
  if (!is.null(cor_matrix)) dimnames(cor_matrix) <- list(label_YXW, label_YXW)
  if (!is.null(cov_matrix)) dimnames(cov_matrix) <- list(label_YXW, label_YXW)


  # Calculating regression coefficients -----------------------------------
  if (theta) {
    # TODO: first, fix 1-category W due to small sample
    # betas <- beta_gen(list(bg = bg, c_mean = c_mean,
    #                        cat_prop_W_p = cat_prop_W_p, theta = theta))
  }


  if (full_output) {
    # suppressed objects from output
    rm(cat_prop_W, cat_prop_YX, full_output, label_YXW, n_tot)
    out <- mget(ls())  # TODO: reorder output?
  } else {
    out <- bg
  }

  return(out)
}
