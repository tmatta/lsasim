#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on a latent
#' correlation matrix and marginal proportions.
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item. If \code{theta
#'   = TRUE}, the first element of \code{cat_prop} must be a scalar 1, which
#'   corresponds to the \code{theta}.
#' @param cor_matrix latent correlation matrix. The first row/column corresponds
#'   to the latent trait (Y). The other rows/columns correspond to the
#'   continuous (X or Z) or the discrete (W) background variables, in the same
#'   order as \code{cat_prop}.
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
#' @param family distribution of the background variables. Can be NULL (default)
#'   or 'gaussian'.
#' @param full_output if \code{TRUE}, output will be a list containing all
#'   function parameters.
#' @importFrom stats rbinom rpois rbeta rgamma
#'
#' @details \code{cat_prop} is a list where \code{length(cat_prop)} is the
#'   number of items to be generated.  Each element of the list is a vector
#'   containing the marginal cumulative proportions for each category, summing
#'   to 1.  For continuous items, the associated element in the list should be
#'   1.
#'
#'   \code{cor_matrix} and \code{cov_matrix} are the correlation and covariance
#'   matrices that are the same size as \code{length(cat_prop)}.  The
#'   correlations related to the correlation between variables on the latent
#'   scale.
#'
#'   \code{c_mean and c_sd} are each vectors whose length is equal to the number
#'   of continuous variables as specified by \code{cat_prop}.  The default is to
#'   keep the continuous variables with mean zero and standard deviation of one.
#'
#'   \code{theta} is a logical indicator that determines if the first continuous
#'   item should be labeled \emph{theta}. If \code{theta = TRUE} but there are
#'   no continuous variables generated, a random number of background variables
#'   will be generated.
#'
#'   If \code{cat_prop} is a named list, those names will be used as variable
#'   names for the returned \code{data.frame}.  Generic names will be provided
#'   to the variables if \code{cat_prop} is not named.
#'
#'   As an alternative to providing \code{cat_prop}, the user can call this
#'   function by specifying the total number of variables using \code{n_vars} or
#'   the specific number of continuous and categorical variables through
#'   \code{n_X} and \code{n_W}. All three arguments should be provided as
#'   scalars; \code{n_W} may also be provided as a list, where each element
#'   contains the number of categories for one background variable.
#'
#'   If \code{family == "gaussian"}, the questionnaire will be generated
#'   assuming that all the variables are jointly-distributed as a multivariate
#'   normal. The default behavior is \code{family = NULL}, where the data is
#'   generated using the polychoric correlation matrix, with no distributional
#'   assumptions.
#'
#'   In essence, this function will check for the validity of the arguments
#'   provided, and randomly generates those that are not. Then, it will either
#'   call one of two internal functions, \code{questionnaire_gen_polychoric} or
#'   \code{questionnaire_gen_family}. The former corresponds to the exact
#'   functionality of questionnaire_gen on lsasim 1.0.1, where the polychoric
#'   correlations are used to generate the background questionnaire data. If
#'   \code{family != NULL}, however, \code{questionnaire_gen_family} is called.
#'   Additionally, if \code{full_output = TRUE}, the external function
#'   \code{beta_gen} is called to generate the correlation coefficients based on
#'   the true covariance matrix.
#'
#' @return By default, the function returns a \code{data.frame} object where the
#'   first column ("subject") is a \eqn{1,\ldots,n} ordered list for the \eqn{n}
#'   observations and the other columns correspond to the questionnaire answers.
#'   If \code{theta = TRUE}, the first column after "subject" will be the latent
#'   variable \eqn{\theta}; in any case, the continuous variables always come
#'   before the categorical ones.
#'
#'   If \code{full_output = TRUE}, the output will be a list containing the
#'   following 27 objects:
#'
#'   \item{bg}{a data frame containing the background questionnaire answers
#'   (i.e., the same object as described above).}
#'
#'   \item{c_mean}{identical to the input argument of the same name. Read the
#'   Details section for more information.}
#'
#'   \item{c_sd}{identical to the input argument of the same name. Read the
#'   Details section for more information.}
#'
#'   \item{cat_prop}{identical to the input argument of the same name. Read the
#'   Details section for more information.}
#'
#'   \item{cat_prop_W}{a subset of cat_prop containing only the elements
#'   corresponding to the categorical variables.}
#'
#'   \item{cat_prop_W_p}{a list containing the probabilities for each category
#'   of the categorical variables (cat_prop_W contains the cumulative
#'   probabilities).}
#'
#'   \item{cat_prop_YX}{a subset of cat_prop containing only the elements
#'   corresponding to the continuous variables.}
#'
#'   \item{cor_matrix}{identical to the input argument of the same name. Read
#'   the Details section for more information.}
#'
#'   \item{cov_matrix}{identical to the input argument of the same name. Read
#'   the Details section for more information.}
#'
#'   \item{family}{identical to the input argument of the same name.}
#'
#'   \item{full_output}{identical to the input argument of the same name.}
#'
#'   \item{label_YXZ}{vector containing the labels of the background
#'   questionnaire variables.}
#'
#'   \item{n_cats}{vector containing the number of categories for each
#'   categorical variable.}
#'
#'   \item{n_obs}{identical to the input argument of the same name.}
#'
#'   \item{n_tot}{named vector containing the number of total variables, the
#'   number of continuous background variables (i.e., the total number of
#'   background variables except \eqn{\theta}) and the number of categorical
#'   variables.}
#'
#'   \item{n_vars}{subset of \code{n_tot}, containing only the number of
#'   background continuous and categorical variables.}
#'
#'   \item{n_W}{vector containing the number of categorical variables.}
#'
#'   \item{n_X}{vector containing the number of continuous variables (except
#'   \eqn{\theta}).}
#'
#'   \item{sd_YXW}{vector with the standard deviations of all the variables}
#'
#'   \item{sd_YXZ}{vector containing the standard deviations of \eqn{\theta},
#'   the background continuous variables (\eqn{X}) and the Normally-distributed
#'   variables \eqn{Z} which will generate the background categorical variables
#'   (\eqn{W}).}
#'
#'   \item{theta}{identical to the input argument of the same name.}
#'
#'   \item{var_W}{list containing the variances of the categorical variables.}
#'
#'   \item{var_YX}{list containing the variances of the continuous variables
#'   (including \eqn{\theta}).}
#'
#'   \item{var_Z}{list containing the variances of the continuous variables
#'   \eqn{Z} that will generate the categorical variables \eqn{W}.}
#' @references Matta, T. H., Rutkowski, L., Rutkowski, D., & Liaw, Y. L. (2018).
#'   lsasim: an R package for simulating large-scale assessment data.
#'   Large-scale Assessments in Education, 6(1), 15.
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
#'                   family = "gaussian")
#' @export
questionnaire_gen <- function(n_obs, cat_prop = NULL, cor_matrix = NULL,
                              c_mean = NULL, c_sd = NULL, theta = FALSE,
                              n_vars = NULL, n_X = NULL, n_W = NULL,
                              family = NULL,
                              cov_matrix = NULL, full_output = FALSE) {
  # TODO: keep original order of parameters (keeps retrocompatibility) or change
  # to something more sensible (breaks compatibility). Group matrices and
  # cat_prop with n_X n_W.

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

  # Generation of number and characteristic of variables, if necessary ----
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

  # Adding Y if necessary
  if (length(cat_prop) != n_vars) {
    cat_prop <- c(1, cat_prop)
  }

  cat_prop_YX <- cat_prop[lapply(cat_prop, length) == 1]
  cat_prop_W <- cat_prop[lapply(cat_prop, length) > 1]
  cat_prop_W_p <- lapply(cat_prop_W, function(x) c(x[1], diff(x)))

  # Generating covariance and correlation matrix, if necessary ------------
  var_W <- lapply(seq(cat_prop_W_p),
                  function(x) cat_prop_W_p[[x]] * (1 - cat_prop_W_p[[x]]))
  if (is.null(c_sd) & length(cat_prop_YX)) {
    var_YX <- ifelse(is.null(c_sd), 1, c_sd ^ 2)
    var_YX <- rep(var_YX, length(cat_prop_YX))
  } else {
    var_YX <- rep(c_sd ^ 2, abs(length(c_sd) - length(cat_prop_YX)) + 1)
  }
  var_Z <- lapply(seq(var_W), function(x) 1)
  if (is.null(family)) var_W <- lapply(seq(var_W), function(x) var_W[[x]][1])
  sd_YXW <- sqrt(c(var_YX, unlist(var_W)))
  sd_YXZ <- sqrt(c(var_YX, unlist(var_Z)))

    if (is.null(cov_matrix)) {
    if (is.null(cor_matrix)) {
      if (is.null(family)) {
        cor_matrix <- cor_gen(n_vars)
      } else {
        cor_matrix <- cor_gen(length(cat_prop))
      }
    }
    if (is.null(family)) {
      # Variance of W reduced to 1st category
      cov_matrix <- cor_matrix * (sd_YXW %*% t(sd_YXW))
    } else {
      cov_matrix <- cor_matrix * (sd_YXZ %*% t(sd_YXZ))
    }
  } else if (is.null(cor_matrix)) {
      cor_matrix <- cov2cor(cov_matrix)
  }

  # Recalculating n_X and n_W ---------------------------------------------
  # TODO: check if this is this still necessary
  n_X <- length(cat_prop[lapply(cat_prop, length) == 1]) - theta
  n_W <- length(cat_prop[lapply(cat_prop, length) > 1])

  # Creating or, if necessary, stretching c_mean and c_sd -----------------
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

  # Labeling the matrices (and, if necessary, the data) -------------------
  label_YXZ <- names(bg)[-1]
  if (!is.null(cor_matrix)) dimnames(cor_matrix) <- list(label_YXZ, label_YXZ)
  if (!is.null(cov_matrix)) dimnames(cov_matrix) <- list(label_YXZ, label_YXZ)

  # Pre-assembling output object ------------------------------------------
  if (full_output) {
    out <- mget(ls())
  } else {
    out <- bg
  }

  # Calculating regression coefficients -----------------------------------
  if (theta & full_output & !is.null(family)) {
    betas <- beta_gen(out, output_cov = TRUE, rename_to_q = TRUE)
    out <- c(out, linear_regression = list(betas))
    # suppressed objects from output
    rm(cat_prop_YX, full_output, n_vars, label_YXZ)
  }

  return(out)
}
