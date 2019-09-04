#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on several
#' arguments.
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item. If \code{theta
#'   = TRUE}, the first element of \code{cat_prop} must be a scalar 1, which
#'   corresponds to the \code{theta}.
#' @param cor_matrix latent correlation matrix. The first row/column corresponds
#'   to the latent trait (\eqn{Y}). The other rows/columns correspond to the
#'   continuous (\eqn{X} or \eqn{Z}) or the discrete (\eqn{W}) background
#'   variables, in the same order as \code{cat_prop}.
#' @param cov_matrix latent covariance matrix, formatted as \code{cor_matrix}.
#' @param c_mean is a vector of population means for each continuous variable
#'   (\eqn{Y} and \eqn{X}).
#' @param c_sd is a vector of population standard deviations for each continuous
#'   variable  (\eqn{Y} and \eqn{X}).
#' @param theta if \code{TRUE}, the first continuous variable will be labeled
#'   'theta'. Otherwise, it will be labeled 'q1'.
#' @param n_vars total number of variables in the questionnaire, including the
#'   continuous and the discrete covariates (\eqn{X} and \eqn{W}, respectively),
#'   as well as the latent trait (\eqn{Y}, which is equivalent to \eqn{\theta}).
#' @param n_X number of continuous background variables. If not provided, a
#'   random number of continuous variables will be generated.
#' @param n_W either a scalar corresponding to the number of categorical
#'   background variables or a list of scalars representing the number of
#'   categories for each categorical variable. If not provided, a random number
#'   of categorical variables will be generated.
#' @param family distribution of the background variables. Can be NULL (default)
#'   or 'gaussian'.
#' @param full_output if \code{TRUE}, output will be a list containing the
#'   questionnaire data as well as several objects that might be of interest for
#'   further analysis of the data.
#' @param verbose if `FALSE`, output messages will be suppressed (useful for
#'   simulations). Defaults to `TRUE`
#' @importFrom stats rbinom rpois rbeta rgamma
#'
#' @details In essence, this function begins by checking the validity of the
#'   arguments provided and randomly generating those that are not. Then, it
#'   will call one of two internal functions,
#'   \code{questionnaire_gen_polychoric} or \code{questionnaire_gen_family}. The
#'   former corresponds to the exact functionality of questionnaire_gen on
#'   lsasim 1.0.1, where the polychoric correlations are used to generate the
#'   background questionnaire data. If \code{family != NULL}, however,
#'   \code{questionnaire_gen_family} is called to generate data based on a joint
#'   probability distribution. Additionally, if \code{full_output == TRUE}, the
#'   external function \code{beta_gen} is called to generate the correlation
#'   coefficients based on the true covariance matrix. The latter argument also
#'   changes the class of the output of this function.
#'
#'   What follows are some notes on the input parameters.
#'
#'   \code{cat_prop} is a list where \code{length(cat_prop)} is the number of
#'   items to be generated.  Each element of the list is a vector containing the
#'   marginal cumulative proportions for each category, summing to 1.  For
#'   continuous items, the associated element in the list should be 1.
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
#'   item should be labeled \emph{theta}. If \code{theta == TRUE} but there are
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
#'   Alternatively, \code{n_W} may be provided as a one-element list, in which
#'   case it will be interpreted as all the categorical variables having the
#'   same number of categories.
#'
#'   If \code{family == "gaussian"}, the questionnaire will be generated
#'   assuming that all the variables are jointly-distributed as a multivariate
#'   normal. The default behavior is \code{family == NULL}, where the data is
#'   generated using the polychoric correlation matrix, with no distributional
#'   assumptions.
#'
#'   When data is generated using the Gaussian distribution, the matrices
#'   provided correspond to the relations between the latent variable
#'   \eqn{\theta}, the continuous covariates \eqn{X} and the continuous
#'   covariates---\eqn{Z ~ N(0, 1)}---that will later be discretized into
#'   categorical covariates \eqn{W}. That is why there will be a difference
#'   between labels and lengths between \code{cov_matrix} and \code{vcov_YXW}.
#'   For more information, check the references cited later in this document.
#'
#' @note If \code{family == NULL}, the number of levels for each categorical
#'   variables will be determined by the number of categories observed in the
#'   generated data. This means it might be smaller than the number of
#'   categories determined by \code{cat_prop}, which is more likely to happen
#'   with small values of \code{n_obs}. If \code{family == "gaussian"}, however,
#'   the number of levels for the categorical variables will always be
#'   equivalent to the number of possible categories, even if they are not
#'   observed in the data.
#'
#'   It is important to note that all arguments directly related to variable 
#'   parameters (e.g. `cat_prop`, `cov_matrix`, `cor_matrix`, `c_mean`, `c_sd`)
#'   have the following order: Y, X, W (missing variables are skipped). This 
#'   must be kept in mind when using real-life data as input to 
#'   `questionnaire_gen`, as the input might need to be reordered to fit the
#'   expectations of the function.
#'
#' @return By default, the function returns a \code{data.frame} object where the
#'   first column ("subject") is a \eqn{1,\ldots,n} ordered list of the \eqn{n}
#'   observations and the other columns correspond to the questionnaire answers.
#'   If \code{theta = TRUE}, the first column after "subject" will be the latent
#'   variable \eqn{\theta}; in any case, the continuous variables always come
#'   before the categorical ones.
#'
#'   If \code{full_output = TRUE}, the output will be a list containing the
#'   following objects:
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
#'   \item{cat_prop_W_p}{a list containing the probabilities for each category
#'   of the categorical variables (cat_prop_W contains the cumulative
#'   probabilities).}
#'
#'   \item{cor_matrix}{identical to the input argument of the same name. Read
#'   the Details section for more information.}
#'
#'   \item{cov_matrix}{identical to the input argument of the same name. Read
#'   the Details section for more information.}
#'
#'   \item{family}{identical to the input argument of the same name.}
#'
#'   \item{n_obs}{identical to the input argument of the same name.}
#'
#'   \item{n_tot}{named vector containing the number of total variables, the
#'   number of continuous background variables (i.e., the total number of
#'   background variables except \eqn{\theta}) and the number of categorical
#'   variables.}
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
#'   (including \eqn{\theta})}
#'   
#'   \item{linear_regression}{This list is printed only If `theta = TRUE`,
#'   `family = "gaussian"` and `full_output = TRUE`. It contains one vector 
#'   named `betas` and one tabled named `cov_YXW`. The former displays the true
#'   linear regression coefficients of \eqn{theta} on the background  
#'   questionnaire answers; the latter contains the covariance matrix between
#'   all these variables.}
#'
#' @references Matta, T. H., Rutkowski, L., Rutkowski, D., & Liaw, Y. L. (2018).
#'   lsasim: an R package for simulating large-scale assessment data.
#'   Large-scale Assessments in Education, 6(1), 15.
#' @examples
#' # Using polychoric correlations
#' props <- list(c(1), c(.25, .6, 1))  # one continuous, one with 3 categories
#' questionnaire_gen(n_obs = 10, cat_prop = props,
#'                   cor_matrix = matrix(c(1, .6, .6, 1), nrow = 2),
#'                   c_mean = 2, c_sd = 1.5, theta = TRUE)
#'
#' # Using the multinomial distribution
#' # two categorical variables W: one has 2 categories, the other has 3
#' props <- list(1, c(.25, 1), c(.2, .8, 1))
#' yw_cov <- matrix(c(1, .5, .5, .5, 1, .8, .5, .8, 1), nrow = 3)
#' questionnaire_gen(n_obs = 10, cat_prop = props, cov_matrix = yw_cov,
#'                   family = "gaussian")
#'
#' # Not providing covariance matrix
#' questionnaire_gen(n_obs = 10,
#'                   cat_prop = list(c(.25, 1), c(.6, 1), c(.2, 1)),
#'                   family = "gaussian")
#' @seealso beta_gen
#' @export
questionnaire_gen <- function(n_obs, cat_prop = NULL, n_vars = NULL, n_X = NULL,
                              n_W = NULL, cor_matrix = NULL, cov_matrix = NULL,
                              c_mean = NULL, c_sd = NULL, theta = FALSE,
                              family = NULL, full_output = FALSE, verbose = TRUE) {

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
      # Expanding n_W if it was provided as a 1-length list for all Ws
      if (length(n_cats) == 1 &
          n_tot["n_vars"] != n_tot["n_X"] + n_tot["n_W"] + n_tot["theta"]) {
        n_tot["n_W"] <- n_tot["n_vars"] - n_tot["n_X"] - n_tot["theta"]
        n_cats <- rep(n_cats, n_tot["n_W"])
      }
      n_W <- n_tot["n_W"]
      cat_prop <- gen_cat_prop(n_X, n_W, n_cats)
      n_vars <- n_X + n_W + theta
    } else {
      # n_vars is absent, cat_prop is present
      n_vars <- length(cat_prop)
      n_X <- length(cat_prop[lapply(cat_prop, length) == 1]) - theta
      n_W <- length(cat_prop[lapply(cat_prop, length) > 1])
      n_tot <- gen_variable_n(n_vars, n_X, n_W, theta)
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
    if (verbose) message("Generating background data from correlation matrix")
    bg <- questionnaire_gen_polychoric(n_obs, cat_prop, cor_matrix,
                                       c_mean, c_sd, theta)
  } else {
    if (verbose) message("Generating ", family, "-distributed background data")
    bg <- questionnaire_gen_family(n_obs, cat_prop, cov_matrix,
                                   family, theta, c_mean, n_cats)
    if (!is.null(names(cat_prop))) names(bg)[-1] <- names(cat_prop)
  }

  # Labeling the matrices (and, if necessary, the data) -------------------
  if (!is.null(rownames(cov_matrix))) {
    label_YXZ <- rownames(cov_matrix)
    names(bg)[-1] <- label_YXZ
  } else if (!is.null(rownames(cor_matrix))) {
    label_YXZ <- rownames(cor_matrix)
    names(bg)[-1] <- label_YXZ
  } else {
    label_YXZ <- names(bg)[-1]
  }

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
    betas <- beta_gen(data        = out,
                      output_cov  = TRUE,
                      rename_to_q = TRUE,
                      verbose     = verbose)
    out <- c(out, linear_regression = list(betas))
  }

  # Suppressing objects from output ---------------------------------------
  if (full_output) {
    out <- within(out, rm(full_output, n_vars, n_cats, label_YXZ, var_Z,
                          cat_prop_YX, cat_prop_W))
  }

  return(out)
}
