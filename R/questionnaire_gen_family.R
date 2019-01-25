#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on a latent
#' correlation matrix and marginal proportions.
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item.
#' @param cov_matrix covariance matrix. between the latent trait (Y) and the
#'   background variables (X and Z).
#' @param family distribution of the background variables. Can be NULL or
#'   'gaussian'.
#' @param theta if \code{TRUE} will label the first continuous variable 'theta'.
#' @param mean_yx vector with the means of the latent trait (Y) and the
#'   continuous background variables with flexible variance (X).
#' @param n_cats vector with number of categories for each W.
#'
questionnaire_gen_family <- function(n_obs, cat_prop, cov_matrix,
                                     family = "gaussian", theta = FALSE,
                                     mean_yx = NULL, n_cats) {
  # Generating raw data according to distribution -------------------------
  if (family == "gaussian") {
    cat_prop_YX <- cat_prop[lapply(cat_prop, length) == 1]
    cat_prop_W <- cat_prop[lapply(cat_prop, length) > 1]
    abs_prop_W <- lapply(cat_prop_W, function(x) c(x[1], diff(x)))
    if (length(cat_prop_W) > 0) {
      mean_z <- sapply(cat_prop_W, function(x) 0)
      var_z <- lapply(abs_prop_W, function(x) 1)
    } else {
      mean_z <- NULL
    }
    if (is.null(mean_yx) & length(cat_prop_YX)) {
      mean_yx <- rep(0, cat_prop_YX)
    }
    mean_yxz <- c(mean_yx, mean_z)

    data_yxz <- mvtnorm::rmvnorm(n = n_obs, mean = mean_yxz, sigma = cov_matrix)
  } else if (family == "binomial") {
    stop("Binomial family not yet implemented.")
  } else if (family == "poisson") {
    stop("Poisson family not yet implemented.")
  } else {
    stop("Invalid distribution family.")
  }

  # Formatting raw data ---------------------------------------------------
  data_yxw <- data.frame(data_yxz)
  if (theta) {
    cat_prop_minus_theta <- cat_prop[-1]
  } else {
    cat_prop_minus_theta <- cat_prop
  }
  num_categories <- sapply(cat_prop_minus_theta, length)
  if (any(num_categories == 1)) {
    x_name <- paste0("x", 1:(sum(num_categories == 1)))
  } else {
    x_name <- NULL
  }
  if (any(num_categories > 1)) {
    z_name <- paste0("z", 1:sum(num_categories > 1))
  } else {
    z_name <- NULL
  }
  if (theta) {
    colnames(data_yxw) <- c("theta", x_name, z_name)
  } else {
    colnames(data_yxw) <- c(x_name, z_name)
  }

  # Categorizing W as Z ---------------------------------------------------
  names(cat_prop) <- colnames(data_yxw)
  for (z in z_name) {
    z_num <- match(z, z_name)
    cut_points <- c(-Inf, qnorm(p    = cat_prop_W[[z_num]],
                                mean = mean_z[z_num],
                                sd   = sqrt(var_z[[z_num]][1])))
    # if W is dichotomous, labels are 0:1; else, labels start at 1.
    if (length(cat_prop[[z]]) == 2) {
      labels <- 1:2
    } else {
      labels <- seq(cat_prop[[z]])
    }
    w_name <- gsub("z", "w", z)
    data_yxw[substitute(w_name)] <- cut(data_yxw[, z], cut_points, labels)
    data_yxw[z] <- NULL
  }

  # Adding subject numbers to final dataset -------------------------------
  if (theta) {
    colnames(data_yxw) <- c("theta", paste0("q", 1:(ncol(data_yxw) - 1)))
  } else {
    colnames(data_yxw) <- paste0("q", seq(data_yxw))
  }
  out <- data.frame(subject = 1:nrow(data_yxz), data_yxw)

  return(out)
}
