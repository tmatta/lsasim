#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on a latent
#' correlation matrix and marginal proportions.
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item.
#' @param mean_yw vector with the means of the latent trait (Y) and the
#'   background variables (W).
#' @param cov_matrix covariance matrix. between the latent trait (Y) and the
#'   background variables (W).
#' @param family distribution of the background variables. Can be NULL or
#'   'gaussian'.
#' @param theta if \code{TRUE} will label the first continuous variable 'theta'.
questionnaire_gen_family <- function(n_obs, cat_prop, cov_matrix,
                                     family = "gaussian", theta = FALSE,
                                     mean_yw = NULL) {
  # Generating raw data according to distribution
  if (family == "gaussian") {
    if (is.null(mean_yw)) mean_yw <- rep(0, length(cat_prop))
    raw_data <- mvtnorm::rmvnorm(n = n_obs, mean = mean_yw, sigma = cov_matrix)
  } else if (family == "binomial") {
    stop("Binomial family not yet implemented.")
  } else if (family == "poisson") {
    stop("Poisson family not yet implemented.")
  } else {
    stop("Invalid distribution family.")
  }

  # Formatting raw data
  bg_data <- data.frame(raw_data)
  num_categories <- sapply(cat_prop[seq(cat_prop) + theta], length)
  if (any(num_categories == 1)) {
    x_name <- paste0("x", 1:(sum(num_categories == 1)))
  } else {
    x_name <- NULL
  }
  if (any(num_categories > 1)) {
    w_name <- paste0("w", 1:sum(num_categories > 1))
  } else {
    w_name <- NULL
  }
  if (theta) {
    colnames(bg_data) <- c("theta", x_name, w_name)
  } else {
    colnames(bg_data) <- c(x_name, w_name)
  }

  # Categorizing W as Z
  names(cat_prop) <- colnames(bg_data)
  for (w in w_name) {
    cut_points <- c(-Inf, qnorm(cat_prop[[w]][-length(cat_prop[[w]])]), Inf)
    # if W is dichotomous, labels are 0:1; else, labels start at 1.
    if (length(cat_prop[[w]]) == 2) {
      labels <- 0:1
    } else {
      labels <- seq(cat_prop[[w]])
    }
    z_name <- gsub("w", "z", w)
    bg_data[substitute(z_name)] <- cut(bg_data[, w], cut_points, labels)
    bg_data[w] <- NULL
  }

  # Adding subject numbers to final dataset
  if (theta) {
    colnames(bg_data) <- c("theta", paste0("q", 1:(ncol(bg_data) - 1)))
  } else {
    colnames(bg_data) <- paste0("q", seq(bg_data))
  }
  discrete_df <- data.frame(subject = 1:nrow(raw_data), bg_data)

  return(discrete_df)
}
