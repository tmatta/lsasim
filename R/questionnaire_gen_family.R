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
questionnaire_gen_family <- function(n_obs, cat_prop, mean_yw, cov_matrix,
                                     family, theta) {
  # Generating raw data according to distribution
  if (family == "gaussian") {
    if (is.null(mean_yw)) mean_yw <- rep(0, ncol(cov_matrix))
    cov_mx <- cov_matrix  # TODO: allow sd != 1 so cov != cor
    raw_data <- mvtnorm::rmvnorm(n = n_obs, mean = mean_yw, sigma = cov_mx)
  } else if (family == "binomial") {
    stop("Binomial family not yet implemented.")
  } else if (family == "poisson") {
    stop("Poisson family not yet implemented.")
  } else {
    stop("Invalid distribution family.")
  }
  message("Using ", family, " distribution")

  # Formatting raw data
  bg_data <- data.frame(raw_data)
  y_name <- ifelse(theta, "theta", "y")
  colnames(bg_data) <- c(y_name, paste0("w", seq(length(cat_prop))))

  # Categorizing W as Q
  w_cols <- names(bg_data)[-1]
  names(cat_prop) <- w_cols
  for (w in w_cols) {
    cut_points <- c(-Inf, qnorm(cat_prop[[w]][-length(cat_prop[[w]])]), Inf)
    # if W is dichotomous, labels are 0:1; else, labels start at 1.
    if (length(cat_prop[[w]]) == 2) {
      labels <- 0:1
    } else {
      labels <- seq(cat_prop[[w]])
    }
    q_name <- gsub("w", "q", w)
    bg_data[substitute(q_name)] <- cut(bg_data[, w], cut_points, labels)
    bg_data[w] <- NULL
  }

  # Adding subject numbers to final dataset
  discrete_df <- data.frame(subject = 1:nrow(raw_data), bg_data)

  return(discrete_df)
}
