#' Generation of ordinal and continuous variables
#'
#' Creates a data frame of discrete and continuous variables based on a latent
#' correlation matrix and marginal proportions.
#'
#' @param n_obs number of observations to generate.
#' @param cat_prop list of cumulative proportions for each item.
#' @param cor_matrix latent correlation matrix.
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
                              c_mean = NULL, c_sd = NULL, theta = FALSE,
                              family = NULL, mean_yw = NULL){
  if (!is.null(family)) {
    # Generating raw data according to distribution
    if (family == "gaussian") {
      if (is.null(mean_yw)) mean_yw <- rep(0, ncol(cor_matrix))
      cov_mx <- cor_matrix  # TODO: allow sd != 1 so cov != cor
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
  } else {
    #--- Load functions -----------------------------------------------------------#
    #--- generates data from num_x independent standard normals
    mvsn <- function(n, num_x){
      uncor_mat <- matrix(NA, nrow = n, ncol = num_x)
      for (i in 1:num_x) uncor_mat[, i] <- rnorm(n, 0, 1)
      return(uncor_mat)
    }
    message("Using cumulative proportions and correlation matrix")
    #--- Generate uncorrelated standard normals
    n_vars <- length(cat_prop)
    uncor_dat <- mvsn(n = n_obs, num_x = n_vars)

    #--- Correlate data
    chol_cor_matrix <- chol(cor_matrix)
    cor_dat <- uncor_dat %*% chol_cor_matrix

    #--- Number of categories for each variable in cor_dat
    n_cats <- unlist(lapply(cat_prop, function(x) length(x)))

    #--- Scale continuous variables ---------------------------------------------#
    if (any(n_cats == 1)){
      #--- which variables are continuous
      continuous_var <- which(n_cats == 1)

      #--- if they are left null, the rescaling is neutralized
      if (is.null(c_mean)) c_mean <- rep(0, length(continuous_var))
      if (is.null(c_sd)) c_sd <- rep(1, length(continuous_var))

      for (i in 1: length(continuous_var)){
        cor_dat[, continuous_var[i]] <- cor_dat[, continuous_var[i]] * c_sd[i] + c_mean[i]
      }

    }

    #--- Coarsen discrete variables ---------------------------------------------#
    if (any(n_cats > 1)){

      #--- which variables are discrete
      discrete_var <- which(n_cats > 1)

      #--- Replace cumulative pr = 1 with .999999 for computation
      cat_prop_2 <- lapply(cat_prop[discrete_var], function(x) ifelse(x == 1, (1 - 1e-7), x))

      #--- Find thresholds assuming standard normal
      var_thresholds <- lapply(cat_prop_2, function(x) qnorm(x, 0, 1))

      #--- Coarsen variables based on thresholds
      discrete_dat <- cor_dat

      #--- place an upper limit on the z score so that it will not exceed the upper threshold
      discrete_dat[discrete_dat[, discrete_var] > 5.6]  <- 5.6

      for (i in 1:length(discrete_var)) {
        for (j in rev(1:length(var_thresholds[[i]]))) {
          discrete_dat[which(cor_dat[, discrete_var[i]] <= var_thresholds[[i]][j]), discrete_var[i]] <- j
        }
      }
    } else {
      discrete_dat <- cor_dat
    }

    discrete_df <- data.frame(discrete_dat)

    #--- Create factor variables for all discrete variables.
    if (length(which(n_cats > 1)) > 1){
      fac_var_list <- lapply(discrete_df[, discrete_var], factor)
      fac_var_mat <- do.call(cbind.data.frame, fac_var_list)
      discrete_df[, discrete_var] <- fac_var_mat
    }

    if (length(which(n_cats > 1)) == 1){
      discrete_df[, discrete_var] <- as.factor(discrete_df[, discrete_var])
    }


    #--- Name variables ---------------------------------------------------------#

    #--- If the list cat_prop is not labeled, give it labels q1, ..., qX
    if (is.null(names(cat_prop))) {

      if (all(vapply(discrete_df, is.factor, c(is.factor = FALSE))) & theta == TRUE){
        stop("Cannot assign theta, all data are discrete", call. = FALSE)
      }

      if (theta == TRUE){

        #  Call first non-integer variable theta.
        colnames(discrete_df)[which(!(vapply(discrete_df, is.factor, c(is.factor = FALSE))))[1]] <- "theta"
        xtra_cols <- paste0("q", 1:(ncol(discrete_df)-1) )
        colnames(discrete_df)[which(colnames(discrete_df) != "theta")] <- xtra_cols
        discrete_df$subject <- 1:nrow(discrete_df)
        discrete_df <- discrete_df[, c("subject", "theta", xtra_cols)]

      } else {

        xtra_cols <- paste0("q", 1:(ncol(discrete_df)) )
        colnames(discrete_df) <- xtra_cols
        discrete_df$subject <- 1:nrow(discrete_df)
        discrete_df <- discrete_df[, c("subject", xtra_cols)]

      }

    } else {

      df_named <-  names(cat_prop)[which(names(cat_prop) != "NA")]
      colnames(discrete_df)[which(names(cat_prop) != "NA")] <- df_named
      df_unnamed <- paste0("p", seq_along(names(cat_prop))[is.na(names(cat_prop))])
      colnames(discrete_df)[is.na(names(cat_prop))] <- df_unnamed
      df_names <- colnames(discrete_df)
      discrete_df$subject <- 1:nrow(discrete_df)
      discrete_df <- discrete_df[, c("subject", df_names)]

    }
  }
  return(discrete_df)
}
