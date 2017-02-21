#' Generation of item response data using a rotated block design
#'
#' Creates a data frame of discrete item responses based on.
#' 
#' @param subject integer vector of test taker IDs.
#' @param item integer vector of item IDs.
#' @param theta numeric vector of latent test taker abilities.
#' @param b_par numeric vector of item b parameters for each item.
#' @param a_par numeric vector of item a parameters for each item. 
#' @param c_par numeric vector of item c parameters for each item.
#' @param d_par list of numeric vectors of item threshold parameters for each item.
#' @param ogive can be "Normal" or "Logistic".
#' 
#' @section Details:
#' \code{subject} and \code{item} must be equal lengths.  
#' 
#' Generalized partial credit models (\code{!is.null(d_par)}) uses threshold parameterization.  
#' 
#' @examples
#' set.seed(1234)
#' s_id <- c(1, 1, 1, 1, 1, 1, 1, 2, 2, 2, 2, 2, 2, 2, 3, 3, 3, 3, 3, 3, 3, 4, 4, 
#'           4, 4, 4, 4, 4, 5, 5, 5, 5, 5, 5, 5, 6, 6, 6, 6, 6, 6, 7, 7, 7, 7, 7, 
#'           7, 7, 8, 8, 8, 8, 8, 8, 9, 9, 9, 9, 9, 9, 9, 10, 10, 10, 10, 10, 10, 
#'           10, 11, 11, 11, 11, 11, 11, 12,12, 12, 12, 12, 12, 12, 13, 13, 13, 13, 
#'           13, 13, 14, 14, 14, 14, 14, 14, 15, 15, 15, 15, 15, 15, 16,16, 16, 16, 
#'           16, 16, 17, 17, 17, 17, 17, 17, 17, 18, 18, 18, 18, 18, 18, 18, 19, 19, 
#'           19, 19, 19, 19,19, 20, 20, 20, 20, 20, 20, 20)
#' i_id<- c(1, 4, 7, 10, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 1, 4, 7, 10, 3, 6, 9, 1, 4, 
#'          7, 10, 3, 6, 9, 1, 4, 7, 10, 3, 6, 9, 2, 5, 8, 3, 6, 9, 1, 4, 7, 10, 2, 
#'          5, 8, 2, 5, 8, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 1, 4, 7, 10, 3, 6, 9, 2, 
#'          5, 8, 3, 6, 9, 1, 4, 7, 10, 3, 6, 9, 2, 5, 8, 3, 6, 9, 2, 5, 8, 3, 6, 9, 
#'          2, 5, 8, 3, 6, 9, 2, 5, 8, 3, 6, 9, 1, 4, 7, 10, 2, 5, 8, 1, 4, 7, 10, 
#'          2, 5, 8, 1, 4, 7, 10, 2, 5, 8, 1, 4, 7, 10, 3, 6, 9)
#' bb <- c(-1.72, -1.85, 0.98, 0.07, 1.00, 0.13, -0.43, -0.29, 0.86, 1.26)
#' aa <- c(1.28, 0.78, 0.98, 1.21, 0.83, 1.01, 0.92, 0.76, 0.88, 1.11)
#' cc <- rep(0, 10)
#' dd <- list(c(0, 0, -0.13, 0, -0.19, 0, 0, 0, 0, 0), 
#'            c(0, 0,  0.13, 0,  0.19, 0, 0, 0, 0, 0))
#' response_gen(subject = s_id, item = i_id, theta = rnorm(20, 0, 1), 
#'              b_par = bb, a_par = aa, c_par = cc, d_par = dd)
#' 
#' @export
response_gen <- function(subject, item, theta, a_par = NULL, b_par, c_par = NULL, d_par = NULL, ogive = "Logistic"){

  if (length(subject) != length(item)) stop("subject and item vectors must be equal length.", call. = FALSE)
  if (length(unique(item)) != length(b_par)) stop("Must include b_par for each item.", call. = FALSE)
  if (!is.null(a_par) & length(unique(item)) != length(a_par)) stop("Must include a parameter for each item.", call. = FALSE)
  if (!is.null(c_par) & length(unique(item)) != length(c_par)) stop("Must include c parameter for each item.", call. = FALSE)
  #if (ogive != "Normal" | ogive != "Logistic") stop("ogive must be Normal or Logistic.", call. = FALSE)
  
  if (is.null(a_par)) a_par <- rep(1, length(unique(item)))
  if (is.null(c_par)) c_par <- rep(0, length(unique(item)))

  if (ogive == "Logistic") DD <- 1
  if (ogive == "Normal") DD <- 1.7

  #--- construct b_pars list to be used in irt_gen() --------------------------# 
  if (is.null(d_par)) b_pars <- split(b_par, seq(length(b_par)))
  


  if (!is.null(d_par)) { 
    d_mat <- do.call("cbind", d_par)
    b_pars <- list()
  
    for (i in 1:length(b_par)){

      if (sum(abs(d_mat[i, ])) != 0) {
        b_list <- list()
        for (j in 1:length(d_mat[i, ])) b_list[[j]] <- b_par[i] + d_mat[i, j]
        b_pars[[i]] <- unlist(b_list)
      } 

      if (sum(abs(d_mat[i, ])) == 0) b_pars[[i]] <- b_par[i]

    }
  }  
  #----------------------------------------------------------------------------#

  y <- numeric(length(subject))
  
  for (n in 1 : length(subject)) {
    y[n] <- irt_gen(theta = theta[subject[n]], 
                     a_par = a_par[item[n]], 
                     b_par = b_pars[[item[n]]],
                     c_par = c_par[item[n]],
                     D     = DD)
  }

  df_l <- data.frame(item = item, subject = subject, response = y)

  df_w <- reshape(df_l, timevar = "item", idvar = "subject", direction = "wide")

  # This could be better generalized to respond to the number of digits.
  df_item_old <- colnames(df_w)[2:length(df_w)]
  df_item_num <- gsub("[^[:digit:]]", "", df_item_old)
  df_item_new <- ifelse(nchar(df_item_num) == 1, paste0("i00", df_item_num), 
                   ifelse(nchar(df_item_num) == 2, paste0("i0", df_item_num),
                     paste0("i", df_item_num)))

  colnames(df_w)[2:length(df_w)] <- df_item_new
  df_w <- df_w[, order(names(df_w))]
  rownames(df_w) <- NULL
  return(y = df_w)
}

