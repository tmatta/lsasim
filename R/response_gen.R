response_gen <- function(subject, item, theta, a_par = NULL, b_par, c_par = NULL, d_par = NULL){

  if (is.null(a_par)) a_par <- rep(1, length(item))
  if (is.null(c_par)) c_par <- rep(0, length(item))


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
                     c_par = c_par[item[n]])
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

