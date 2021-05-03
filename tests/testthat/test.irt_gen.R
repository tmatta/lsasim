# Context: https://github.com/tmatta/lsasim/issues/40

# Extra effort to make code work
n_items <- 30

# Original code
response_gen2 <- function (subject, item, theta, a_par = NULL, b_par, c_par = NULL,
                           d_par = NULL, item_no = NULL, ogive = "Logistic") {
  if (length(subject) != length(item))
    stop("subject and item vectors must be equal length.",
         call. = FALSE)
  if (is.null(a_par))
    a_par <- rep(1, length(unique(item)))
  if (is.null(c_par))
    c_par <- rep(0, length(unique(item)))
  if (ogive == "Logistic")
    DD <- 1
  if (ogive == "Normal")
    DD <- 1.7
  if (is.null(item_no))
    item_no <- seq(length(unique(item)))
  if (is.null(d_par))
    b_pars <- split(b_par, seq(length(b_par)))
  if (!is.null(d_par)) {
    d_mat <- do.call("cbind", d_par)
    b_pars <- list()
    for (i in 1:length(b_par)) {
      if (sum(abs(d_mat[i, ])) != 0) {
        b_list <- list()
        for (j in 1:length(d_mat[i, ])) b_list[[j]] <- b_par[i] +
            d_mat[i, j]
        b_pars[[i]] <- unlist(b_list)
      }
      if (sum(abs(d_mat[i, ])) == 0)
        b_pars[[i]] <- b_par[i]
    }
  }
  names(b_pars) <- item_no

  y <- numeric(length(subject))
  for (n in 1:length(subject)) {
    y[n] <- irt_gen2(theta = theta[subject[n]], a_par = a_par[which(item_no ==
                                                                      item[n])], b_par = b_pars[[which(item_no == item[n])]],
                     c_par = c_par[which(item_no == item[n])], D = DD)
  }
  df_l <- data.frame(item = item, subject = subject, response = y)
  df_w <- reshape(df_l, timevar = "item", idvar = "subject",
                  direction = "wide")
  df_item_old <- colnames(df_w)[2:length(df_w)]
  df_item_num <- gsub("[^[:digit:]]", "", df_item_old)
  df_item_new <- ifelse(nchar(df_item_num) == 1, paste0("i00",
                                                        df_item_num), ifelse(nchar(df_item_num) == 2, paste0("i0",
                                                                                                             df_item_num), paste0("i", df_item_num)))
  colnames(df_w)[2:length(df_w)] <- df_item_new
  df_w <- df_w[, order(names(df_w))]
  rownames(df_w) <- NULL
  return(y = df_w)
}



irt_gen2 <- function (theta, a_par = 1, b_par, c_par = 0, D = 1)
{
  response_pr <- c_par + (1 - c_par) * (exp(a_par*(theta - b_par)) / (1 + exp(a_par*(theta - b_par))))
  y <- rbinom(1, size = 1, prob = response_pr)
  return(y)
}

set.seed(345)

#this is a normal example
subject <- 1:100
theta <- rnorm(100,0,1)
student_dt <- data.frame(subject, theta)

item <- as.integer(c(1:n_items))
a <- runif(n_items, 0.5, 1.5)
b <- runif(n_items, -3, 3)
c <- runif(n_items, 0, .15)
item_par <- data.frame(item, a, b, c)

resp_matrix <- response_gen(subject = sort(rep(subject, 30)), item = rep(item,100), theta = theta, a_par = a, b_par = b, c_par = c)

#item means make sense and everything looks fine
print(colMeans(resp_matrix[item]))

print(lm(colMeans(resp_matrix[item]) ~ b))


#Let's make it very extreme, student mean theta is -20, I don't expect them to solve anything. But guessing parameters are super high. So, regardles if their theta 80%-90% of them should solve these items.
subject <- 1:100
theta <- rnorm(100, -20 ,1)
student_dt <- data.frame(subject, theta)


item <- as.integer(c(1:n_items))
a <- runif(n_items, 0.5, 1.5)
b <- runif(n_items, -3, 3)

#chance is super high
c <- runif(n_items, 0.8, .9)
item_par <- data.frame(item, a, b, c)

resp_matrix <- response_gen(subject = sort(rep(subject, 30)), item = rep(item,100), theta = theta, a_par = a, b_par = b, c_par = c)

print(colMeans(resp_matrix[item])) #look at the results, they are around 50%, what happened to .8-.9 guessing parameter
print(lm(colMeans(resp_matrix[item]) ~ b))


#let's check after the fix - but it only works for dichotomous variables
resp_matrix2 <- response_gen2(subject = sort(rep(subject, 30)), item = rep(item,100), theta = theta, a_par = a, b_par = b, c_par = c)
print(colMeans(resp_matrix2[item])) #look at the results, they are around 50%, what happened to .8-.9 guessing parameter
print(lm(colMeans(resp_matrix2[item]) ~ b))
