context("X and W variables are renamed correctly after beta_gen")

gen_questionnaire_var_names <- function(nW = NULL, nX = NULL, n = 10) {
  data <- questionnaire_gen(n, n_W = nW, n_X = nX, theta = TRUE,
                            full_output = TRUE, family = "gaussian")
  # Data from covariance matrix and cat_prop
  short_names <- rownames(data$cor_matrix)
  W_levels_cov <- sapply(data$cat_prop[lapply(data$cat_prop, length) > 1], length)
  X_length <- sapply(data$cat_prop[lapply(data$cat_prop, length) == 1], length)[-1]
  cov_numbers <- as.numeric(substring(short_names[-1], 2))

  # Data from the regression
  expanded_names <- names(data$linear_regression$betas)
  expanded_numbers <- as.numeric(gsub("\\.\\d", "", gsub("q", "", expanded_names[-1])))
  expanded_levels_reg <- table(expanded_numbers)
  if (length(X_length) > 0) {
    expanded_levels_reg <- as.vector(expanded_levels_reg[-(1:length(X_length))] + 1)
  } else {
    expanded_levels_reg <- as.vector(expanded_levels_reg + 1)
  }

  # Workaround for unmatching classes in expanded_levels_reg and W_levels_cov
  if (length(expanded_levels_reg) == 0 & length(W_levels_cov) == 0) {
    expanded_levels_reg <- W_levels_cov <- NULL
  }

  # Putting everything together
  output <- list(data = data$bg,
                 reg_var_numbers = unique(expanded_numbers),
                 cov_numbers = cov_numbers,
                 reg_var_length = expanded_levels_reg,
                 cov_var_length = W_levels_cov)
  return(output)
}

d_random <- gen_questionnaire_var_names()
d_20W <- gen_questionnaire_var_names(nW = 20, nX = 0)
d_20X <- gen_questionnaire_var_names(nX = 20, nW = 0)
d_20X_20W <- gen_questionnaire_var_names(nX = 20, nW = 20)

test_that("Names in regression match those in the covariance matrix", {
  expect_equal(d_random$reg_var_number, d_random$cov_numbers)
  expect_equal(d_random$reg_var_length, d_random$cov_var_length)
  expect_equal(d_20W$reg_var_number, d_20W$cov_numbers)
  expect_equal(d_20W$reg_var_length, d_20W$cov_var_length)
  expect_equal(d_20X$reg_var_number, d_20X$cov_numbers)
  expect_equal(d_20X$reg_var_length, d_20X$cov_var_length)
  expect_equal(d_20X_20W$reg_var_number, d_20X_20W$cov_numbers)
  expect_equal(d_20X_20W$reg_var_length, d_20X_20W$cov_var_length)
})
