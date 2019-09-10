context("Basic cluster sampling")
wrap_cluster_gen <- function(...) {
  cluster_gen(..., family = "gaussian", verbose = FALSE)
}

# Basic argument handling ------------------------------------------------------
test_that("Basic argument handling generates data", {
  df01 <- wrap_cluster_gen(1:2)
  df02 <- wrap_cluster_gen(2:4)
  df03 <- wrap_cluster_gen(2:4, cluster_labels = LETTERS[1:2])
  df04 <- wrap_cluster_gen(2:3, resp_labels = LETTERS[1:2])
  df05 <- wrap_cluster_gen(2:4,
    cluster_labels = LETTERS[1:2],
    resp_labels = LETTERS[3:5]
  )
  df06 <- wrap_cluster_gen(2:3, n_X = 1)
  df07 <- wrap_cluster_gen(2:3, n_X = 1, n_W = 1)
  df08 <- wrap_cluster_gen(2:3, n_X = 2:3, n_W = 3:4)
  df09 <- wrap_cluster_gen(2:3, n_X = 0, n_W = list(5, 2))
  df10 <- wrap_cluster_gen(rep(10, 3),
    c_mean = list(c(1, 10), c(1e2, 1e3)), n_X = 2
  )
  df11 <- wrap_cluster_gen(2:4,
    separate_questionnaires = FALSE,
    c_mean = c(1, 10), n_X = 2
  )
  df12 <- wrap_cluster_gen(2:4, collapse = "partial")
  df13 <- wrap_cluster_gen(2:4, collapse = "full")
  df14 <- wrap_cluster_gen(2:4,
    collapse = "full",
    separate_questionnaires = FALSE
  )

  expect_output(str(df01), "List of 1")
  expect_output(str(df02), "List of 2")
  expect_equal(names(df03), LETTERS[1:2])
  expect_equal(df04$country[[1]]$uniqueID[1], "A1_country1")
  expect_equal(df05$B[[3]]$uniqueID[4], "D4_B3_A1")
  expect_equal(
    as.vector(sapply(
      df06$country,
      function(c) sapply(c[1:3], class)
    )),
    rep(c("integer", "numeric", "factor"), 2)
  )
  expect_equal(as.vector(sapply(df07$country, function(c) sapply(c, class))),
               rep(c("integer", "numeric", "factor",
                   rep("numeric", "numeric", 3), "character"), 2))
  expect_equal(as.vector(sapply(df08$country, function(c) sapply(c, class))),
               rep(c("integer", rep("numeric", 2), rep("factor", 3),
                     rep("numeric", 3), "character"), 2))
  expect_output(str(df09$country[[1]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$country[[1]]$q2), "Factor w/ 2 levels")
  expect_output(str(df09$country[[2]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$country[[2]]$q2), "Factor w/ 2 levels")
  expect_equivalent(sapply(df10, function(x) apply(x[[1]][2:3], 2, mean))[, 1],
                    c(1, 10), tolerance = .1)
  expect_equivalent(sapply(df10, function(x) apply(x[[2]][2:3], 2, mean))[, 1],
                    c(1, 10), tolerance = .1)
  expect_equivalent(sapply(df10, function(x) apply(x[[1]][2:3], 2, mean))[, 2],
                    c(1e2, 1e3), tolerance = .1)
  expect_equivalent(sapply(df10, function(x) apply(x[[2]][2:3], 2, mean))[, 2],
                    c(1e2, 1e3), tolerance = .1)
  expect_output(str(df11), "List of 6")
  expect_output(str(df12), "List of 2")
  expect_output(str(df13), "24 obs.")
  expect_output(str(df14), "24 obs.")
})

# Errors are caught ------------------------------------------------------------
test_that("Errors are caught", {
  expect_error(cluster_gen(1))
  expect_error(cluster_gen(2:4, separate_questionnaires = FALSE, n_X = 1:2))
  expect_error(cluster_gen(2:4, separate_questionnaires = FALSE, n_W = 1:2))
  expect_error(cluster_gen(2:4, cluster_labels = "a"))
  expect_warning(cluster_gen(2:4, separate_quest = FALSE, collapse = "partial", 
                             verbose = FALSE))
})

# uniqueIDs are correct --------------------------------------------------------
test_that("uniqueIDs are correct", {
  wrap_cluster_gen_2 <- function(..., coll = "full", return_all = FALSE) {
    data <- cluster_gen(..., n_X = 0, n_W = 1, family = "gaussian",
                        verbose = FALSE, collapse = coll)
    if (!return_all) data <- data[, 6]
    return(data)  # corresponds to the bottom level's uniqueID
  }
  scheme1 <- list(1, 2, c(1, 2), c(3, 2, 3))
  scheme2 <- list(2, c(3, 2), c(1, 1, 2, 3, 2), rep(2, 9))

  df1 <- wrap_cluster_gen_2(2:4)
  df2 <- wrap_cluster_gen_2(2:4, separate_questionnaires = FALSE)
  df3 <- wrap_cluster_gen_2(scheme1)
  df4 <- wrap_cluster_gen_2(scheme1, separate_questionnaires = FALSE)
  expect_error(wrap_cluster_gen_2(list(1, c(1, 1), c(1, 2), c(3, 2, 3)),
                                  separate_questionnaires = FALSE))
  df6 <- wrap_cluster_gen_2(scheme2)
  df7 <- wrap_cluster_gen_2(scheme2, separate_questionnaires = FALSE)
  df8 <- wrap_cluster_gen(c(2, 3, 4), n_X = 1, n_W = 1, c_mean = 10,
                          cluster_labels = c("school", "class"),
                          resp_labels = c("teacher", "student"),
                          separate_questionnaires = FALSE)
  expect_equal(df1, c('teacher1_school1_country1', 'teacher2_school1_country1', 
    'teacher3_school1_country1', 'teacher4_school1_country1', 
    'teacher1_school1_country2', 'teacher2_school1_country2', 
    'teacher3_school1_country2', 'teacher4_school1_country2', 
    'teacher1_school2_country1', 'teacher2_school2_country1', 
    'teacher3_school2_country1', 'teacher4_school2_country1', 
    'teacher1_school2_country2', 'teacher2_school2_country2', 
    'teacher3_school2_country2', 'teacher4_school2_country2', 
    'teacher1_school3_country1', 'teacher2_school3_country1', 
    'teacher3_school3_country1', 'teacher4_school3_country1', 
    'teacher1_school3_country2', 'teacher2_school3_country2', 
    'teacher3_school3_country2', 'teacher4_school3_country2'))
  expect_equal(df2, c('student1_school1_country1', 'student2_school1_country1', 
   'student3_school1_country1', 'student4_school1_country1', 
   'student1_school2_country1', 'student2_school2_country1', 
   'student3_school2_country1', 'student4_school2_country1', 
   'student1_school3_country1', 'student2_school3_country1', 
   'student3_school3_country1', 'student4_school3_country1', 
   'student1_school1_country2', 'student2_school1_country2', 
   'student3_school1_country2', 'student4_school1_country2', 
   'student1_school2_country2', 'student2_school2_country2', 
   'student3_school2_country2', 'student4_school2_country2', 
   'student1_school3_country2', 'student2_school3_country2', 
   'student3_school3_country2', 'student4_school3_country2'))
  expect_equal(df3, c('student1_class1_school1_country1',
    'student2_class1_school1_country1', 'student3_class1_school1_country1',
    'student1_class1_school2_country1', 'student2_class1_school2_country1', 
    'student1_class2_school2_country1', 'student2_class2_school2_country1', 
    'student3_class2_school2_country1'))
  expect_equal(df4, c('student1_class1_school1_country1', 
    'student2_class1_school1_country1', 'student3_class1_school1_country1', 
    'student1_class1_school2_country1', 'student2_class1_school2_country1', 
    'student1_class2_school2_country1', 'student2_class2_school2_country1', 
    'student3_class2_school2_country1'))
  expect_equal(df6, c('student1_class1_school1_country1', 
    'student2_class1_school1_country1', 'student1_class1_school1_country2', 
    'student2_class1_school1_country2', 'student1_class1_school2_country1', 
    'student2_class1_school2_country1', 'student1_class1_school2_country2', 
    'student2_class1_school2_country2', 'student1_class1_school3_country1', 
    'student2_class1_school3_country1', 'student1_class2_school1_country2', 
    'student2_class2_school1_country2', 'student1_class2_school2_country2', 
    'student2_class2_school2_country2', 'student1_class2_school3_country1', 
    'student2_class2_school3_country1', 'student1_class3_school1_country2', 
    'student2_class3_school1_country2'))
  expect_equal(df7, c('student1_class1_school1_country1', 
    'student2_class1_school1_country1', 'student1_class1_school2_country1', 
    'student2_class1_school2_country1', 'student1_class1_school3_country1', 
    'student2_class1_school3_country1', 'student1_class2_school3_country1', 
    'student2_class2_school3_country1', 'student1_class1_school1_country2', 
    'student2_class1_school1_country2', 'student1_class2_school1_country2', 
    'student2_class2_school1_country2', 'student1_class3_school1_country2', 
    'student2_class3_school1_country2', 'student1_class1_school2_country2', 
    'student2_class1_school2_country2', 'student1_class2_school2_country2', 
    'student2_class2_school2_country2'))
})

# Named n vector ---------------------------------------------------------------
test_that("Named vectors are working properly", {
  df1 <- cluster_gen(n       = c("land" = 1, "skole" = 3, "klasse" = 2),
                     verbose = FALSE,
                     collapse = "full")
  df2 <- cluster_gen(n       = list("país" = 1, "cidade" = 4,
                                    "escola" = 1:4, "estudante" = rep(1, 10)),
                    verbose = FALSE,
                    collapse = "full")
  expect_equal(df1$uniqueID, c("klasse1_skole1_land1", "klasse2_skole1_land1",
                               "klasse1_skole2_land1", "klasse2_skole2_land1",
                               "klasse1_skole3_land1", "klasse2_skole3_land1"))
  expect_equal(df2$uniqueID, c("estudante1_escola1_cidade1_pais1",
    "estudante1_escola1_cidade2_pais1", "estudante1_escola1_cidade3_pais1",
    "estudante1_escola1_cidade4_pais1", "estudante1_escola2_cidade2_pais1", 
    "estudante1_escola2_cidade3_pais1", "estudante1_escola2_cidade4_pais1",
    "estudante1_escola3_cidade3_pais1", "estudante1_escola3_cidade4_pais1",
    "estudante1_escola4_cidade4_pais1"))
})

# Different means --------------------------------------------------------------
wrap_c_gen_mu <- function(...) {
  cluster_gen(..., n_X = 2, n_W = 0, family = "gaussian",
              verbose = FALSE, calc_weights = FALSE)
}
df1 <- wrap_c_gen_mu(c(2, 1000))
df2 <- wrap_c_gen_mu(c(2, 1000), c_mean = 10)
df3 <- wrap_c_gen_mu(c(2, 1000), c_mean = c(10, 100))
df4 <- wrap_c_gen_mu(c(2, 1000), c_mean = list(list(c(10, 100), c(20, 200))))
df5 <- wrap_c_gen_mu(n      = c(school = 2, class = 3, student = 1000),
                     c_mean = list(
                        school = list(c(10, 100), c(20, 200)),
                        class  = list(c(30, 300), c(40, 400), c(50, 500),
                                      c(60, 600), c(70, 700), c(80, 800))
                        )
                    )
test_that("Different means are working", {
  mean_Xs <- function(x) colMeans(x[2:3])
  expect_equivalent(sapply(df1$country, mean_Xs)[, 1], c(0, 0), tol = .1)
  expect_equivalent(sapply(df1$country, mean_Xs)[, 2], c(0, 0), tol = .1)
  expect_equivalent(sapply(df2$country, mean_Xs)[, 1], c(10, 10), tol = .1)
  expect_equivalent(sapply(df2$country, mean_Xs)[, 2], c(10, 10), tol = .1)
  expect_equivalent(sapply(df3$country, mean_Xs)[, 1], c(10, 100), tol = .1)
  expect_equivalent(sapply(df3$country, mean_Xs)[, 2], c(10, 100), tol = .1)
  expect_equivalent(sapply(df4$country, mean_Xs)[, 1], c(10, 100), tol = .1)
  expect_equivalent(sapply(df4$country, mean_Xs)[, 2], c(20, 200), tol = .1)
  expect_equivalent(sapply(df5$school, mean_Xs)[, 1], c(10, 100), tol = .1)
  expect_equivalent(sapply(df5$school, mean_Xs)[, 2], c(20, 200), tol = .1)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 1], c(30, 300), tol = .1)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 2], c(40, 400), tol = .1)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 3], c(50, 500), tol = .1)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 4], c(60, 600), tol = .1)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 5], c(70, 700), tol = .1)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 6], c(80, 800), tol = .1)
})


context("Sampling weights")
wrap_cluster_gen <- function(n, N, meth = "SRS", sum_pop = sapply(N, sum),
                             sep = FALSE, verbose = FALSE, ...) {
  data <- cluster_gen(
    n                       = n,
    N                       = N,
    sum_pop                 = sum_pop,
    n_X                     = 1,
    n_W                     = 1,
    sampling_method         = meth,
    separate_questionnaires = sep,
    verbose                 = verbose,
    ...
  )
  if (verbose) print(data)
  return(data)
}

calcWeights <- function(data_list) {
  w <- sapply(data_list, function(x) colSums(x[4:6]))
  w_sum <- rowSums(w)
  w_1_i <- w_sum[1]
  n_i_w_ij <- w_sum[3]
  out <- c(w_1_i, n_i_w_ij)
  return(out)
}

# FIXME: cluster_gen(c(school = 2, class = 3, stu = 5)) "not 'all class', 'each'"
# FIXME: cluster_gen(c(school = 2, class = 3, stu = 5)) weights OK? If N is set, yes.

# Custom weight tests ----------------------------------------------------------
test_that("Weights are correct", {
  ex1 <- wrap_cluster_gen(c(1, 2, 3), c(10, 100, 600))
  ex2 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 4)),
                          meth = "PPS")
  ex3 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 4)),
                          meth = "SRS")
  expect_equivalent(calcWeights(ex1), c(50 * 3 * 2, 10000 * 3 * 2))
  expect_equivalent(calcWeights(ex2)["final.student.weight"], 400)
  expect_equivalent(calcWeights(ex3)["school.weight"], 2.5 * (10 + 5 + 2 + 3))
})

# Example from PISA manual tables ----------------------------------------------
ex_3.3 <- wrap_cluster_gen(n = c(school = 4, student = 10),
                           N = c(        10,           40))
ex_3.4 <- wrap_cluster_gen(n = list(school =  4,student = c(10, 10, 10,  10)),
                           N = list(         10,          c(15, 30, 40, 100)))
ex_3.5 <- wrap_cluster_gen(n = list(school =  4, student = c(10, 10, 10,  10)),
                           N = list(         10,           c(10, 15, 20,  25)))
ex_3.6 <- wrap_cluster_gen(n = list(school =  4, student = c(10, 10, 10,  10)),
                           N = list(         10,           c(40, 45, 80, 100)))
ex_3.7 <- wrap_cluster_gen(n = list(school =  4, student = c(10, 10, 10,  10)),
                           N = list(         10,           c(20, 40, 80, 100)),
                           sum_pop = c(10, 400), "PPS")

test_that("Weights from PISA examples are correct", {
  expect_equivalent(calcWeights(ex_3.3), c(2.5 * 10 * 4, 400))
  expect_equivalent(calcWeights(ex_3.4), c(2.5 * 10 * 4, 462.5))
  expect_equivalent(calcWeights(ex_3.5), c(2.5 * 10 * 4, 175))
  expect_equivalent(calcWeights(ex_3.6), c(2.5 * 10 * 4, 662.5))
  expect_equivalent(calcWeights(ex_3.7), c(9.75 * 10, 400))
})

test_that("Labels are correct", {
  weight_names <- c("school.weight", "final.student.weight")
  expect_equal(names(calcWeights(ex_3.3)), weight_names)
  expect_equal(names(calcWeights(ex_3.4)), weight_names)
  expect_equal(names(calcWeights(ex_3.5)), weight_names)
  expect_equal(names(calcWeights(ex_3.6)), weight_names)
  expect_equal(names(calcWeights(ex_3.7)), weight_names)
})

# Exploring different sampling methods -----------------------------------------
calcWeights2 <- function(data_list) {
  p_1_i <- sapply(data_list, function(x) sapply(x, function(y) x[[y]][1, 4]))
  
  w <- sapply(data_list, function(x) colSums(x[4:6]))
  w_sum <- rowSums(w)

  w_1_i <- p_1_i
  n_i_w_ij <- w_sum[3]
  out <- c(w_1_i, n_i_w_ij)
  return(out)
}

n1 <- list(cnt = 1, sch = 3, cls = c(2, 1, 3), stu = rep(2, 6))
N1 <- list(cnt = 1, sch = 9, cls = c(8, 7, 6), stu = rep(8, 6))
ex4 <- wrap_cluster_gen(n1, N1, meth = "SRS", sep = TRUE, collapse = "partial")
ex5 <- wrap_cluster_gen(n1, N1, meth = "PPS", sep = TRUE, collapse = "partial")
ex6 <- wrap_cluster_gen(n1, N1, meth = "mixed", sep = TRUE, collapse = "partial")
ex7 <- wrap_cluster_gen(n1, N1, meth = c("PPS", "SRS", "PPS"), sep = TRUE, collapse = "partial")
ex8 <- wrap_cluster_gen(n = list(cnt =  2,
                                 sch = c(1, 1),
                                 cls = c(5, 7),
                                 stu = rep(10, 12)),
                        N = list(cnt = 10,
                                 sch = c(50, 2),
                                 cls = c(10, 7),
                                 stu = c(rep(50, 5), rep(10, 7))),
                        sep = TRUE, collapse = "partial",
                        meth = c("SRS", "PPS", "SRS"))
# FIXME: check sch.weight. Should add to pop size on each upper level separately
# final.stu.weight should be 500 for cnt1.
test_that("Weights are correct for different sampling methods", {
  w1 <- c(1 * 3 + 3 * (2 + 1 + 3) + 4 * 4 + 7 * 2 + 2 * 6)
  w2 <- c(3 * 3 + 12 * 2 + 21 + 6 * 3 + 16 * 4 + 28 * 2 + 8 * 6)
  expect_equivalent(calcWeights(ex4), c(w1, w2))
  w3 <- c(3 * 3 + 3.5 * 2 + 7 + 7/3 * 3 + 12 * 4 + 24 * 2 + 8 * 6)
  expect_equivalent(calcWeights(ex5)[2], w3)
  expect_equivalent(calcWeights(ex6)[2], w2)
  w4 <- c(3 * 3 + 12 * 2 + 21 + 6 * 3 + 12 * 4 + 24 * 2 + 8 * 6)
  expect_equivalent(calcWeights(ex7)[2], w4)
  w5 <- c(250 + 10 + 3.4 * 5 + 17/7 * 7 + 10 * 50 + 2 * 70)
  expect_equivalent(calcWeights(ex8)[2], w5)
})