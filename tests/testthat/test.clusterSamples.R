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
                    c(1, 10), tolerance = .5)
  expect_equivalent(sapply(df10, function(x) apply(x[[2]][2:3], 2, mean))[, 1],
                    c(1, 10), tolerance = .5)
  expect_equivalent(sapply(df10, function(x) apply(x[[1]][2:3], 2, mean))[, 2],
                    c(1e2, 1e3), tolerance = .5)
  expect_equivalent(sapply(df10, function(x) apply(x[[2]][2:3], 2, mean))[, 2],
                    c(1e2, 1e3), tolerance = .5)
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
test_that("Different means are working", {
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

# Sampling weights =============================================================
context("Sampling weights")
calcWeights <- function(data_list) {
  w <- sapply(data_list, function(x) colSums(x[4:6]))
  w_sum <- rowSums(w)
  w_1_i <- w_sum[1]
  n_i_w_ij <- w_sum[3]
  out <- c(w_1_i, n_i_w_ij)
  return(out)
}

# Custom weight tests ----------------------------------------------------------
test_that("Weights for 2 levels are correct", {
  wrap_cluster_gen <- function(n, N, meth = "SRS", sum_pop = sapply(N, sum),
                             sep = FALSE, verbose = FALSE, ...) {
    data <- cluster_gen(n                       = n,
                        N                       = N,
                        sum_pop                 = sum_pop,
                        n_X                     = 1,
                        n_W                     = 1,
                        sampling_method         = meth,
                        separate_questionnaires = sep,
                        verbose                 = verbose,
                        ...)
    if (verbose) print(data)
    return(data)
  }
  ex1 <- wrap_cluster_gen(n = c(1, 2, 3), N = c(10, 100, 600))
  ex2 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 10)),
                          meth = "PPS")
  ex3 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 10)),
                          meth = "SRS")
  expect_equivalent(calcWeights(ex1)["school.weight"] / 3 / 2, 50)
  expect_equivalent(calcWeights(ex2)["final.student.weight"], 100 * 10)
  expect_equivalent(calcWeights(ex3)["school.weight"], 2.5 * (10 + 5 + 2 + 3))
})

# Example from PISA manual tables ----------------------------------------------
test_that("Weights and labels from PISA examples are correct", {
  wrap_cl_gen <- function(n, N, meth = "SRS", sum_pop = sapply(N, sum),
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
  ex_3.3 <- wrap_cl_gen(n = c(school = 4, student = 10),
                        N = c(        10,           40))
  ex_3.4 <- wrap_cl_gen(
    n = list(school =  4, student = c(10, 10, 10,  10)),
    N = list(school = 10,
             student = c(15, 30, 40, 100, 10, 20, 25, 35, 45, 80))
  )
  ex_3.5 <- wrap_cl_gen(
    n = list(school =  4, student = c(10, 10, 10,  10)),
    N = list(school = 10,
             student = c(10, 15, 20, 25, 30, 35, 40, 45, 80, 100))
  )
  ex_3.6 <- wrap_cl_gen(
    n = list(school = 4, student = c(10, 10, 10,  10)),
    N = list(school = 10,
             student = c(40, 45, 80, 100, 10, 15, 20, 25, 30, 35))
  )
  ex_3.7 <- wrap_cl_gen(
    n = list(school =  4, student = c(10, 10, 10,  10)),
    N = list(school = 10,
             student = c(20, 40, 80, 100, 10, 15, 25, 30, 35, 45)), "PPS"
  )
  expect_equivalent(calcWeights(ex_3.3), c(2.5 * 10 * 4, 400))
  expect_equivalent(calcWeights(ex_3.4), c(2.5 * 10 * 4, 462.5))
  expect_equivalent(calcWeights(ex_3.5), c(2.5 * 10 * 4, 175))
  expect_equivalent(calcWeights(ex_3.6), c(2.5 * 10 * 4, 662.5))
  expect_equivalent(calcWeights(ex_3.7), c(9.75 * 10, 400))

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

# FIXME: replace n1 with "select"
# n1 <- list(cnt = 1, sch = 3, cls = c(2, 1, 3), stu = rep(2, 6))
# N1 <- list(cnt = 1, sch = 9, cls = c(8, 7, 6), stu = rep(8, 6))
# ex4 <- wrap_cluster_gen(n1, N1, meth = "SRS", sep = TRUE, collapse = "partial")
# ex5 <- wrap_cluster_gen(n1, N1, meth = "PPS", sep = TRUE, collapse = "partial")
# ex6 <- wrap_cluster_gen(n1, N1, meth = "mixed", sep = TRUE, collapse = "partial")
# ex7 <- wrap_cluster_gen(n1, N1, meth = c("PPS", "SRS", "PPS"), sep = TRUE, collapse = "partial")
# ex8 <- wrap_cluster_gen(n = list(cnt =  2,
#                                  sch = c(1, 1),
#                                  cls = c(5, 7),
#                                  stu = rep(10, 12)),
#                         N = list(cnt = 10,
#                                  sch = c(50, 2),
#                                  cls = c(10, 7),
#                                  stu = c(rep(50, 5), rep(10, 7))),
#                         sep = TRUE, collapse = "partial",
#                         meth = c("SRS", "PPS", "SRS"))
# FIXME: check sch.weight. Should add to pop size on each upper level separately
# final.stu.weight should be 500 for cnt1.

test_that("Weights are correct for different sampling methods", {
#   w1 <- c(1 * 3 + 3 * (2 + 1 + 3) + 4 * 4 + 7 * 2 + 2 * 6)
#   w2 <- c(3 * 3 + 12 * 2 + 21 + 6 * 3 + 16 * 4 + 28 * 2 + 8 * 6)
#   expect_equivalent(calcWeights(ex4), c(w1, w2))
#   w3 <- c(3 * 3 + 3.5 * 2 + 7 + 7/3 * 3 + 12 * 4 + 24 * 2 + 8 * 6)
#   expect_equivalent(calcWeights(ex5)[2], w3)
#   expect_equivalent(calcWeights(ex6)[2], w2)
#   w4 <- c(3 * 3 + 12 * 2 + 21 + 6 * 3 + 12 * 4 + 24 * 2 + 8 * 6)
#   expect_equivalent(calcWeights(ex7)[2], w4)
#   w5 <- c(250 + 10 + 3.4 * 5 + 17/7 * 7 + 10 * 50 + 2 * 70)
#   expect_equivalent(calcWeights(ex8)[2], w5)
})

# Script for testing with Leslie ===============================================

# This script is meant to aid the verification of the calculation of sampling weights.

# Data examples ----------------------------------------------------------------
# options(width = 150)  # keeps everything in one line for easier visualization

# Example 1: 2-level symmetric structure, census, PPS weights
test_that("Examples worked on with Leslie have correct weights", {
  wrap_cluster_gen <- function(...)
  {
    cluster_gen(..., n_X = 1, n_W = 1, verbose = FALSE)
  }
  #TODO: check weights
  lr1 <- wrap_cluster_gen(n = c(school = 2, student = 10))  # DONE

  # FIXME: wrong weights on lr2.
  # IDEA: force list? use select?
  # lr2 <- wrap_cluster_gen(n = c(school = 2, class = 1, student =  5),
  #                         N = c(school = 5, class = 2, student = 10))
  # FIXME: wrong vector length when calculating weights
  # lr3 <- wrap_cluster_gen(n = list(country = 2,
  #                                  school  = c(2, 3),
  #                                  student = c(10, 20, 6, 9, 12)),
  #                         N = list(country = 10,
  #                                  school  = c(20, 3, rep(1, 8)),
  #                                  student = c(20, 30, 1:18, rep(12, 3), 1:8)))
  expect_equal(
    object = sapply(seq_along(lr1$school),
                    function (x) sum(lr1$school[[x]]["final.student.weight"])),
    expected = c(10, 10)
  )
  # expect_equal(
  #   object = sapply(seq_along(lr2$school),
  #                   function (x) sum(lr1$school[[x]]["final.student.weight"])),
  #   expected = c(10, 10)
  # )
})

# DONE: generate tree and data for level ranges
context("Cluster sampling with ranged number of elements")
check_cluster_structure <- function(n, FUN = "length") {
  set.seed(1234)
  n_list <- convert_vector_to_list(n)
  structure <- draw_cluster_structure(n_list, output="text")
  func <- match.fun(FUN)
  structure_out <- func(structure)
  return(structure_out)
}
n <- c(city = 2, school = 2, class = 3, student = 4)
n2 <- list(city = 2, school = 2, class = 3, student = ranges(10, 50))
n3 <- list(city = 2, school = 2, class = ranges(1, 3), stu = ranges(10, 50))
n4 <- list(city = 2, school = 2, class = ranges(1, 3), student = 20)
n5 <- list(city = 2, school = ranges(5, 8), class = ranges(1, 3), stu = 20)
n6 <- list(2, ranges(1, 3), ranges(2, 5), ranges(1, 5), ranges(5, 100))

test_that("Random levels work", {
  expect_equal(check_cluster_structure(n), 18)
  expect_equal(check_cluster_structure(n2), 18)
  expect_equal(check_cluster_structure(n3), 14)
  expect_equal(check_cluster_structure(n4), 14)
  expect_equal(check_cluster_structure(n5), 50)
  expect_equal(check_cluster_structure(n6), 67)
})

test_that("Random level-generated data generates questionnaires", {
  set.seed(1234); df2 <- cluster_gen(n2, verbose = FALSE, separate = FALSE)
  set.seed(1234); df3 <- cluster_gen(n3, verbose = FALSE)
  set.seed(1234); df4 <- cluster_gen(n4, verbose = FALSE)
  set.seed(7646); df5 <- cluster_gen(n5, verbose = FALSE)
  set.seed(7646); df6 <- cluster_gen(n6, verbose = FALSE)
  expect_equivalent(
    sapply(df2, nrow), c(37, 25, 31, 46, 18, 14, 47, 25, 13, 43, 48, 31)
  )
  expect_equivalent(sapply(df3$school, nrow), c(2, 2, 1, 3))
  expect_equivalent(sapply(df3$class, nrow), c(18, 14, 47, 25, 13, 43, 48, 31))
  expect_equivalent(sapply(df4$school, nrow), c(2, 2, 1, 3))
  expect_equivalent(sapply(df5$city, nrow), c(5, 8))
  expect_equivalent(
    sapply(df5$school, nrow), c(3, 2, 1, 2, 1, 2, 2, 2, 3, 2, 3, 2, 2)
  )
  expect_equivalent(sapply(df6$country, nrow), c(1, 3))
  expect_equivalent(sapply(df6$school, nrow), c(5, 3, 2, 3))
  expect_equivalent(
    sapply(df6$class, nrow), c(4, 1, 2, 2, 2, 3, 2, 2, 4, 5, 4, 5, 4)
  )
  expect_equivalent(
    sapply(df6$unknowncluster, nrow),
    c(87, 99, 21, 26, 87, 29, 41, 57, 10, 86, 92, 67, 53, 18, 38, 89, 75, 37,
      9, 88, 95, 59, 35, 14, 88, 44, 18, 100, 61, 99, 48, 75, 14, 18, 68, 87,
      60, 40, 20, 9)
  )
})

# Testing actual sampling ======================================================
# cl_scheme <- list(school = 2, class = c(3, 2), student = c(5, 4, 5, 5, 5))
# cl_scheme2 <- list(country = 5,
#                    school  = c(20, 8, 5, 7, 3),
#                    student = c(20, 30, 12, 12, 12))
# drawClusterStructure(cl_scheme)
# drawClusterStructure(cl_scheme2) #FIXME: incomplete is wrong
# cluster_gen(n = cl_scheme)
# cluster_gen(n = select(1, 2, 4), N = cl_scheme)


context("Replicate weights")
# set.seed(230)
# df <- cluster_gen(c(sch = 4, stu = 10), n_X = 3, n_W = 1, verb = TRUE)
# jackknife(df$sch[[4]], c("sch.weight", "final.stu.weight"))
# df2 <- replicate_weights(df, var_method = "Jackknife", verbose = FALSE, print_data = TRUE)=