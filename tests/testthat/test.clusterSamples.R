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
  expect_equal(df04$school[[1]]$uniqueID[1], "A1_school1")
  expect_equal(df05$B[[3]]$uniqueID[4], "D4_B3_A1")
  expect_equal(
    as.vector(sapply(
      df06$school,
      function(c) sapply(c[1:3], class)
    )),
    rep(c("integer", "numeric", "factor"), 2)
  )
  expect_equal(as.vector(sapply(df07$school, function(c) sapply(c, class))),
               rep(c("integer", "numeric", "factor",
                   rep("numeric", "numeric", 3), "character"), 2))
  expect_equal(as.vector(sapply(df08$school, function(c) sapply(c, class))),
               rep(c("integer", rep("numeric", 2), rep("factor", 3),
                     rep("numeric", 3), "character"), 2))
  expect_output(str(df09$school[[1]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$school[[1]]$q2), "Factor w/ 2 levels")
  expect_output(str(df09$school[[2]]$q1), "Factor w/ 5 levels")
  expect_output(str(df09$school[[2]]$q2), "Factor w/ 2 levels")
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
  wrap_cluster_gen_2 <- function(..., coll = "full", return_all = FALSE,
                                 verb = FALSE) {
    data <- cluster_gen(..., n_X = 0, n_W = 1, family = "gaussian",
                        verbose = verb, collapse = coll)
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
  expect_equal(df1,
    c("student1_class1_school1", "student2_class1_school1", 
      "student3_class1_school1", "student4_class1_school1", 
      "student1_class1_school2", "student2_class1_school2", 
      "student3_class1_school2", "student4_class1_school2", 
      "student1_class2_school1", "student2_class2_school1", 
      "student3_class2_school1", "student4_class2_school1", 
      "student1_class2_school2", "student2_class2_school2", 
      "student3_class2_school2", "student4_class2_school2", 
      "student1_class3_school1", "student2_class3_school1", 
      "student3_class3_school1", "student4_class3_school1", 
      "student1_class3_school2", "student2_class3_school2", 
      "student3_class3_school2", "student4_class3_school2")
  )
  expect_equal(df2,
    c("student1_class1_school1", "student2_class1_school1",
     "student3_class1_school1", "student4_class1_school1", 
     "student1_class2_school1", "student2_class2_school1", 
     "student3_class2_school1", "student4_class2_school1", 
     "student1_class3_school1", "student2_class3_school1", 
     "student3_class3_school1", "student4_class3_school1", 
     "student1_class1_school2", "student2_class1_school2", 
     "student3_class1_school2", "student4_class1_school2", 
     "student1_class2_school2", "student2_class2_school2", 
     "student3_class2_school2", "student4_class2_school2", 
     "student1_class3_school2", "student2_class3_school2", 
     "student3_class3_school2", "student4_class3_school2")
  )
  expect_equal(df3,
    c("student1_class1_school1_state1", "student2_class1_school1_state1", 
    "student3_class1_school1_state1", "student1_class1_school2_state1", 
    "student2_class1_school2_state1", "student1_class2_school2_state1", 
    "student2_class2_school2_state1", "student3_class2_school2_state1")
  )
  expect_equal(df4,
    c("student1_class1_school1_state1", "student2_class1_school1_state1", 
    "student3_class1_school1_state1", "student1_class1_school2_state1", 
    "student2_class1_school2_state1", "student1_class2_school2_state1", 
    "student2_class2_school2_state1", "student3_class2_school2_state1")
  )
  expect_equal(df6,
    c("student1_class1_school1_state1", "student2_class1_school1_state1", 
    "student1_class1_school1_state2", "student2_class1_school1_state2", 
    "student1_class1_school2_state1", "student2_class1_school2_state1", 
    "student1_class1_school2_state2", "student2_class1_school2_state2", 
    "student1_class1_school3_state1", "student2_class1_school3_state1", 
    "student1_class2_school1_state2", "student2_class2_school1_state2", 
    "student1_class2_school2_state2", "student2_class2_school2_state2", 
    "student1_class2_school3_state1", "student2_class2_school3_state1", 
    "student1_class3_school1_state2", "student2_class3_school1_state2")
  )
  expect_equal(df7,
    c("student1_class1_school1_state1", "student2_class1_school1_state1", 
    "student1_class1_school2_state1", "student2_class1_school2_state1", 
    "student1_class1_school3_state1", "student2_class1_school3_state1", 
    "student1_class2_school3_state1", "student2_class2_school3_state1", 
    "student1_class1_school1_state2", "student2_class1_school1_state2", 
    "student1_class2_school1_state2", "student2_class2_school1_state2", 
    "student1_class3_school1_state2", "student2_class3_school1_state2", 
    "student1_class1_school2_state2", "student2_class1_school2_state2", 
    "student1_class2_school2_state2", "student2_class2_school2_state2")
  )
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
  expect_equivalent(sapply(df1$school, mean_Xs)[, 1], c(0, 0), tol = .5)
  expect_equivalent(sapply(df1$school, mean_Xs)[, 2], c(0, 0), tol = .5)
  expect_equivalent(sapply(df2$school, mean_Xs)[, 1], c(10, 10), tol = .5)
  expect_equivalent(sapply(df2$school, mean_Xs)[, 2], c(10, 10), tol = .5)
  expect_equivalent(sapply(df3$school, mean_Xs)[, 1], c(10, 100), tol = .5)
  expect_equivalent(sapply(df3$school, mean_Xs)[, 2], c(10, 100), tol = .5)
  expect_equivalent(sapply(df4$school, mean_Xs)[, 1], c(10, 100), tol = .5)
  expect_equivalent(sapply(df4$school, mean_Xs)[, 2], c(20, 200), tol = .5)
  expect_equivalent(sapply(df5$school, mean_Xs)[, 1], c(10, 100), tol = .5)
  expect_equivalent(sapply(df5$school, mean_Xs)[, 2], c(20, 200), tol = .5)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 1], c(30, 300), tol = .5)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 2], c(40, 400), tol = .5)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 3], c(50, 500), tol = .5)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 4], c(60, 600), tol = .5)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 5], c(70, 700), tol = .5)
  expect_equivalent(sapply(df5$class, mean_Xs)[, 6], c(80, 800), tol = .5)
})

# Sampling weights =============================================================
context("Sampling weights")
calc_weights <- function(data_list) {
  w <- sapply(data_list, function(x) colSums(x[4:6]))
  w_sum <- rowSums(w)
  w_1_i <- w_sum[1]
  n_i_w_ij <- w_sum[3]
  out <- c(w_1_i, n_i_w_ij)
  return(out)
}

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
  expect_equivalent(calc_weights(ex_3.3), c(2.5 * 10 * 4, 400))
  expect_equivalent(calc_weights(ex_3.4), c(2.5 * 10 * 4, 462.5))
  expect_equivalent(calc_weights(ex_3.5), c(2.5 * 10 * 4, 175))
  expect_equivalent(calc_weights(ex_3.6), c(2.5 * 10 * 4, 662.5))
  expect_equivalent(calc_weights(ex_3.7), c(9.75 * 10, 400))

  weight_names <- c("school.weight", "final.student.weight")
  expect_equal(names(calc_weights(ex_3.3)), weight_names)
  expect_equal(names(calc_weights(ex_3.4)), weight_names)
  expect_equal(names(calc_weights(ex_3.5)), weight_names)
  expect_equal(names(calc_weights(ex_3.6)), weight_names)
  expect_equal(names(calc_weights(ex_3.7)), weight_names)
})

# Custom weight tests ----------------------------------------------------------
wrap_cluster_gen <- function(n, N, meth = "SRS", sum_pop = sapply(N, sum),
                               sep = FALSE, verb = FALSE, print = FALSE, ...) {
    data <- cluster_gen(n                       = n,
                        N                       = N,
                        sum_pop                 = sum_pop,
                        n_X                     = 1,
                        n_W                     = 1,
                        sampling_method         = meth,
                        separate_questionnaires = sep,
                        verbose                 = verb,
                        ...)
    if (print) print(data)
    return(data)
}
test_that("Sampling weights are correct", {
  ex1 <- wrap_cluster_gen(n = c(1, 2, 3), N = c(10, 100, 600))
  ex2 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 10)),
                          meth = "PPS")
  ex3 <- wrap_cluster_gen(n = list(school = 4, student = c(10, 5, 2, 3)),
                          N = list(school = 10, students = rep(100, 10)),
                          meth = "PPS")
  expect_equivalent(calc_weights(ex1)["class.weight"] / 3, 100 * 10)
  expect_equivalent(calc_weights(ex2)["final.student.weight"], 100 * 10)
  expect_equivalent(calc_weights(ex3)["school.weight"], 2.5 * (10 + 5 + 2 + 3))
})

# Exploring different sampling methods -----------------------------------------
n1 <- list(cnt = 1, sch = 3, cls = c(2, 1, 3), stu = rep(2, 6))
N1 <- list(cnt = 1, sch = 5, cls = 8:4, stu = rep(8, sum(8:4)))
ex4 <- wrap_cluster_gen(n1, N1, meth = "SRS", sep = TRUE)
ex5 <- wrap_cluster_gen(n1, N1, meth = "PPS", sep = TRUE)
ex6 <- wrap_cluster_gen(n1, N1, meth = c("PPS", "SRS", "PPS"), sep = TRUE)
n2 <- list(1, 3, c(2, 1, 3), rep(2, 6))
N2 <- list(1, 5, 8:4, rep(8, sum(8:4)))
ex7 <- wrap_cluster_gen(n2, N2, meth = "mixed", sep = TRUE,
                        cluster_labels = c("state", "school", "class"),
                        resp_labels = c("governor", "principal", "student"))
test_that("Weights are correct for different sampling methods", {
  expect_equivalent(calc_weights(ex4$cnt)["cnt.weight"], 1 * 3)
  expect_equivalent(calc_weights(ex4$sch)["sch.weight"], (5 / 3) * 6)
  expect_equivalent(calc_weights(ex4$cls)["cls.weight"], 5 * 2 * 6)
  expect_equivalent(calc_weights(ex5$cnt)["final.sch.weight"], 5)
  expect_equivalent(calc_weights(ex5$sch)["final.cls.weight"], sum(8:4))
  expect_equivalent(calc_weights(ex5$cls)["final.stu.weight"],
                    sum(rep(8, sum(8:4))))
  expect_equivalent(calc_weights(ex6$cnt)["final.sch.weight"], 5)
  expect_equivalent(calc_weights(ex6$sch)["sch.weight"], (5 / 3) * 6)
  expect_equivalent(calc_weights(ex6$cls)["final.stu.weight"],
                    sum(rep(8, sum(8:4))))
  expect_equivalent(calc_weights(ex7$state)["state.weight"], 1 * 3)
  expect_equivalent(calc_weights(ex7$school)["final.principal.weight"], sum(8:4))
  expect_equivalent(calc_weights(ex7$class)["class.weight"], 5 * 2 * 6)
})

# Script for testing with Leslie ===============================================
test_that("Examples worked on with Leslie have correct weights", {
  wrap_cluster_gen <- function(..., verb = FALSE) {
    cluster_gen(..., n_X = 1, n_W = 1, verbose = verb)
  }
  lr1 <- wrap_cluster_gen(n = c(school = 2, student = 10))
  lr2 <- wrap_cluster_gen(n = c(school = 2, class = 1, student =  5), 
                          N = c(school = 5, class = 2, student = 10))
  lr3 <- wrap_cluster_gen(n = list(state = 2,
                                   school  = c(2, 3),
                                   student = c(10, 20, 6, 9, 12)),
                          N = list(state = 10,
                                   school  = c(20, 3, rep(1, 8)),
                                   student = c(20, 30, 1:18, rep(12, 3), 1:8)))
  expect_equal(
    object = sapply(seq_along(lr1$school),
                    function (x) sum(lr1$school[[x]]["final.student.weight"])),
    expected = rep(10, 2)
  )
  expect_equal(
    object = sapply(seq_along(lr2$school),
                    function (x) sum(lr2$school[[x]]["final.class.weight"])),
    expected = rep(5, 2)
  )
  expect_equal(
    object = sapply(seq_along(lr2$class),
                    function (x) sum(lr2$class[[x]]["final.student.weight"])),
    expected = rep(50, 2)
  )
  expect_equal(
    object = sapply(seq_along(lr3$state),
                    function (x) sum(lr3$state[[x]]["state.weight"])),
    expected = 5 * c(2, 3)
  )
  expect_equal(
    object = sapply(seq_along(lr3$school),
                    function (x) sum(lr3$school[[x]]["final.student.weight"])),
    expected = rep(58.6, 5)
  )
})

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
  expect_equivalent(sapply(df6$state, nrow), c(1, 3))
  expect_equivalent(sapply(df6$city, nrow), c(5, 3, 2, 3))
  expect_equivalent(
    sapply(df6$school, nrow), c(4, 1, 2, 2, 2, 3, 2, 2, 4, 5, 4, 5, 4)
  )
  expect_equivalent(
    sapply(df6$class, nrow),
    c(87, 99, 21, 26, 87, 29, 41, 57, 10, 86, 92, 67, 53, 18, 38, 89, 75, 37,
      9, 88, 95, 59, 35, 14, 88, 44, 18, 100, 61, 99, 48, 75, 14, 18, 68, 87,
      60, 40, 20, 9)
  )
})

# DONE: add the following as tests
test_that("Combinations of ranges for n and N are treated correctly", {
  wrap_cluster_gen_3 <- function(n, N, ...) {
    cluster_gen(n       = n,
                N       = N,
                n_X     = 1,
                n_W     = 0,
                verbose = FALSE)
  }
  # Templates for n and N
  n_combos_2 <- list(
    n_vect_nn = c(4, 5),
    n_list_nr = list(4,            ranges(5, 10)),
    n_list_rn = list(ranges(1, 5), 5),
    n_list_rr = list(ranges(1, 5), ranges(5, 10))
  )
  n_combos_3 <- list(
    n_vect_nnn = c(4, 3, 5),
    n_list_nnr = list(4,            3,            ranges(5, 10)),
    n_list_nrn = list(4,            ranges(1, 3), 5),
    n_list_nrr = list(4,            ranges(2, 3), ranges(5, 10)),
    n_list_rnn = list(ranges(1, 5), 3,            5),
    n_list_rnr = list(ranges(1, 5), 3,            ranges(5, 10)),
    n_list_rrn = list(ranges(1, 5), ranges(2, 3), 5),
    n_list_rrr = list(ranges(1, 5), ranges(2, 3), ranges(5, 10))
  )
  N_combos_2 <- list(
    N_nn = list(10,            rep(10, 10)),
    N_nr = list(10,            ranges(10, 20)),
    N_rn = list(ranges(5, 10), ranges(10, 20))
  )
  N_combos_3 <- list(
    N_nnr = list(10,           3,            ranges(10, 30)),
    N_nrn = list(10,           ranges(1, 3), 10),
    N_nrr = list(10,           ranges(1, 3), ranges(10, 30)),
    N_rnn = list(ranges(1, 5), 3,            10),
    N_rnr = list(ranges(1, 5), 3,            ranges(10, 30)),
    N_rrn = list(ranges(1, 5), ranges(1, 3), 10),
    N_rrr = list(ranges(1, 5), ranges(1, 3), ranges(10, 30))
  )

  # Data combininng templates
  data <- list()
  for (n in names(n_combos_2)) {
    for (N in names(N_combos_2)) {
      name <- paste(n, N, sep=",")
      data[[name]] <- wrap_cluster_gen_3(n_combos_2[[n]], N_combos_2[[N]])
    }
  }
  for (n in names(n_combos_3)) {
    for (N in names(N_combos_3)) {
      name <- paste(n, N, sep=",")
      data[[name]] <- wrap_cluster_gen_3(n_combos_3[[n]], N_combos_3[[N]])
    }
  }

  expect_length(data, 68)
})

# Testing actual sampling ======================================================
# cl_scheme <- list(school = 2, class = c(3, 2), student = c(5, 4, 5, 5, 5))
# cl_scheme2 <- list(country = 5,
#                    school  = c(20, 8, 5, 7, 3),
#                    student = c(20, 30, 12, 12, 12))
# draw_cluster_structure(cl_scheme)
# draw_cluster_structure(cl_scheme2)
# cluster_gen(n = cl_scheme)
# cluster_gen(n = select(1, 2, 4), N = cl_scheme)


context("Replicate weights")
#TODO: turn this into a test
test_that("Replication weights are correct", {
  set.seed(230)
  df <- cluster_gen(c(sch = 4, stu = 10), n_X = 3, n_W = 1, verb = FALSE)
  # replicate_var(df$sch[[1]], method = "Jackknife", full_output = TRUE)
  # replicate_var(df$sch[[1]], method = "BRR")
  # replicate_var(df$sch[[1]], method = "BRR Fay")
  # sampling_variance(df, "BRR")
  # sampling_variance(df, "BRR Fay")
  df2 <- cluster_gen(c(4, 2, 50), N = 2, n_X = 3, n_W = 1, verb = FALSE)
  # sampling_variance(df2, "Jackknife")
  # sampling_variance(df2, "BRR")
  # sampling_variance(df2, "BRR Fay")
  expect_equivalent(
    unlist(sampling_variance(df, "Jackknife")),
    c(0.15831098532366, 0.258632952368482, -0.62911601852465, 
      0.223732792213627, 0.379914386612429, 0.226530527465725, 
      0.465956763496715, 0.122538887989038, -0.215586976377966, 
      0.219496830387965, 0.0076354930021202, 0.199264256994936, 
      0.224432723636251, 0.359642650100045, 0.578239078356181, 
      0.291615179333453, 0.556626353406706, 0.3801385816327, 
      -0.227181248467164, 0.242076691331234, -0.0620894218594812, 
      0.346261672701384, -0.0581337067871681, 0.384841366539278)
  )
  expect_equivalent(
    unlist(sampling_variance(df, "BRR")),
    c(0.15831098532366, 0.269485760050326, -0.62911601852465,
      0.254588909740161, 0.379914386612429, 0.268409701935955, 
      0.465956763496715, 0.0873360105280653, -0.215586976377966, 
      0.184624186558973, 0.0076354930021202, 0.118301213273541, 
      0.224432723636251, 0.422007167636833, 0.578239078356181, 
      0.198303332648028, 0.556626353406706, 0.198541074393956, 
      -0.227181248467164, 0.180621505701521, -0.0620894218594812, 
      0.152984003569588, -0.0581337067871681, 0.395061693299737)
  )
  expect_equivalent(
    unlist(sampling_variance(df, "BRR Fay")), 
    c(0.15831098532366, 0.379248209587238, -0.62911601852465,
      0.379819744817198, 0.379914386612429, 0.350836556219722, 
      0.465956763496715, 0.113886829482729, -0.215586976377966, 
      0.178355423879951, 0.0076354930021202, 0.0610610454785091, 
      0.224432723636251, 0.313471328887017, 0.578239078356181, 
      0.266800689681702, 0.556626353406706, 0.277889504577754, 
      -0.227181248467164, 0.156902907786301, -0.0620894218594812, 
      0.165658026319974, -0.0581337067871681, 0.439462199482152)
  )
  expect_equivalent(
    unlist(sampling_variance(df2, "Jackknife")),
    c(-0.916824527432973, 0.585248050605567, -0.755269467569318, 
      0.622025259048983, 0.0786510882952661, 0.181525188864791, 
      -0.606839801057376, 1.03049758012258, -0.618871552356925, 
      0.282148522498351, 0.146483636734465, 0.0976261057587896, 
      0.343931796736848, 0.816897204488609, -0.35188683035428, 
      0.690584218757351, -0.184486361070713, 1.00983572994238, 
      -0.976715605943166, 0.540907960136706, 0.0890315799545073, 
      0.336462604195358, 0.317801416544627, 0.249621900732227, 
      0.141998304572704, 0.1602797038198, -0.175906062361598,
      0.153258958539661, -0.149770903650374, 0.157755652092409, 
      0.0163792442398034, 0.135213617551386, -0.0397372876671378,
      0.159340899464703, -0.0834884431341278, 0.149850756571017, 
      -0.00655517064210713, 0.123760221295559, 0.141742829314873, 
      0.126276758707677, -0.113622268459726, 0.127345153269018, 
      -0.0888334931299791, 0.153586536906821, 0.0556864355733864, 
      0.139081163199771, -0.0381207933251701, 0.10486442721813, 
      0.000445280554633429, 0.149730213851758, -0.0834422817368653, 
      0.161809157858918, -0.0739157837633565, 0.164254011877234,
      -0.227348584395448, 0.132116564828475, 0.129831535059066, 
      0.13269215210621, 0.0570658019177376, 0.150714741523559, 
      -0.128380197829855, 0.137984066948075, -0.102145340128612, 
      0.135059552593661, -0.138849509085631, 0.132370933441646, 
      0.0711903387149986, 0.147792294277672, -0.0489083764770658, 0.126945564691283, -0.0826242295517048, 0.128967289216484)
  )
  expect_equivalent(
    unlist(sampling_variance(df2, "BRR")),
    c(-0.916824527432973, 0.585248050605567, -0.755269467569318,
      0.622025259048983, 0.0786510882952661, 0.181525188864791, 
      -0.606839801057376, 1.03049758012258, -0.618871552356925, 
      0.282148522498351, 0.146483636734465, 0.0976261057587896, 
      0.343931796736848, 0.816897204488609, -0.35188683035428, 
      0.690584218757351, -0.184486361070713, 1.00983572994238, 
      -0.976715605943166, 0.540907960136706, 0.0890315799545073, 
      0.336462604195358, 0.317801416544627, 0.249621900732227, 
      0.141998304572704, 0.175905925306331, -0.175906062361598,
      0.162965234651872, -0.149770903650374, 0.189311614718682, 
      0.0163792442398034, 0.13679695469435, -0.0397372876671378, 
      0.147016195637783, -0.0834884431341278, 0.112122350362824, 
      -0.00655517064210713, 0.12535690340583, 0.141742829314873, 
      0.0977152220401755, -0.113622268459726, 0.115820777444533, 
      -0.0888334931299791, 0.148649733980349, 0.0556864355733864, 
      0.131051935860468, -0.0381207933251701, 0.114230525491531, 
      0.000445280554633429, 0.1759503905232, -0.0834422817368653, 
      0.189676244310998, -0.0739157837633565, 0.185244267660356,
      -0.227348584395448, 0.152049117042316, 0.129831535059066, 
      0.160006825815814, 0.0570658019177376, 0.181716915344519, 
      -0.128380197829855, 0.167367720938153, -0.102145340128612, 
      0.195318985179616, -0.138849509085631, 0.136274268300512, 
      0.0711903387149986, 0.11243645512546, -0.0489083764770658, 
      0.114267671328122, -0.0826242295517048, 0.103135430261304)
  )
  expect_equivalent(
    unlist(sampling_variance(df2, "BRR Fay")), 
    c(-0.916824527432973, 0.585248050605567, -0.755269467569318,
      0.622025259048983, 0.0786510882952661, 0.181525188864791, 
      -0.606839801057376, 1.03049758012258, -0.618871552356925, 
      0.282148522498351, 0.146483636734465, 0.0976261057587896, 
      0.343931796736848, 0.816897204488609, -0.35188683035428, 
      0.690584218757351, -0.184486361070713, 1.00983572994238, 
      -0.976715605943166, 0.540907960136706, 0.0890315799545073, 
      0.336462604195358, 0.317801416544627, 0.249621900732227, 
      0.141998304572704, 0.149601197872825, -0.175906062361598,
      0.152198047972994, -0.149770903650374, 0.139981141808025, 
      0.0163792442398034, 0.134242732342061, -0.0397372876671378, 
      0.147752600974845, -0.0834884431341278, 0.104421525364961, 
      -0.00655517064210713, 0.164604461820054, 0.141742829314873, 
      0.145784401745871, -0.113622268459726, 0.126676202615967, 
      -0.0888334931299791, 0.139112478741471, 0.0556864355733864, 
      0.138342827632241, -0.0381207933251701, 0.109728969361484, 
      0.000445280554633429, 0.174869765069336, -0.0834422817368653, 
      0.188648800434544, -0.0739157837633565, 0.187890139742511,
      -0.227348584395448, 0.112316549812096, 0.129831535059066, 
      0.158327645769281, 0.0570658019177376, 0.182787734435263, 
      -0.128380197829855, 0.13782966908026, -0.102145340128612, 
      0.12693254490925, -0.138849509085631, 0.129532439735117, 
      0.0711903387149986, 0.142178007870234, -0.0489083764770658, 
      0.127247099246219, -0.0826242295517048, 0.0979339407597531)
  )
  # cat(paste(unname(unlist(sampling_variance(df, "BRR Fay"))), collapse = ", "))
})