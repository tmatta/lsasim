context("Basic cluster sampling")
wrap_cluster_gen <- function(...) {
  cluster_gen(..., family = "gaussian", verbose = FALSE)
}

# Basic argument handling =====================================================
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
  df08 <- wrap_cluster_gen(2:3, n_X = 2:3, n_W = 3)
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

# Errors are caught ===========================================================
test_that("Errors are caught", {
  expect_error(cluster_gen(1))
  expect_error(cluster_gen(2:4, separate_questionnaires = FALSE, n_X = 1:2))
  expect_error(cluster_gen(2:4, separate_questionnaires = FALSE, n_W = 1:2))
  expect_error(cluster_gen(2:4, cluster_labels = "a"))
  expect_warning(cluster_gen(2:4, separate_quest = FALSE, collapse = "partial",
                             verbose = FALSE))
})

# uniqueIDs are correct =======================================================
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

# Named n vector ==============================================================
test_that("Named vectors are working properly", {
  df1 <- cluster_gen(n       = c("land" = 1, "skole" = 3, "klasse" = 2),
                     verbose = FALSE,
                     collapse = "full")
  df2 <- cluster_gen(n       = list("pais" = 1, "cidade" = 4,
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

# Different means =============================================================
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

# Example from PISA manual tables ==============================================
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

# Custom weight tests ==========================================================
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

# Exploring different sampling methods =========================================
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
    suppressWarnings(cluster_gen(..., n_X = 1, n_W = 1, verbose = verb))
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

# Ranges for n and N ==========================================================
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
  expect_equal(check_cluster_structure(n3), 13)
  expect_equal(check_cluster_structure(n4), 11)
  expect_equal(check_cluster_structure(n5), 40)
  expect_equal(check_cluster_structure(n6), 37)
})

test_that("Random level-generated data generates questionnaires", {
  set.seed(1234); df2 <- cluster_gen(n2, verbose = FALSE, separate = FALSE)
  set.seed(1234); df3 <- cluster_gen(n3, verbose = FALSE)
  set.seed(1234); df4 <- cluster_gen(n4, verbose = FALSE)
  set.seed(7646); df5 <- cluster_gen(n5, verbose = FALSE)
  set.seed(7646); df6 <- cluster_gen(n6, verbose = FALSE)
  expect_output(str(df2), "List of 12")
  expect_output(str(df3$school), "List of 4")
  expect_output(str(df3$class), "List of 7")
  expect_output(str(df4$school), "List of 4")
  expect_output(str(df5$city), "List of 2")
  expect_output(str(df5$school), "List of 13")
  expect_output(str(df6$state), "List of 2")
  expect_output(str(df6$city), "List of 3")
  expect_output(str(df6$school), "List of 8")
  expect_output(str(df6$class), "List of 17")
})

test_that("Combinations of ranges for n and N are treated correctly", {
  wrap_cluster_gen_3 <- function(n, N, ...) {
    suppressWarnings(
      cluster_gen(n       = n,
                  N       = N,
                  n_X     = 1,
                  n_W     = 0,
                  verbose = FALSE)
    )
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

  # Data combining templates
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

test_that("N cannot be smaller than n", {
  N7 <- list(ranges(1, 5), ranges(2, 5), ranges(5, 10))
  n7 <- list(ranges(2, 4), 6, ranges(5, 15))
  set.seed(212128)
  expect_warning(df7 <- cluster_gen(n7, N = N7, verbose = FALSE))
  expect_warning(df8 <- cluster_gen(n = 2:4, N = 1:3, verbose = FALSE))
  expect_warning(df9 <- cluster_gen(n = list(2, 3:4, ranges(3, 9)),
                                    N = list(1, 2, 1:2), verbose = FALSE))
  expect_output(str(df7), "List of 2")
  expect_output(str(df8), "List of 2")
  expect_output(str(df9), "List of 2")
})

test_that("n cannor be larger than N", {
  n7 <- list(school = ranges(5, 10), student = ranges(10, 20))
  N7 <- c(150, 40)
  set.seed(683)
  expect_output(str(cluster_gen(n7, N7, verbose = FALSE)), "List of 7")
})

# Replicate weights ===========================================================
context("Replicate weights")
test_that("Replication weights are correct", {
  set.seed(230)
  df <- cluster_gen(c(sch = 4, stu = 10), n_X = 3, n_W = 1, verb = FALSE)
  df2 <- cluster_gen(c(4, 2, 50), N = 2, n_X = 3, n_W = 1, verb = FALSE)
  expect_equivalent(
    unlist(calc_replicate_weights(df, "Jackknife")),
    c(0.1583, 0.25863, -0.6291, 0.22373, 0.37991, 0.22653, 0.46595, 0.12253,
     -0.21558, 0.21949, 0.007635, 0.19926, 0.22443, 0.35964, 0.57823, 0.29161,
      0.55662, 0.380, -0.22718, 0.24207, -0.062089, 0.34626, -0.058133,
      0.38484),
    tolerance = .001
  )
  expect_equivalent(
    unlist(calc_replicate_weights(df, "BRR")),
    c(0.1583, 0.26948, -0.6291,
      0.25458, 0.37991, 0.26840, 0.46595, 0.087336, -0.21558, 0.18462, 0.007635,
      0.11830, 0.22443, 0.42200, 0.57823, 0.19830, 0.55662, 0.19854, -0.22718,
      0.18062, -0.062089, 0.15298, -0.058133, 0.39506),
    tolerance = .001
  )
  expect_equivalent(
    unlist(calc_replicate_weights(df, "BRR Fay")), c(0.1583, 0.37924, -0.6291,
      0.37981, 0.37991, 0.35083, 0.46595, 0.11388, -0.21558, 0.17835, 0.007635,
      0.061061, 0.22443, 0.31347, 0.57823, 0.26680, 0.55662, 0.27788, -0.22718,
      0.15690, -0.062089, 0.16565, -0.058133, 0.43946),
    tolerance = .001
  )
  expect_equivalent(
    unlist(calc_replicate_weights(df2, "Jackknife")),
    c(-0.91682, 0.58524, -0.75526, 0.62202, 0.078651, 0.18152, -0.60683, 1.0304,
      -0.61887, 0.28214, 0.14648, 0.097626, 0.34393, 0.81689, -0.3518, 0.69058,
      -0.18448, 1.0098, -0.97671, 0.54090, 0.089031, 0.33646, 0.31780, 0.24962,
      0.14199, 0.160, -0.17590, 0.15325, -0.14977, 0.15775, 0.016379, 0.13521,
      -0.039737, 0.15934, -0.083488, 0.14985, -0.0065551, 0.12376, 0.14174,
      0.12627, -0.11362, 0.12734, -0.088833, 0.15358, 0.055686, 0.13908,
      -0.038120, 0.1048, 0.00044528, 0.14973, -0.083442, 0.16180, -0.073915,
      0.16425, -0.22734, 0.13211, 0.12983, 0.1326, 0.057065, 0.15071, -0.12838,
      0.13798, -0.10214, 0.13505, -0.13884, 0.13237, 0.071190, 0.14779,
      -0.048908, 0.12694, -0.082624, 0.12896),
    tolerance = .001
  )
  expect_equivalent(
    unlist(calc_replicate_weights(df2, "BRR")),
    c(-0.91682, 0.58524, -0.75526, 0.62202, 0.078651, 0.18152, -0.60683, 1.0304,
      -0.61887, 0.28214, 0.14648, 0.097626, 0.34393, 0.81689, -0.3518, 0.69058,
      -0.18448, 1.0098, -0.97671, 0.54090, 0.089031, 0.33646, 0.31780, 0.24962,
      0.14199, 0.17590, -0.17590, 0.16296, -0.14977, 0.18931, 0.016379, 0.1367,
      -0.039737, 0.14701, -0.083488, 0.11212, -0.0065551, 0.1253, 0.14174,
      0.097715, -0.11362, 0.11582, -0.088833, 0.14864, 0.055686, 0.13105,
      -0.038120, 0.11423, 0.00044528, 0.175, -0.083442, 0.18967, -0.073915,
      0.18524, -0.22734, 0.15204, 0.12983, 0.16000, 0.057065, 0.18171, -0.12838,
      0.16736, -0.10214, 0.19531, -0.13884, 0.13627, 0.071190, 0.1124,
      -0.048908, 0.11426, -0.082624, 0.10313),
    tolerance = .001
  )
  expect_equivalent(
    unlist(calc_replicate_weights(df2, "BRR Fay")),
    c(-0.91682, 0.58524, -0.75526, 0.62202, 0.078651, 0.18152, -0.60683, 1.0304,
      -0.61887, 0.28214, 0.14648, 0.097626, 0.34393, 0.81689, -0.3518, 0.69058,
      -0.18448, 1.0098, -0.97671, 0.54090, 0.089031, 0.33646, 0.31780, 0.24962,
      0.14199, 0.14960, -0.17590, 0.15219, -0.14977, 0.13998, 0.016379, 0.13424,
      -0.039737, 0.14775, -0.083488, 0.10442, -0.0065551, 0.16460, 0.14174,
      0.14578, -0.11362, 0.12667, -0.088833, 0.13911, 0.055686, 0.13834,
      -0.038120, 0.10972, 0.00044528, 0.17486, -0.083442, 0.18864, -0.073915,
      0.18789, -0.22734, 0.11231, 0.12983, 0.15832, 0.057065, 0.18278, -0.12838,
      0.1378, -0.10214, 0.1269, -0.13884, 0.12953, 0.071190, 0.14217, -0.048908,
      0.12724, -0.082624, 0.097933),
    tolerance = .001
  )
  # Tests shown to Eugene on 4/sep/2019
  set.seed(1127)
  w <- cluster_gen(N = c(school = 3, class = 2, student = 10),
                   n = c(school = 2, class = 2, student = 5), verbose = FALSE)
  x <- cluster_gen(N = list(school = 2, class = c(2, 3),
                            student = c(6, 7, 3, 2, 9)),
                   n = list(school = 2, class = c(1, 2),
                            student = c(2, 2, 2)), verbose = FALSE)
  y <- cluster_gen(c(4, 2, 50), N = 2, n_X = 3, n_W = 1, verbose = FALSE)
  z <- cluster_gen(n = c(sch = 20, stu = 5),
                   N = c(sch = 1e2, stu = 20),
                   n_X = 3, n_W = 1,
                   print_pop_structure = FALSE, verbose = FALSE)
  expect_equivalent(mean(unlist(calc_replicate_weights(w, "Jackknife"))), 0.2, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(w, "BRR"))), 0.2, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(w, "BRR Fay"))), 0.2, .1)
  expect_equal(mean(unlist(calc_replicate_weights(x, "Jackknife"))), NaN)
  expect_equal(mean(unlist(calc_replicate_weights(x, "BRR"))), 0.4, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(x, "BRR Fay"))), 0.4, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(y, "Jackknife"))), 0.1, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(y, "BRR"))), 0.1, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(y, "BRR Fay"))), 0.1, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(z, "Jackknife"))), 0.2, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(z, "BRR"))), 0.2, .1)
  expect_equivalent(mean(unlist(calc_replicate_weights(z, "BRR Fay"))), 0.2, .1)
})

# Intraclass correlations =====================================================
context("Intraclass correlations")
reps <- 100
rep_stats <- matrix(nrow = reps, ncol = 14)
retrieved <- vector()
bias <- vector()
for (r in seq_len(reps)) {
  rho <- runif(1)
  df <- cluster_gen(c(rpois(1, 10), rpois(1, 100)), n_X = 2, n_W = 0,
                    rho = rho,
                    sigma = rpois(1, 10),
                    verbose = FALSE)
  df_stats <- anova(df, FALSE)
  rep_stats[r, ] <- unlist(df_stats)
  bias <- append(bias, rep_stats[r, 9] - rho)
}
colnames(rep_stats) <- names(unlist(df_stats))
test_that("Observed rho is an unbiased estimator", {
  expect_equivalent(mean(bias), 0, tol = .1)
})
test_that("Rho changes as expected", {
  rho <- c(.9, .3, .2)
  set.seed(8141221)
  df <- cluster_gen(c(40, 100), n_X = 3, n_W = 0, rho = rho, verbose = FALSE)
  df_stats <- anova(df, FALSE)
  expect_equivalent(
    unlist(df_stats$population_estimates)[c(3, 7, 11)], rho, tol = .1
  )
})
test_that("Rho works for dataframes with three or more levels", {
  set.seed(9621)
  df <- cluster_gen(c(5, 4, 50), rho = .7, verbose = FALSE)
  df_stats <- anova(df, FALSE)
  expect_equivalent(mean(unlist(df_stats$school$population_estimates)[c(3, 7)]),
                    .52,
                    tol = .1)
  expect_equivalent(unlist(df_stats$class$population_estimates)[3],
                    .68,
                    tol = .1)
  set.seed(6485)
  df2 <- cluster_gen(c(10, 20, 50), rho = c(.7, .2), verbose = FALSE)
  df2_stats <- anova(df2, FALSE)
  expect_equivalent(unlist(df2_stats$school$population_estimates)[c(3, 7)],
                    c(.7, .2),
                    tol = .1)
  expect_equivalent(unlist(df2_stats$class$population_estimates)[c(3, 7)],
                    c(.7, .2),
                    tol = .1)
  set.seed(893961)
  df3 <- cluster_gen(c(25, 15, 50), rho = list(.4, .9), verbose = FALSE)
  df3_stats <- anova(df3, FALSE)
  expect_equivalent(unlist(df3_stats$school$population_estimates)[c(3, 7)],
                    c(.4, .4),
                    tol = .1)
  expect_equivalent(unlist(df3_stats$class$population_estimates)[c(3, 7)],
                    c(.9, .9),
                    tol = .1)
  # set.seed(977753)
  set.seed(9775)
  df4 <- cluster_gen(
    n = c(10, 20, 50),
    rho = list(.4, c(.1, .2, .3)),
    n_X = list(2, 3),
    verbose = FALSE
  )
  df4_stats <- anova(df4, FALSE)
  expect_equivalent(unlist(df4_stats$school$population_estimates)[c(3, 7)],
                    c(.4, .4),
                    tol = .2)
  expect_equivalent(unlist(df4_stats$class$population_estimates)[c(3, 7, 11)],
                    c(.1, .2, .3),
                    tol = .1)
})

test_that("c_mean and rho work together", {
  rho <- .3
  df <- cluster_gen(n = c(20, 200), n_X = 1, n_W = 0,
                    rho = rho, c_mean = 5, verbose = FALSE)
  expect_equivalent(summary(df, print = "none")$school$y_bar, 5, .1)
  expect_equivalent(
    anova(df, print = FALSE)$population_estimates$q1[3], rho, .1
  )
  rho <- .5
  df <- cluster_gen(n = c(50, 500), n_X = 1, n_W = 0,
                    rho = rho,
                    c_mean = list(as.list(seq(1, 4, length.out = 50))),
                    verbose = FALSE)
  expect_equivalent(
    anova(df, FALSE)$population_estimates$q1[3], rho, .1
  )
})

test_that("Rho behaves properly with sigma and c_mean", {
  expect_error(
    cluster_gen(
      n = c(40, 100), n_X = 1, n_W = 0,
      sigma = list(as.list(1:40)),
      rho = .5,
      verbose = FALSE
    )
  )
  set.seed(9624063)
  df2 <- cluster_gen(
    n = c(5, 40, 100), n_X = 1, n_W = 0,
    sigma = list(5, 10),
    verbose = FALSE
  )
  df3 <- cluster_gen(
    n = c(5, 40, 100), n_X = 1, n_W = 0,
    c_mean = list(1, 7),
    sigma = list(3, 9),
    verbose = FALSE
  )
  expect_equivalent(
    anova(df2, print=FALSE)$school$sample_statistics[1], 25, 1
  )
  expect_equivalent(
    anova(df2, print=FALSE)$class$sample_statistics[1], 100, 1
  )
  expect_equivalent(
    anova(df3, print=FALSE)$school$sample_statistics[1], 9, 1
  )
  expect_equivalent(
    anova(df3, print=FALSE)$class$sample_statistics[1], 64, 1
  )
  anova(df3, print=FALSE)
})

test_that("Rho is retrieved when c_mean is provided and sigma2 is missing", {
  n_classes <- 150
  c_mean_narrow <- list(as.list(seq(10, 12, length = n_classes)))
  c_mean_wide <- list(as.list(seq(10, 50, length = n_classes)))
  df_narrow <- cluster_gen(
    n = c(n_classes, 50), n_X = 1, n_W = 0, rho = .2, verbose = FALSE,
    c_mean = c_mean_narrow
  )
  df_wide <- cluster_gen(
    n = c(n_classes, 50), n_X = 1, n_W = 0, rho = .2, verbose = FALSE,
    c_mean = c_mean_wide
  )
  df_narrow_tog <- cluster_gen(
    n = c(n_classes, 50), n_X = 1, n_W = 0, rho = .2, verbose = FALSE,
    separate_questionnaires = FALSE,
    c_mean = unlist(c_mean_narrow)
  )
  df_wide_tog <- cluster_gen(
    n = c(n_classes, 50), n_X = 1, n_W = 0, rho = .2, verbose = FALSE,
    separate_questionnaires = FALSE,
    c_mean = unlist(c_mean_wide)
  )
  expect_equivalent(
    object = anova(df_narrow, print = FALSE)$population$q1["rho_hat.q1"],
    expected = .2,
    tol = .1
  )
  expect_equivalent(
    object = anova(df_wide, print = FALSE)$population$q1["rho_hat.q1"],
    expected = .2,
    tol = .1
  )
  expect_equivalent(
    object = anova(df_narrow_tog, print = FALSE)$pop$q1["rho_hat.q1"],
    expected = .2,
    tol = .1
  )
  expect_equivalent(
    object = anova(df_wide, print = FALSE)$pop$q1["rho_hat.q1"],
    expected = .2,
    tol = .1
  )
})

test_that("Rho works for together questionnaires", {
  set.seed(278074)
  df1 <- cluster_gen(c(50, 20), rho = .8, n_X = 1, verbose = FALSE,
                    separate_questionnaires = FALSE)
  df2 <- cluster_gen(
    n = c(5, 40, 100), n_X = 2, n_W = 0,
    c_mean = c(1, 7),
    rho = c(.2, .8),
    verbose = FALSE,
    separate_questionnaires = FALSE
  )
  df3 <- cluster_gen(
    n = c(5, 40, 100), n_X = 2, n_W = 0,
    sigma = c(5, 10),
    rho = .5,
    verbose = FALSE,
    separate_questionnaires = FALSE
  )
  df4 <- cluster_gen(
    n = c(5, 40, 100), n_X = 2, n_W = 0,
    sigma = c(3, 9),
    rho = c(.2, .8),
    verbose = FALSE,
    separate_questionnaires = FALSE
  )
  expect_equivalent(anova(df1, print=F)$pop$q1["rho_hat.q1"], .8, .1)
  expect_equivalent(anova(df2, print=F)$pop$q1["rho_hat.q1"], .2, .1)
  expect_equivalent(anova(df2, print=F)$pop$q2["rho_hat.q2"], .8, .1)
  expect_equivalent(
    summary(df2, print="none")$class$y_bar, c(1, 7), .1
  )
  expect_equivalent(anova(df3, print=F)$pop$q1["rho_hat.q1"], .5, .1)
  expect_equivalent(anova(df3, print=F)$pop$q2["rho_hat.q2"], .5, .1)
  expect_equivalent(anova(df3, print=F)$pop$q1["sigma2_hat.q1"], 25, .1)
  expect_equivalent(anova(df3, print=F)$pop$q2["sigma2_hat.q2"], 100, 1)
  expect_equivalent(anova(df4, print=F)$pop$q1["rho_hat.q1"], .2, .1)
  expect_equivalent(anova(df4, print=F)$pop$q2["rho_hat.q2"], .8, .1)
  expect_equivalent(anova(df4, print=F)$pop$q1["sigma2_hat.q1"], 9, .1)
  expect_equivalent(anova(df4, print=F)$pop$q2["sigma2_hat.q2"], 81, .1)
})

# Adding cor_matrix and cat_prop to cluster_gen ===============================
context("Passing cor_matrix and cat_prop from cluster_gen to questionnaire_gen")
cl_gen_cor <- function(n, mx, nX = 0, nW = 0, sep = TRUE) {
  cluster_gen(
    n, n_X = nX, n_W = nW, cor_matrix = mx,
    verbose = FALSE, separate_questionnaires = sep
  )
}
test_that("Correlation matrix is correctly parsed in 2-level structures", {
  ## Setting up datasets ------------------------------------------------------
  cor_mx <- matrix(c(1, .8, .8, 1), 2)
  set.seed(33602732)
  dfXX <- cl_gen_cor(c(4, 100), cor_mx, 2)
  dfXW <- cl_gen_cor(c(4, 100), cor_mx, 1, 1)
  dfWW <- cl_gen_cor(c(4, 100), cor_mx, 0, 2)
  dfXXt <- cl_gen_cor(c(4, 100), cor_mx, 2, 0, FALSE)

  ## Testing output -----------------------------------------------------------
  expect_equivalent(
    object    = lapply(dfXX$school, function(x) cor(x[, c("q1", "q2")])),
    expected  = replicate(4, list(cor_mx)),
    tolerance = .1
  )
  expect_equivalent(
    object    = with(dfXW$school[[1]], polycor::polychor(q1, q2)),
    expected  = cor_mx[1, 2],
    tolerance = .1
  )
  expect_equivalent(
    object    = with(dfXW$school[[3]], polycor::polychor(q1, q2)),
    expected  = cor_mx[1, 2],
    tolerance = .1
  )
  expect_equivalent(
    object    = with(dfWW$school[[1]], polycor::polychor(q1, q2)),
    expected  = cor_mx[1, 2],
    tolerance = .1
  )
  expect_equivalent(
    object    = lapply(dfXXt, function(x) cor(x[, c("q1", "q2")])),
    expected  = replicate(4, list(cor_mx)),
    tolerance = .1
  )
})
test_that("Correlation matrix works for structures with 3+ levels", {
  ## Generating data ----------------------------------------------------------
  cor_mx_schools <- matrix(c(1, .6, .3, .6, 1, -.4, .3, -.4, 1), 3)
  cor_mx_classes <- matrix(c(1, -.9, -.9, 1), 2)
  cor_mx <- list(school = cor_mx_schools, class = cor_mx_classes)
  set.seed(6400492)
  df3 <- cl_gen_cor(c(2, 200, 100), cor_mx, list(3, 2))

  ## Testing output -----------------------------------------------------------
  expect_equivalent(
    object    = lapply(df3$school, function(x) cor(x[, c("q1", "q2", "q3")])),
    expected  = list(cor_mx$school, cor_mx$school),
    tolerance = .1
  )
  expect_equivalent(
    object    = lapply(df3$class, function(x) cor(x[, c("q1", "q2")])),
    expected  = replicate(400, list(cor_mx$class)),
    tolerance = .1
  )
})
test_that("cor_matrix is customizable between elements at the same level", {
  ## Generating data ----------------------------------------------------------
  cor_mx_school1 <- matrix(c(1, -.9, -.9, 1), 2)
  cor_mx_school2 <- matrix(c(1, .4, .4, 1), 2)
  cor_mx <- list(school = list(cor_mx_school1, cor_mx_school2))
  df_sep <- cl_gen_cor(c(2, 200), cor_mx, 2)
  df_tog <- cl_gen_cor(c(2, 200), cor_mx, 2, 0, FALSE)

  ## Testing output -----------------------------------------------------------
  expect_equivalent(
    object = lapply(df_sep$school, function(x) cor(x[, c("q1", "q2")])),
    expected = cor_mx$school,
    tolerance = .5
  )
  expect_equivalent(
    object = lapply(df_tog, function(x) cor(x[, c("q1", "q2")])),
    expected = cor_mx$school,
    tolerance = .5
  )
})

# Passing cat_prop ============================================================
context("Passing cat_prop to cluster_gen")
wrap_cluster_gen_cat <- function(n, cat, sep = TRUE) {
  cluster_gen(n, cat_prop = cat, separate_questionnaires = sep, verbose = FALSE)
}
test_that("cat_prop is parsed correctly: unique props for all structures", {
  set.seed(288497)
  propX <- list(1)
  propW <- list(c(.5, .8, 1))
  propXW <- list(1, c(.3, 1))
  propXX <- list(1, 1)
  propWW <- list(c(.7, 1), c(.2, .3, .7, .9, 1))
  propXXWW <- list(1, 1, c(.5, 1), c(.25, .75, 1))
  nXW <- c(1, 100)
  dfXsep <- wrap_cluster_gen_cat(nXW, propX)
  dfWsep <- wrap_cluster_gen_cat(nXW, propW)
  dfXWsep <- wrap_cluster_gen_cat(nXW, propXW)
  dfXXsep <- wrap_cluster_gen_cat(nXW, propXX)
  dfWWsep <- wrap_cluster_gen_cat(nXW, propWW)
  dfXXWWsep <- wrap_cluster_gen_cat(nXW, propXXWW)
  dfXtog <- wrap_cluster_gen_cat(nXW, propX, FALSE)
  dfWtog <- wrap_cluster_gen_cat(nXW, propW, FALSE)
  dfXWtog <- wrap_cluster_gen_cat(nXW, propXW, FALSE)
  dfXXtog <- wrap_cluster_gen_cat(nXW, propXX, FALSE)
  dfWWtog <- wrap_cluster_gen_cat(nXW, propWW, FALSE)
  dfXXWWtog <- wrap_cluster_gen_cat(nXW, propXXWW, FALSE)
  expect_equivalent(
    object = sapply(dfXsep$school[[1]], class),
    expected = c("integer", "numeric", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfXtog$school1, class),
    expected = c("integer", "numeric", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfWsep$school[[1]], class),
    expected = c("integer", "factor", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfWtog$school1, class),
    expected = c("integer", "factor", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfXWsep$school[[1]], class),
    expected = c("integer", "numeric", "factor", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfXWtog$school1, class),
    expected = c("integer", "numeric", "factor", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfWWsep$school[[1]], class),
    expected = c("integer", "factor", "factor", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfWWtog$school1, class),
    expected = c("integer", "factor", "factor", rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfXXWWsep$school[[1]], class),
    expected = c("integer", "numeric", "numeric", "factor", "factor",
      rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = sapply(dfXXWWtog$school1, class),
    expected = c("integer", "numeric", "numeric", "factor", "factor",
      rep("numeric", 3), "character")
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfWsep$school[[1]]["q1"]))),
    expected = unlist(propW),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXWsep$school[[1]]["q2"]))),
    expected = unlist(propXW[[2]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfWWsep$school[[1]]["q1"]))),
    expected = unlist(propWW[[1]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfWWsep$school[[1]]["q2"]))),
    expected = unlist(propWW[[2]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWsep$school[[1]]["q3"]))),
    expected = unlist(propXXWW[[3]]),
    tol = .1
  )
    expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWsep$school[[1]]["q4"]))),
    expected = unlist(propXXWW[[4]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfWtog$school1["q1"]))),
    expected = unlist(propW),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXWtog$school1["q2"]))),
    expected = unlist(propXW[[2]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfWWtog$school1["q1"]))),
    expected = unlist(propWW[[1]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfWWtog$school1["q2"]))),
    expected = unlist(propWW[[2]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWtog$school1["q3"]))),
    expected = unlist(propXXWW[[3]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWtog$school1["q4"]))),
    expected = unlist(propXXWW[[4]]),
    tol = .1
  )
})
test_that("cat_prop is parsed correctly: individual props for each level", {
  set.seed(879752)
  nXW <- c(1, 30, 100)
  propXXWW <- list(list(1, 1, c(.5, 1)), list(1, c(.25, .75, 1), c(.9, 1)))
  dfXXWWsep <- wrap_cluster_gen_cat(nXW, propXXWW)
  dfXXWWtog <- wrap_cluster_gen_cat(nXW, propXXWW, FALSE)
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWsep$school[[1]]["q3"]))),
    expected = unlist(propXXWW[[1]][[3]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWsep$class[[1]]["q2"]))),
    expected = unlist(propXXWW[[2]][[2]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWsep$class[[1]]["q3"]))),
    expected = unlist(propXXWW[[2]][[3]]),
    tol = .1
  )
})
test_that("cat_prop is parsed correctly: individual props within a level", {
  set.seed(87975)
  nXW <- c(2, 100)
  propXXWW <- list(list(list(c(.9, 1)), list(c(.1, 1))))
  dfXXWWsep <- wrap_cluster_gen_cat(nXW, propXXWW)
  dfXXWWtog <- wrap_cluster_gen_cat(nXW, propXXWW, FALSE)
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWsep$school[[1]]["q1"]))),
    expected = unlist(propXXWW[[1]][[1]][[1]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWsep$school[[2]]["q1"]))),
    expected = unlist(propXXWW[[1]][[2]][[1]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWtog$school1["q1"]))),
    expected = unlist(propXXWW[[1]][[1]][[1]]),
    tol = .1
  )
  expect_equivalent(
    object = cumsum(prop.table(table(dfXXWWtog$school2["q1"]))),
    expected = unlist(propXXWW[[1]][[2]][[1]]),
    tol = .1
  )
})

# n_W as list of lists =========================================================
context("Complex n_W")
test_that("Special cases of n_W work as expected", {
  n3 <- c(school = 3, class = 2, student = 5)
  cluster_gen_2 <- function(...) {
      cluster_gen(
        ..., verbose = FALSE, calc_weights = FALSE, family = "gaussian"
      )
  }
  df3 <- cluster_gen_2(n3, n_X = 0, n_W = list(list(3, 4, 4), list(2, 5)))
  for (i in seq_len(3)) {
    expect_equivalent(
      object = sapply(df3$school[[i]][, 2:4], function(x) max(levels(x))),
      expected = c("3", "4", "4")
    )
  }
  for (i in seq_len(6)) {
    expect_equivalent(
      object = sapply(df3$class[[i]][, 2:3], function(x) max(levels(x))),
      expected = c("2", "5")
    )
  }
})