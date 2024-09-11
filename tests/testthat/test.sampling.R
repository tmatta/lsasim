context("Sampling alternatives on cluster_gen")

cluster_gen_wrap <- function(samp, clab = NULL, rlab = NULL, seed = 9674731) {
  set.seed(seed)
  enn <- list(3, rep(20, 3))
  ENN <- list(6, seq(30, 80, 10))
  cluster_gen(
    enn, ENN, cluster_labels = clab, resp_labels = rlab,
    calc_weights = TRUE, sampling_method = samp, verbose = FALSE
  )
}

data_default <- cluster_gen_wrap("mixed")
data_sch_stu <- cluster_gen_wrap("mixed", "school", "student")
data_sch_stu_pps <- cluster_gen_wrap("PPS", "school", "student")
data_sch_stu_srs <- cluster_gen_wrap("SRS", "school", "student")
data_skl_elv_mix <- cluster_gen_wrap("mixed", "skole", "elev")
data_skl_elv_srs <- cluster_gen_wrap("SRS", "skole", "elev")
data_skl_elv_pps <- cluster_gen_wrap("PPS", "skole", "elev")

extract_weights <- function(data) {
  w_cols <- c("school.weight", "within.school.weight", "final.student.weight")
  out <- list(
    vapply(data[["school"]], function(x) x[[w_cols[1]]], numeric(20)),
    vapply(data[["school"]], function(x) x[[w_cols[2]]], numeric(20)),
    vapply(data[["school"]], function(x) x[[w_cols[3]]], numeric(20))
  )
  out
}

test_that("Sampling weights are the expected", {
  expect_equal(extract_weights(data_default), extract_weights(data_sch_stu))
  expect_equal(extract_weights(data_default), extract_weights(data_sch_stu_pps))
  expect_false(
    identical(extract_weights(data_default), extract_weights(data_sch_stu_srs))
  )
  expect_equal(
    extract_weights(data_skl_elv_mix), extract_weights(data_skl_elv_srs)
  )
  expect_false(
    identical(extract_weights(data_skl_elv_mix), extract_weights(data_sch_stu_pps))
  )
})
