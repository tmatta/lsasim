context("Random cluster sampling")

cl_scheme <- list(school = 2, class = c(3, 2), student = c(5, 4, 5, 5, 5))
cl_scheme2 <- list(
  country = 5,
  school  = c(20, 8, 5, 7, 3),
  student = c(rep(20, 20), rep(30, 8), rep(10, 5), rep(5, 7), rep(9, 3))
)
draw_cluster_structure(cl_scheme)
draw_cluster_structure(cl_scheme2)
set.seed(1);cluster_gen(n = select(1, 2, 4), N = cl_scheme2)

