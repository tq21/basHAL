library(testthat)

test_that("enumerate_zero_order_basis work", {
  data <- data.frame(X1 = c(10, 20, 30),
                     X2 = c(30, 10, 20))

  basis_1 <- Basis$new(1, 10)
  basis_2 <- Basis$new(1, 20)
  basis_3 <- Basis$new(1, 30)
  basis_4 <- Basis$new(c(1, 2), c(10, 20))
  basis_set <- list(basis_1, basis_2, basis_3, basis_4)

  true_mat <- matrix(c(1, 1, 1,
                       0, 1, 1,
                       0, 0, 1,
                       1, 0, 1), nrow = 3, ncol = 4)
  mat <- make_design_matrix(basis_set, data)
  expect_equal(true_mat, mat)
})
