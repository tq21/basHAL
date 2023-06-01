library(testthat)

test_that("enumerate_zero_order_basis work", {
  data <- data.frame(X1 = c(10, 20, 30),
                     X2 = c(30, 10, 20))

  # degree-one basis
  basis <- Basis$new(1, 20)
  true_vals <- c(0, 1, 1)
  vals <- enumerate_zero_order_basis(basis, data)
  expect_equal(true_vals, vals)

  # higher-degree basis
  basis <- Basis$new(c(1, 2), c(10, 20))
  true_vals <- c(1, 0, 1)
  vals <- enumerate_zero_order_basis(basis, data)
  expect_equal(true_vals, vals)
})
