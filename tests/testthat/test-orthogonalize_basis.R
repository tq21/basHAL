
test_that("working", {
  basis_1 <- c(1, 0, 1, 1, 0)
  basis_2 <- c(0, 1, 1, 1, 0)
  dot_prod <- dot(basis_1, basis_1)
  orthos <- vector(mode = "list", length = 10)
  orthos[[1]] <- basis_1
  res_1 <- orthogonalize_basis(basis_2, dot_prod, orthos)

  basis_3 <- c(1, 1, 0, 0, 0)
  dot_prod[[2]] <- dot(res_1, res_1)
  orthos[[2]] <- res_1
  res_2 <- orthogonalize_basis(basis_3, dot_prod, orthos)

  expect_equal(res_1, c(-2/3, 1, 1/3, 1/3, 0))
  expect_equal(res_2, c(4/5, 4/5, -2/5, -2/5, 0))
})
