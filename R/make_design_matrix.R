make_design_matrix <- function(basis_set, X) {
  basis_matrix <- sapply(basis_set, function(basis) {
    enumerate_zero_order_basis(basis, X)
  })

  return(basis_matrix)
}
