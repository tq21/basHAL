#' Make design matrix from basis set
#' 
#' @export
#'
#' @param basis_set A list \code{Basis} objects.
#' @param X A vector of values to evaluate the basis functions at.
#'
#' @return A design matrix.
make_design_matrix <- function(basis_set, X) {
  basis_matrix <- sapply(basis_set, function(basis) {
    enumerate_zero_order_basis(basis, X)
  })

  return(basis_matrix)
}
