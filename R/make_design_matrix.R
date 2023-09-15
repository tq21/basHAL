#' Make design matrix from basis set
#'
#' @export
#'
#' @param basis_set A list \code{Basis} objects.
#' @param X A vector of values to evaluate the basis functions at.
#'
#' @return A design matrix.
make_design_matrix <- function(basis_set, X, dot_prods=NULL, orthos=NULL, smooth=TRUE) {
  if (is.null(dot_prods)) {
    return(sapply(basis_set, function(basis) {
      if (smooth) {
        return(evaluate_zero_order_basis_smoothed(basis, X))
      } else {
        return(evaluate_zero_order_basis(basis, X))
      }
    }))
  } else {
    return(sapply(basis_set, function(basis) {
      orthogonalize_basis(evaluate_zero_order_basis(basis, X), dot_prods, orthos)
    }))
  }
}
