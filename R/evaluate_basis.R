#' Evaluate a given zero-order HAL basis function
#'
#' @param basis A \code{Basis} object
#' @param X Data matrix
#'
#' @returns An integer vector of evaluated basis
evaluate_zero_order_basis <- function(basis, X) {
  X_sub <- X[, basis$col_indices, drop = FALSE]

  # repeat the knot_points to match the dimensions of X_sub
  knot_points_repeated <- matrix(rep(basis$knot_points, each = nrow(X_sub)),
                                 ncol = length(basis$knot_points),
                                 byrow = FALSE)

  # vectorized comparisons
  comparisons <- X_sub >= knot_points_repeated
  result <- rowSums(comparisons) == ncol(X_sub)

  return(as.integer(result))
}

