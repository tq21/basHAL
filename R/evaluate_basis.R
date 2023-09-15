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

sigmoid <- function(x, delta) {
  1 / (1 + exp(-delta * x))
}

evaluate_zero_order_basis_smoothed <- function(basis, X, delta = 20) {
  X_sub <- X[, basis$col_indices, drop = FALSE]

  # subtract the knot_points to center the transformation around the knots
  X_transformed <- X_sub - matrix(rep(basis$knot_points, each = nrow(X_sub)),
                                  ncol = length(basis$knot_points),
                                  byrow = FALSE)

  # Apply sigmoid transformation
  sigmoid_values <- sigmoid(X_transformed, delta)

  # Define a custom function for your mean computation
  compute_mean <- function(row_values) {
    if (all(row_values >= 0.5)) {
      return(mean(row_values))
    } else {
      return(mean(row_values[row_values < 0.5]))
    }
  }

  # If basis is of higher degree, use the custom function for mean computation
  result <- if (ncol(X_sub) > 1) {
    apply(sigmoid_values, 1, compute_mean)
  } else {
    as.vector(sigmoid_values)
  }

  return(as.numeric(result))
}
