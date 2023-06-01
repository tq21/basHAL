enumerate_zero_order_basis <- function(basis, X) {
  X_sub <- as.matrix(X[, basis$col_indices])
  result <- apply(X_sub, 1, function(row) all(row >= basis$knot_points))
  return(as.integer(result))
}
