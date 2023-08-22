#' Randomly draw without replacement to form a candidate basis set.
#'
#' @param n Number of rows in the data
#' @param d Number of columns in the data
#' @param max_degree Maximum degree of basis to sample
#' @param n_sets Number of candidate basis sets to generate
#' @param X Data matrix
#'
#' @returns A list of \code{Basis} objects.
generate_basis_set = function(n, d, max_degree, n_sets, X) {
  row_indices <- sample(x = n, size = n_sets, replace = TRUE)

  # get knot points
  col_indices_and_knot_points <- map(row_indices, function(row_idx) {
    col_indices <- sample(x = d, size = sample(max_degree, size = 1))
    return(list(col_indices = col_indices,
                knot_points = as.numeric(X[row_idx, col_indices])))
  })

  # make list of candidate basis sets
  basis_sets <- map(col_indices_and_knot_points, function(.x) {
    Basis$new(.x[[1]], .x[[2]])
  })

  return(basis_sets)
}
