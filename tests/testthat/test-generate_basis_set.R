test_that("basic functionalities", {
  n <- 3
  d <- 3
  max_degree <- 3
  n_sets <- 10

  X <- matrix(1:9, nrow = n, ncol = d, byrow = TRUE)
  basis_set <- generate_basis_set(n, d, max_degree, n_sets, X)

  # test the type of the returned object
  expect_type(basis_set, "list")
  expect_true(all(map_lgl(basis_set, ~ inherits(., "Basis"))))

  # test the length of returned object
  expect_equal(length(basis_set), n_sets)

  # test if the column indices have no duplicates
  expect_true(all(map_lgl(basis_set, function(basis) {
    length(unique(basis$col_indices)) == length(basis$col_indices)
  })))

  # test if all column indices are smaller than or equal to d
  expect_true(all(map_lgl(basis_set, function(basis) {
    all(basis$col_indices <= d)
  })))

  # test if the length of the column indices is smaller than or equal to max_degree
  expect_true(all(map_lgl(basis_set, function(basis) {
    length(basis$col_indices) <= max_degree
  })))

  # test if the sampled knot points are in the same row in the data
  expect_true(all(map_lgl(basis_set, function(basis) {
    knot_points <- basis$knot_points
    col_indices <- basis$col_indices

    if (length(col_indices) == 1) {
      rows <- which(X[, col_indices] == knot_points)
    } else {
      rows <- which(apply(X[, col_indices], 1,
                          function(row) all(row == knot_points)))
    }

    return(length(rows) == 1)
  })))
})
