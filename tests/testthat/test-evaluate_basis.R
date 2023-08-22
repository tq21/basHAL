test_that("basic functionalities of evaluate_zero_order_basis", {
  n <- 10
  d <- 5
  max_degree <- 3
  n_sets <- 4
  X <- matrix(runif(n*d), ncol=d, nrow=n)

  basis_set <- generate_basis_set(n, d, max_degree, n_sets, X)

  walk(basis_set, function(.x) {
    # test that the indicators are valid
    indicators <- evaluate_zero_order_basis(.x, X)
    expect_true(all(indicators %in% c(0, 1)))

    # test that the function works when X only has one row
    X_one_row <- matrix(runif(d), ncol=d, nrow=1)
    indicators_one_row <- evaluate_zero_order_basis(.x, X_one_row)
    expect_equal(length(indicators_one_row), 1)
    expect_true(indicators_one_row %in% c(0, 1))

    # test the correctness of the indicators
    expected_indicators <- map_int(1:nrow(X), function(.y) {
      all(X[.y, .x$col_indices] >= .x$knot_points)
    })
    expect_equal(indicators, expected_indicators)
  })
})
