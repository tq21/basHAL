
<!-- README.md is generated from README.Rmd. Please edit that file -->

# R/`basHAL`: Basis adaptive sampling (BAS) for highly adaptive lasso (HAL)

<!-- badges: start -->

[![License: GPL
v3](https://img.shields.io/badge/License-GPL%20v3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0)
<!-- badges: end -->

> A scalable implementation of highly adaptive lasso for
> high-dimensional data using a basis adaptive sampling algorithm

**Authors:** [Sky Qiu](https://github.com/tq21), [Yi
Li](https://github.com/yiberkeley), [Toru
Shirakawa](https://github.com/shirakawatoru)

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/tq21/basHAL/issues).

------------------------------------------------------------------------

## Example

``` r
library(basHAL)

# load data
data <- mtcars

# get X and y
y_col_idx <- 1
x_col_idx <- setdiff(seq(1, ncol(data)), y_col_idx)
X <- as.matrix(data[, x_col_idx])
y <- as.matrix(data[, y_col_idx])

set.seed(12345)

# train-test split
indices <- sample(seq_len(nrow(X)), size = 0.2 * nrow(X))
X_test <- X[indices,]
y_test <- y[indices]
X_train <- X[-indices,]
y_train <- y[-indices]

# initialize basHAL object
basHAL_obj <- basHAL$new(X = X_train,
                         y = y_train,
                         len_candidate_basis_set = nrow(X_train),
                         len_final_basis_set = nrow(X_train),
                         max_rows = nrow(X_train),
                         max_degree = 5,
                         batch_size = 50,
                         n_batch = 50,
                         p = 0.5,
                         seed = 29857,
                         weight_function = "glmnet",
                         top_k = TRUE,
                         n_cores = 5,
                         cv_loss = TRUE)

# run basHAL
result <- basHAL_obj$run(verbose = TRUE, plot = FALSE)
final_lasso <- result[[1]]
final_basis_set <- result[[2]]

# generate basis matrix for test data
basis_matrix_test <- make_design_matrix(final_basis_set, X_test)
test_pred <- predict(final_lasso, newx = basis_matrix_test)
basHAL_rmse <- sqrt(mean((y_test - test_pred)^2))
print(paste("basHAL RMSE: ", basHAL_rmse))
#> [1] "basHAL RMSE:  3.864894203462"
```

------------------------------------------------------------------------

## License

© 2023 [Sky Qiu](https://github.com/tq21), [Yi
Li](https://github.com/yiberkeley), [Toru
Shirakawa](https://github.com/shirakawatoru)

The contents of this repository are distributed under the GPL-3 license.
See file `LICENSE` for details.

------------------------------------------------------------------------
