
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
Shirakawa](https://github.com/shirakawatoru), [Mark van der
Laan](https://vanderlaan-lab.org/)

------------------------------------------------------------------------

## Issues

If you encounter any bugs or have any specific feature requests, please
[file an issue](https://github.com/tq21/basHAL/issues).

------------------------------------------------------------------------

## Example

``` r
library(basHAL)
library(xgboost)

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

# univariate glm screening method (non-adaptive) -------------------------------
# initialize basHAL object
basHAL_obj <- basHAL$new(X = X_train,
                         y = y_train,
                         method = "univariate glm",
                         len_final_basis_set = nrow(X_train),
                         max_rows = nrow(X_train),
                         max_degree = 5,
                         batch_size = 50,
                         n_batch = 50,
                         p = 0.5,
                         seed = 29857,
                         n_cores = 1,
                         cv_loss = TRUE)

# run basHAL
basHAL_obj$run(verbose = TRUE, plot = FALSE)
test_pred <- basHAL_obj$predict(newx = X_test, type = "response")
basHAL_rmse <- sqrt(mean((y_test - test_pred)^2))
print(paste("basHAL RMSE: ", basHAL_rmse))
#> [1] "basHAL RMSE:  3.06838073258906"

# benchmark against xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# train xgboost model
xgb_fit <- xgboost(data = dtrain, nrounds = 1000, verbose = 0)
test_pred_xgb <- predict(xgb_fit, dtest)
xgb_rmse <- sqrt(mean((y_test - test_pred_xgb)^2))
print(paste("xgboost RMSE: ", xgb_rmse))
#> [1] "xgboost RMSE:  3.65127456907944"
```

------------------------------------------------------------------------

## License

© 2023 [Sky Qiu](https://github.com/tq21), [Yi
Li](https://github.com/yiberkeley), [Toru
Shirakawa](https://github.com/shirakawatoru), [Mark van der
Laan](https://vanderlaan-lab.org/)

The contents of this repository are distributed under the GPL-3 license.
See file `LICENSE` for details.

------------------------------------------------------------------------
