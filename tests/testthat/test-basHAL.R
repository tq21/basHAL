library(testthat)
library(tidyverse)

# load data
data <- mtcars

# get X and y
y_col_idx <- 1
x_col_idx <- setdiff(seq(1, ncol(data)), y_col_idx)
X <- as.matrix(data[, x_col_idx])
y <- as.matrix(data[, y_col_idx])

set.seed(9847)

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
                         max_degree = 10,
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

print(paste("Number of non zero coefficients in the final lasso: ", sum(coef(final_lasso) != 0)))

# generate basis matrix for test data
basis_matrix_test <- make_design_matrix(final_basis_set, X_test)
test_pred <- predict(final_lasso, newx = basis_matrix_test)
s_hal_rmse <- sqrt(mean((y_test - test_pred)^2))
print(paste("basHAL RMSE: ", s_hal_rmse))

# glm comparison ---------------------------------------------------------------
glm_fit <- lm(mpg ~ ., data = data[-indices,])
glm_pred <- predict(glm_fit, newdata = data[indices,])
glm_rmse <- sqrt(mean((y_test - glm_pred)^2))
print(paste("glm RMSE: ", glm_rmse))

# xgboost comparison -----------------------------------------------------------
library(xgboost)

dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
bst <- xgb.train(data = dtrain, nrounds = 1000)
xgb_pred <- predict(bst, dtest)
xgb_rmse <- sqrt(mean((y_test - xgb_pred)^2))
print(paste("xgboost RMSE: ", xgb_rmse))

# hal9001 comparison -----------------------------------------------------------
library(hal9001)
hal9001_fit <- fit_hal(X = X_train,
                       Y = y_train,
                       family = "gaussian",
                       max_degree = 5,
                       smoothness_orders = 0,
                       screen_variables = FALSE)
hal9001_pred <- predict(hal9001_fit, new_data = X_test, type = "response")
hal9001_rmse <- sqrt(mean((y_test - hal9001_pred)^2))
print(paste("hal9001 RMSE: ", hal9001_rmse))
