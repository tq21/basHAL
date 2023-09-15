

n <- 1000
X_train <- as.matrix(rnorm(n, 1.5, 1))
y_train <- as.numeric(X_train >= 1.5)
X_test <- as.matrix(rnorm(10000, 1.5, 1))
y_test <- as.numeric(X_test >= 1.5)

# initialize basHAL object
basHAL_obj <- basHAL$new(X = X_train,
                         y = y_train,
                         method = "univariate glm",
                         len_final_basis_set = nrow(X_train),
                         max_rows = nrow(X_train),
                         max_degree = 1,
                         batch_size = 1,
                         n_batch = 1,
                         p = 0.5,
                         seed = 29857,
                         n_cores = 1,
                         cv_loss = TRUE)

# run basHAL
basHAL_obj$run(verbose = TRUE, plot = FALSE)
test_pred <- basHAL_obj$predict(newx = X_test, type = "response")
basHAL_rmse <- sqrt(mean((y_test - test_pred)^2))
print(paste("basHAL RMSE: ", basHAL_rmse))
