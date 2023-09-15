devtools::load_all()

data <- read.csv("data/small_data/cpu.csv")
y_col_idx <- 1
x_col_idx <- setdiff(seq(1, ncol(data)), y_col_idx)
X <- as.matrix(data[, x_col_idx])
y <- as.matrix(data[, y_col_idx])

set.seed(12345)
indices <- sample(seq_len(nrow(X)), size = 0.2 * nrow(X))
X_test <- X[indices,]
y_test <- y[indices]
X_train <- X[-indices,]
y_train <- y[-indices]

# lasso only
lasso_fit <- cv.glmnet(x = X_train, y = y_train, nfolds = 5)
lasso_pred <- as.numeric(predict(lasso_fit, newx = X_test, s = "lambda.min"))
sqrt(mean((y_test - lasso_pred)^2))

# hal only
hal_fit <- fit_hal(X = X_train, Y = y_train, smoothness_orders = 0)
hal_pred <- as.numeric(predict(hal_fit, new_data = X_test))
sqrt(mean((y_test - hal_pred)^2))

# additive
additive_obj <- additive(X_train, y_train)
additive_pred <- predict_additive(additive_obj, X_test)
sqrt(mean((y_test - additive_pred)^2))

# basHAL
basHAL_obj <- basHAL$new(X = X_train,
                         y = y_train,
                         method = "univariate glm",
                         len_final_basis_set = nrow(X_train),
                         max_rows = nrow(X_train),
                         max_degree = 6,
                         batch_size = 100,
                         n_batch = 100,
                         p = 0.5,
                         seed = 29857,
                         n_cores = 5,
                         cv_loss = TRUE)

# run basHAL
basHAL_obj$run(verbose = TRUE, plot = FALSE)
basHAL_pred <- basHAL_obj$predict(newx = X_test, type = "response")
sqrt(mean((y_test - basHAL_pred)^2))


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

# benchmark against xgboost
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)

# train xgboost model
xgb_fit <- xgboost(data = dtrain, nrounds = 1000, verbose = 0)
test_pred_xgb <- predict(xgb_fit, dtest)
xgb_rmse <- sqrt(mean((y_test - test_pred_xgb)^2))
print(paste("xgboost RMSE: ", xgb_rmse))

hal_design <- make_design_matrix(basHAL_obj$selected_basis_set, X_train)
kappa(hal_design)

eigen(t(hal_design) %*% hal_design)







library(xgboost)
library(Metrics)  # for RMSE calculation

# Assuming fpath and seed are predefined
dt <- read.csv("data/small_data/laheart.csv")
dt <- na.omit(dt)

set.seed(12941)

# train-test split
train_indices <- sample(x = 1:nrow(dt), size = floor(0.8 * nrow(dt)))
dt_train <- dt[train_indices, ]
dt_test <- dt[-train_indices, ]

# separate the target variable
train_label <- dt_train[, ncol(dt_train)]
train_data <- dt_train[, -ncol(dt_train)]
test_label <- dt_test[, ncol(dt_train)]
test_data <- dt_test[, -ncol(dt_train)]

# convert the data to matrix, as xgboost only accepts matrix or dgCMatrix as input
dtrain <- xgb.DMatrix(data = as.matrix(train_data), label = train_label)
dtest <- xgb.DMatrix(data = as.matrix(test_data), label = test_label)

# xgboost parameters
param <- list(max_depth = 10, eta = 1, silent = 1, objective = 'reg:squarederror')

# train xgboost model
bst <- xgb.train(params = param, data = dtrain, nrounds = 50)

# predict on test data
pred <- predict(bst, dtest)

# calculate RMSE
rmse <- rmse(test_label, pred)
print(paste("Test RMSE: ", rmse))



family <- "binomial"
y_col_idx <- 1
x_col_idx <- setdiff(1:ncol(dt_train), y_col_idx)
hal9001_fit <- fit_hal(X = dt_train[, x_col_idx],
                       Y = dt_train[, y_col_idx],
                       family = family,
                       max_degree = 9,
                       smoothness_orders = 0)

# get loss
hal9001_pred <- predict(hal9001_fit,
                        new_data = dt_test[, x_col_idx], type = "response")
hal9001_true <- dt_test[, y_col_idx]
hal9001_loss <- ifelse(family == "gaussian",
                       sqrt(mean((hal9001_pred - hal9001_true)^2)),
                       -mean(hal9001_true * log(hal9001_pred) + (1 - hal9001_true) * log(1 - hal9001_pred)))

# get number of non zero coefficients
hal9001_num_non_zero <- length(summary(hal9001_fit)$table$coef) - 1 # exclude intercept



sHAL_obj <- sHAL$new(X = dt_train[, x_col_idx],
                     y = dt_train[, y_col_idx],
                     len_candidate_basis_set = 100,
                     len_final_basis_set = 100,
                     max_rows = 392,
                     max_degree = 7,
                     batch_size = 50,
                     n_batch = 10,
                     p = 0.5,
                     seed = 14123,
                     weight_function = "double weight v3",
                     family = "binomial",
                     n_cores = 1)
sHAL_res <- sHAL_obj$run(verbose = TRUE, plot = FALSE)
sHAL_lasso <- sHAL_res[[1]]
sHAL_basis_set <- sHAL_res[[2]]

# get rmse
basis_matrix_test <- make_design_matrix(sHAL_basis_set, dt_test[, x_col_idx])
sHAL_pred <- predict(sHAL_lasso, newx = basis_matrix_test, type = "response", s = "lambda.min")
sHAL_true <- dt_test[, y_col_idx]
sHAL_loss <- ifelse(family == "gaussian",
                    sqrt(mean((sHAL_pred - sHAL_true)^2)),
                    -mean(sHAL_true * log(sHAL_pred) + (1 - sHAL_true) * log(1 - sHAL_pred)))

# get number of non zero coefficients
sHAL_num_non_zero <- sum(coef(sHAL_lasso) != 0)




library(microbenchmark)

# Create a Basis object and data matrix for benchmarking
basis <- Basis$new(col_indices = c(1, 2), knot_points = c(0.3, 0.5))
X <- matrix(runif(2000), ncol=5, nrow=400)

# Define the two functions you want to benchmark
evaluate_zero_order_basis_1 <- function(basis, X) {
  if (nrow(X) == 1) {
    result <- all(X[, basis$col_indices] >= basis$knot_points)
  } else {
    X_sub <- as.matrix(X[, basis$col_indices])
    result <- apply(X_sub, 1, function(row) all(row >= basis$knot_points))
  }

  return(as.integer(result))
}

evaluate_zero_order_basis_2 <- function(basis, X) {
  X_sub <- as.matrix(X[, basis$col_indices])
  knot_points_repeated <- matrix(rep(basis$knot_points, each = nrow(X_sub)),
                                 ncol = length(basis$knot_points),
                                 byrow = TRUE)
  comparisons <- X_sub >= knot_points_repeated
  result <- rowSums(comparisons) == ncol(X_sub)
  return(as.integer(result))
}

# Benchmark the two functions
benchmark_results <- microbenchmark(
  function_1 = evaluate_zero_order_basis_1(basis, X),
  function_2 = evaluate_zero_order_basis_2(basis, X),
  times = 1000
)

# Print the benchmark results
print(benchmark_results)


library(microbenchmark)

# Create two example vectors
a <- runif(1e6)
b <- runif(1e6)

# Define two functions for computing the dot product
dot_product_1 <- function(a, b) sum(a * b)
dot_product_2 <- function(a, b) as.numeric(a %*% b)

# Benchmark the two functions
benchmark_results <- microbenchmark(
  function_1 = dot_product_1(a, b),
  function_2 = dot_product_2(a, b),
  times = 1000
)

# Print the benchmark results
print(benchmark_results)





library(microbenchmark)

# Original function
ortho_basis_original <- function(basis, cur_bases, dot_prods, idx) {
  tmp <- 0
  for (j in 1:idx) {
    tmp <- tmp+as.numeric(basis%*%cur_bases[[j]])/dot_prods[j]*cur_bases[[j]]
  }

  return(basis - tmp)
}

# Optimized function
ortho_basis_updated <- function(basis, cur_bases, dot_prods, idx) {
  tmp <- numeric(length(basis))
  for (j in 1:idx) {
    tmp <- tmp + as.numeric(basis %*% cur_bases[[j]]) / dot_prods[j] * cur_bases[[j]]
  }
  return(basis - tmp)
}

# Generate test data
n <- 1000
basis <- runif(n)
cur_bases <- replicate(10, runif(n), simplify = FALSE)
dot_prods <- runif(10)
idx <- 10

# Benchmark the two functions
benchmark_results <- microbenchmark(
  original = ortho_basis_original(basis, cur_bases, dot_prods, idx),
  optimized = ortho_basis_updated(basis, cur_bases, dot_prods, idx),
  times = 1000
)

# Print the benchmark results
print(benchmark_results)
