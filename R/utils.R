# count the number of degree 1, 2, ..., g-basis functions in a basis set
count_basis <- function(basis_set) {
  degrees <- sapply(basis_set, function(basis) length(basis[["col_indices"]]))
  degrees <- as.factor(degrees)
  res <- as.data.frame(summary(degrees))
  res <- cbind(res, round(res[,1]/length(basis_set)*100, 2))
  colnames(res) <- c("number of basis functions", "percentage")

  return(res)
}

# basis_1 <- Basis$new(c(1,2), c(10,20))
# basis_2 <- Basis$new(c(1,4,6), c(30,20,10))
# basis_3 <- Basis$new(c(2,5,9,10), c(10,20,10,43))
#
# basis_set <- list(basis_1, basis_2, basis_3)
#
# count_basis(basis_set)

library(hal9001)
library(tidyverse)

`%+%` <- function(a, b) paste0(a, b)

run_benchmark <- function(fpath,
                          y_col_idx,
                          len_candidate_basis_set,
                          len_final_basis_set,
                          max_rows,
                          max_degree,
                          batch_size,
                          n_batch,
                          p,
                          seed,
                          weight_function,
                          family = "gaussian",
                          n_cores) {
  # load data
  dt <- read.csv(fpath)
  dt <- drop_na(dt)
  x_col_idx <- setdiff(seq_len(ncol(dt)), y_col_idx)

  # train-test split
  set.seed(seed)
  strata_ids <- NULL
  if (family == "binomial") {
    strata_ids <- dt[, y_col_idx]
  }
  folds <- make_folds(n = nrow(dt), V = 1,
                      fold_fun = folds_montecarlo,
                      strata_ids = strata_ids)
  train_indices <- folds[[1]]$training_set
  test_indices <- folds[[1]]$validation_set
  dt_train <- dt[train_indices, ]
  dt_test <- dt[test_indices, ]

  # run hal9001 --------------------------------------------------------------
  print("running hal9001...")
  hal9001_fit <- fit_hal(X = dt_train[, x_col_idx],
                         Y = dt_train[, y_col_idx],
                         family = family,
                         max_degree = 9,
                         smoothness_orders = 0)

  # get loss
  hal9001_pred <- predict(hal9001_fit,
                          new_data = dt_test[, x_col_idx], type = "response")
  hal9001_true <- dt_test[, y_col_idx]
  hal9001_loss <- get_loss(hal9001_pred, hal9001_true, family)

  # get number of non zero coefficients
  hal9001_num_non_zero <- length(summary(hal9001_fit)$table$coef) - 1 # exclude intercept

  # run sHAL -----------------------------------------------------------------
  print("running sHAL...")
  sHAL_obj <- sHAL$new(X = dt_train[, x_col_idx],
                       y = dt_train[, y_col_idx],
                       len_candidate_basis_set = len_candidate_basis_set,
                       len_final_basis_set = len_final_basis_set,
                       max_rows = max_rows,
                       max_degree = max_degree,
                       batch_size = batch_size,
                       n_batch = n_batch,
                       p = p,
                       seed = seed,
                       weight_function = weight_function,
                       family = family,
                       n_cores = n_cores)
  sHAL_res <- sHAL_obj$run(verbose = TRUE, plot = FALSE)
  sHAL_lasso <- sHAL_res[[1]]
  sHAL_basis_set <- sHAL_res[[2]]

  # get rmse
  basis_matrix_test <- make_design_matrix(sHAL_basis_set, dt_test[, x_col_idx])
  sHAL_pred <- predict(sHAL_lasso, newx = basis_matrix_test, type = "response", s = "lambda.min")
  sHAL_true <- dt_test[, y_col_idx]
  sHAL_loss <- get_loss(sHAL_pred, sHAL_true, family)

  # get number of non zero coefficients
  sHAL_num_non_zero <- sum(coef(sHAL_lasso) != 0)

  # get best loss trajectory
  best_loss_traj <- sHAL_obj$best_loss_traj

  return(list(hal9001_loss,
              hal9001_num_non_zero,
              sHAL_loss,
              sHAL_num_non_zero,
              sHAL_basis_set,
              best_loss_traj))
}

# real data
run_real_data <- function(fpath,
                          y_col_idx,
                          len_candidate_basis_set,
                          len_final_basis_set,
                          max_rows,
                          max_degree,
                          batch_size,
                          n_batch,
                          p,
                          seed,
                          weight_function,
                          family = "gaussian",
                          n_cores) {
  # load data
  dt <- read.csv(fpath)
  dt <- drop_na(dt)

  set.seed(seed)

  # train-test split
  train_indices <- sample(x = 1:nrow(dt), size = floor(0.8 * nrow(dt)))
  dt_train <- dt[train_indices, ]
  dt_test <- dt[-train_indices, ]

  x_col_idx <- setdiff(seq_len(ncol(dt)), y_col_idx)

  # run sHAL -----------------------------------------------------------------
  print("running sHAL...")
  sHAL_obj <- sHAL$new(X = dt_train[, x_col_idx],
                       y = dt_train[, y_col_idx],
                       len_candidate_basis_set = len_candidate_basis_set,
                       len_final_basis_set = len_final_basis_set,
                       max_rows = max_rows,
                       max_degree = max_degree,
                       batch_size = batch_size,
                       n_batch = n_batch,
                       p = p,
                       seed = seed,
                       weight_function = weight_function,
                       family = family,
                       n_cores = n_cores)
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

  best_loss_traj <- sHAL_obj$best_loss_traj # loss trajectory

  return(list(sHAL_loss, sHAL_num_non_zero, best_loss_traj, sHAL_basis_set))
}
