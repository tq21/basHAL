library(hal9001)

`%+%` <- function(a, b) paste0(a, b)

fpath <- "data/small_data/cpu.csv"
V <- 10

# first column is y

run_benchmark <- function(fpath, V) {
  # load data
  dt <- read.csv(fpath)

  # make folds
  folds <- sample(rep(1:V, length.out = nrow(dt)))

  hal9001_rmse <- rep(NA, V)
  sHAL_rmse <- rep(NA, V)

  for (v in 1:V) {
    # train-validation split
    dt_train <- dt[folds != v,]
    dt_valid <- dt[folds == v,]

    # run hal9001
    hal9001_fit <- fit_hal(X = dt_train[, 2:ncol(dt_train)],
                           Y = dt_train[, 1],
                           family = "gaussian",
                           max_degree = max_degree,
                           smoothness_orders = 0)
    hal9001_pred <- predict(hal9001_fit,
                            new_data = dt_valid[, 2:ncol(dt_valid)])
    hal9001_true <- dt_valid[, 1]
    hal9001_rmse[v] <- sqrt(mean((hal9001_pred - hal9001_true)^2))

    # run sHAL
    sHAL_obj <- sHAL$new(X = dt_train[, 2:ncol(dt_train)],
                         y = dt_train[, 1],
                         len_candidate_basis_set = len_candidate_basis_set,
                         len_final_basis_set = len_final_basis_set,
                         max_rows = max_rows,
                         max_degree = max_degree,
                         batch_size = batch_size,
                         n_batch = n_batch)
    sHAL_res <- sHAL_obj$run(verbose = TRUE, plot = FALSE)
    sHAL_lasso <- sHAL_res[[1]]
    sHAL_basis_set <- sHAL_res[[2]]

    # basis frequency
    count_basis(final_basis_set)

  }

}











