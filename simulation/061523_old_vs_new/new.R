library(devtools)
load_all()

# load data
dt <- read.csv("../../data/small_data/cpu.csv")
dt <- drop_na(dt)
y_col_idx <- 1
x_col_idx <- setdiff(seq_len(ncol(dt)), y_col_idx)

# train-test split
set.seed(123)
train_indices <- sample(x = 1:nrow(dt), size = floor(0.8 * nrow(dt)))
dt_train <- dt[train_indices, ]
dt_test <- dt[-train_indices, ]

# run sHAL -----------------------------------------------------------------
sHAL_obj <- sHAL$new(X = dt_train[, x_col_idx],
                     y = dt_train[, y_col_idx],
                     len_candidate_basis_set = 200,
                     len_final_basis_set = 200,
                     max_rows = 209,
                     max_degree = 6,
                     batch_size = 100,
                     n_batch = 100,
                     p = 0.5,
                     seed = 123,
                     weight_function = "inverse loss",
                     family = "gaussian",
                     n_cores = 32)
sHAL_res <- sHAL_obj$run(verbose = TRUE, plot = FALSE)
sHAL_lasso <- sHAL_res[[1]]
sHAL_basis_set <- sHAL_res[[2]]

# get rmse
basis_matrix_test <- make_design_matrix(sHAL_basis_set, dt_test[, x_col_idx])
sHAL_pred <- predict(sHAL_lasso, newx = basis_matrix_test, type = "response")
sHAL_true <- dt_test[, y_col_idx]
print("RMSE: " %+% sqrt(mean((sHAL_pred - sHAL_true)^2)))

# get number of non zero coefficients
print("Non zero coefficients: " %+% sum(coef(sHAL_lasso) != 0))
