library(devtools)
library(origami)
library(xgboost)
library(randomForest)

load_all()

set.seed(123)
dt <- read.csv("../data/boston.csv")
y_col_idx <- ncol(dt)
x_col_idx <- setdiff(seq_len(ncol(dt)), y_col_idx)

# 80% train, 20% test
folds <- make_folds(n = nrow(dt), V = 1, fold_fun = folds_montecarlo)
X_train <- dt[folds[[1]]$training_set, x_col_idx]
X_test <- dt[folds[[1]]$validation_set, x_col_idx]
y_train <- dt[folds[[1]]$training_set, y_col_idx]
y_test <- dt[folds[[1]]$validation_set, y_col_idx]

# supermass --------------------------------------------------------------------
supermass_obj <- basHAL$new(X = X_train,
                            y = y_train,
                            method = "supermass",
                            len_final_basis_set = 1000,
                            max_rows = nrow(X_train),
                            max_degree = 5,
                            batch_size = 100,
                            n_batch = 100,
                            p = 0.5,
                            seed = 29857,
                            n_cores = 5)
supermass_obj$run(verbose = TRUE)
test_pred <- supermass_obj$predict(newx = X_test, type = "response")
supermass_rmse <- sqrt(mean((y_test - test_pred)^2))
print(paste("supermass rmse: ", supermass_rmse))

# xgboost ----------------------------------------------------------------------
dtrain <- xgb.DMatrix(data = as.matrix(X_train), label = y_train)
dtest <- xgb.DMatrix(data = as.matrix(X_test), label = y_test)
xgb_fit <- xgboost(data = dtrain, nrounds = 1000, verbose = 0)
xgb_pred <- predict(xgb_fit, dtest)
xgb_rmse <- sqrt(mean((y_test - xgb_pred)^2))
print(paste("xgboost rmse: ", xgb_rmse))

# random forest ------------------------------------------------------------
rf <- randomForest(x = X_train, y = y_train, ntree = 500)
rf_pred <- predict(rf, X_test)
rf_rmse <- sqrt(mean((y_test - rf_pred)^2))
print(paste("random forest rmse: ", rf_rmse))
