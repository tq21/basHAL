


library(hal9001)

for (i in 1:length(dt_list)) {
  dt <- dt_list[[i]]
  dt_test <- dt_list_test[[i]]
  y_col_idx <- ncol(dt)
  x_col_idx <- setdiff(1:ncol(dt), ncol(dt))
  hal9001_fit <- fit_hal(X = dt[, x_col_idx],
                         Y = dt[, y_col_idx],
                         family = "gaussian",
                         max_degree = ncol(dt)-1,
                         smoothness_orders = 0)
  # get loss
  hal9001_pred <- predict(hal9001_fit, new_data = dt_test[, x_col_idx])
  hal9001_true <- dt_test[, y_col_idx]
  print("rmse: " %+% sqrt(mean((hal9001_pred - hal9001_true)^2)))
  print(length(summary(hal9001_fit)$table$coef) - 1)
}
