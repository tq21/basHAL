library(hal9001)
library(glmnet)

additive <- function(X, y) {
  lasso_fit <- cv.glmnet(x = as.matrix(X), y = y, nfolds = 5)
  init_pred <- as.numeric(predict(lasso_fit, newx = as.matrix(X), s = "lambda.min"))
  res <- y - init_pred
  hal_fit <- fit_hal(X = X, Y = res, smoothness_orders = 0)

  return(list(lasso = lasso_fit,
              hal = hal_fit))
}

predict_additive <- function(additive_obj, newx) {
  lasso_fit <- additive_obj$lasso
  hal_fit <- additive_obj$hal
  lasso_pred <- as.numeric(predict(lasso_fit, newx = as.matrix(newx), s = "lambda.min"))
  hal_pred <- as.numeric(predict(hal_fit, new_data = newx))
  pred <- lasso_pred + hal_pred

  return(pred)
}
