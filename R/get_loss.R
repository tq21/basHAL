get_loss <- function(y_pred, y_true, family) {
  if (family == "gaussian") {
    return(sqrt(mean((y_true - y_pred)^2)))
  } else if (family == "binomial") {
    return(-mean(y_true * log(y_pred) + (1 - y_true) * log(1 - y_pred)))
  }
}
