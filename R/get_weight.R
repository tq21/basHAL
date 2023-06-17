get_weight <- function(weight_fun,
                       loss, base_loss,
                       num_non_zero, total_num,
                       loss_prop = 0.5) {
  if (weight_fun == "inverse loss") {
    return(1/loss)
  } else if (weight_fun == "double weight") {
    return(num_non_zero/total_num-loss/base_loss)
  } else if (weight_fun == "double weight v2") {
    return(num_non_zero/total_num+(1-loss/base_loss))
  } else if (weight_fun == "double weight v3") {
    return((1-loss_prop)*(num_non_zero/total_num)+loss_prop*(1-loss/base_loss))
  }
}
