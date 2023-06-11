inverse_loss_weight <- function(loss) {
  return(1 / loss)
}

double_weight <- function(num_non_zero, total_num, loss, base_loss) {
  return(num_non_zero/total_num-loss/base_loss)
}

double_weight_v2 <- function(num_non_zero, total_num, loss, base_loss) {
  return(num_non_zero/total_num+(1-loss/base_loss))
}

double_weight_v3 <- function(num_non_zero, total_num, loss, base_loss) {
  return(0.5*(num_non_zero/total_num)+0.5*(1-loss/base_loss))
}
