get_weights <- function(weight_fun,
                        loss,
                        base_loss,
                        coefs,
                        num_non_zero, total_num,
                        loss_prop = 0.5) {
  if (weight_fun == "glm") {
    return((1-loss/base_loss)/sum(abs(coefs))*abs(coefs))
  } else if (weight_fun == "univariate glm") {
    return(1-loss/base_loss)
  } else if (weight_fun == "k-variate glmnet") {
    return(rep(1-loss/base_loss, length(num_non_zero)))
  } else if (weight_fun == "double weight") {
    return(num_non_zero/total_num-loss/base_loss)
  } else if (weight_fun == "double weight v2") {
    return(num_non_zero/total_num+(1-loss/base_loss))
  } else if (weight_fun == "double weight v3") {
    return((1-loss_prop)*(num_non_zero/total_num)+loss_prop*(1-loss/base_loss))
  }
}
