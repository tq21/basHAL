
get_result <- function(fname) {
  res <- readRDS(fname)
  print("hal9001 RMSE: " %+% mean(res[[1]]))
  print("sHAL RMSE: " %+% mean(res[[3]]))
  print("hal9001 non-zero: " %+% mean(res[[2]]))
  print("sHAL non-zero: " %+% mean(res[[4]]))
}

get_result("benchmark/out/cpu_inverse_loss.RDS")
get_result("benchmark/out/cpu_double_weight_v3.RDS")
