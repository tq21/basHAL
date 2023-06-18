library(cuda.ml)
library(glmnet)
library(microbenchmark)

data(mtcars)

y <- mtcars$mpg
X <- as.matrix(mtcars[-1])

cpu_benchmark <- microbenchmark(
  glmnet_model_cpu <- glmnet(X, y, alpha = 1, lambda = 0.01),
  times = 1000L
)

gpu_benchmark <- microbenchmark(
  glmnet_model_gpu <- cuda_ml_lasso(X, y, alpha = 0.01),
  times = 100L
)

print(cpu_benchmark)
print(gpu_benchmark)
