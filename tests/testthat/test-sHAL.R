library(testthat)

# load data
data <- read.csv("data/boston.csv")
print(paste("Data dimension: ", dim(data)))

# get X and y
X <- as.matrix(data[, 1:(ncol(data)-1)])
y <- as.matrix(data[, ncol(data)])

set.seed(123)

# train-test split
indices <- sample(seq_len(nrow(X)), size = 0.1 * nrow(X))
X_test <- X[indices,]
y_test <- y[indices]
X_train <- X[-indices,]
y_train <- y[-indices]

# initialize S-HAL object
sHAL_obj <- sHAL$new(X = X_train,
                     y = y_train,
                     len_candidate_basis_set = nrow(X_train),
                     len_final_basis_set = nrow(X_train),
                     max_rows = nrow(X_train),
                     max_degree = 7,
                     batch_size = 100,
                     n_batch = 50,
                     p = 0.1,
                     seed = 29857)

# run S-HAL
result <- sHAL_obj$run(verbose = TRUE, plot = TRUE)
final_lasso <- result[[1]]
final_basis_set <- result[[2]]

count_basis(final_basis_set)

sampled_basis_set <- sHAL_obj$get_top_k(456)
fit <- sHAL_obj$fit_sampled_basis_set(sampled_basis_set)

final_lasso <- fit
final_basis_set <- sampled_basis_set

print(paste("Number of non zero coefficients in the final lasso: ", sum(coef(final_lasso) != 0)))

# generate basis matrix for test data
basis_matrix_test <- make_design_matrix(final_basis_set, X_test)
test_pred <- predict(final_lasso, newx = basis_matrix_test)
s_hal_rmse <- sqrt(mean((y_test - test_pred)^2))
print(paste("RMSE: ", s_hal_rmse))




hist(sHAL_obj$probs)




  X = X_train
  y = y_train
  len_candidate_basis_set = 5
  len_final_basis_set = nrow(X_train)
  max_rows = 400
  max_degree = 5
  batch_size = 2
  n_batch = 2
  random_state = 2985
  V_folds <- 5
  self <- list()
  self$X <- X
  self$y <- y
  self$len_candidate_basis_set <- len_candidate_basis_set
  self$len_final_basis_set <- len_final_basis_set
  self$max_rows <- max_rows
  self$max_degree <- max_degree
  self$batch_size <- batch_size
  self$n_batch <- n_batch
  self$V_folds <- V_folds
  self$model_loss_env <- new.env(hash = TRUE)

  self$generate_basis_set <- generate_basis_set
  self$evaluate_candidate <- evaluate_candidate
  self$top_k_from_dictionary <- top_k_from_dictionary
  self$fit_sampled_basis_set <- fit_sampled_basis_set
  self$run <- run
