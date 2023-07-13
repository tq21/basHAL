library(ggplot2)

# count the number of degree 1, 2, ..., g-basis functions in a basis set
count_basis <- function(basis_set) {
  degrees <- sapply(basis_set, function(basis) length(basis[["col_indices"]]))
  degrees <- as.factor(degrees)
  res <- as.data.frame(summary(degrees))
  res <- cbind(res, round(res[,1]/length(basis_set)*100, 2))
  colnames(res) <- c("number of basis functions", "percentage")

  return(res)
}

# basis_1 <- Basis$new(c(1,2), c(10,20))
# basis_2 <- Basis$new(c(1,4,6), c(30,20,10))
# basis_3 <- Basis$new(c(2,5,9,10), c(10,20,10,43))
#
# basis_set <- list(basis_1, basis_2, basis_3)
#
# count_basis(basis_set)

library(hal9001)
library(tidyverse)

`%+%` <- function(a, b) paste0(a, b)

run_benchmark <- function(fpath,
                          y_col_idx,
                          len_candidate_basis_set,
                          len_final_basis_set,
                          max_rows,
                          max_degree,
                          batch_size,
                          n_batch,
                          weight_function,
                          seed,
                          family = "gaussian",
                          loss_prop = 0.5,
                          n_cores,
                          top_k) {
  # load data
  dt <- read.csv(fpath)
  dt <- drop_na(dt)
  x_col_idx <- setdiff(seq_len(ncol(dt)), y_col_idx)

  # 10-fold CV
  set.seed(seed)
  strata_ids <- NULL
  if (family == "binomial") {
    strata_ids <- dt[, y_col_idx]
  }

  V <- 10

  folds <- make_folds(n = nrow(dt), V = V,
                      fold_fun = folds_vfold,
                      strata_ids = strata_ids)

  hal9001_loss <- c()
  hal9001_non_zero <- c()
  random_loss <- c()
  random_non_zero <- c()
  bsas_1_loss <- c()
  bsas_1_non_zero <- c()
  bsas_2_loss <- c()
  bsas_2_non_zero <- c()

  for (v in 1:V) {
    print("fold: " %+% v)
    train_indices <- folds[[v]]$training_set
    test_indices <- folds[[v]]$validation_set
    dt_train <- dt[train_indices, ]
    dt_valid <- dt[test_indices, ]

    # run hal9001 --------------------------------------------------------------
    # print("running hal9001...")
    # hal9001_fit <- fit_hal(X = dt_train[, x_col_idx],
    #                        Y = dt_train[, y_col_idx],
    #                        family = family,
    #                        max_degree = 9,
    #                        smoothness_orders = 0)
#
    # # get loss
    # hal9001_pred <- predict(hal9001_fit, new_data = dt_valid[, x_col_idx], type = "response")
    # hal9001_loss <- c(hal9001_loss, get_loss(hal9001_pred, dt_valid[, y_col_idx], family))
#
    # # get number of non zero coefficients
    # hal9001_non_zero <- c(hal9001_non_zero, length(summary(hal9001_fit)$table$coef) - 1) # exclude intercept

    # run random ---------------------------------------------------------------
    # print("running random...")
    # random_obj <- sHAL$new(X = dt_train[, x_col_idx],
    #                        y = dt_train[, y_col_idx],
    #                        len_candidate_basis_set = len_candidate_basis_set,
    #                        len_final_basis_set = len_final_basis_set,
    #                        max_rows = max_rows,
    #                        max_degree = max_degree,
    #                        batch_size = 1,
    #                        n_batch = 1,
    #                        p = 1,
    #                        seed = seed,
    #                        weight_function = weight_function,
    #                        family = family,
    #                        loss_prop = loss_prop,
    #                        n_cores = n_cores,
    #                        top_k = FALSE)
    # random_res <- random_obj$run(verbose = TRUE, plot = FALSE)
    # random_lasso <- random_res[[1]]
    # random_basis_set <- random_res[[2]]
#
    # # get loss
    # basis_matrix_valid <- make_design_matrix(random_basis_set, dt_valid[, x_col_idx])
    # random_pred <- predict(random_lasso, newx = basis_matrix_valid, type = "response", s = "lambda.min")
    # random_loss <- c(random_loss, get_loss(random_pred, dt_valid[, y_col_idx], family))
#
    # # get number of non zero coefficients
    # random_non_zero <- c(random_non_zero, sum(coef(random_lasso) != 0))

    # run bsas 1 ---------------------------------------------------------------
    print("running bsas 1...")
    bsas_1_obj <- sHAL$new(X = dt_train[, x_col_idx],
                           y = dt_train[, y_col_idx],
                           len_candidate_basis_set = 100,
                           len_final_basis_set = 50, # len_final_basis_set,
                           max_rows = max_rows,
                           max_degree = max_degree,
                           batch_size = batch_size,
                           n_batch = n_batch,
                           p = 0.5,
                           seed = seed,
                           weight_function = weight_function,
                           family = family,
                           loss_prop = loss_prop,
                           n_cores = n_cores,
                           top_k = TRUE)
    bsas_1_res <- bsas_1_obj$run(verbose = TRUE, plot = FALSE)
    bsas_1_lasso <- bsas_1_res[[1]]
    bsas_1_basis_set <- bsas_1_res[[2]]

    # get loss
    basis_matrix_valid <- make_design_matrix(bsas_1_basis_set, dt_valid[, x_col_idx])
    # bsas_1_pred <- predict(bsas_1_lasso, newx = basis_matrix_valid, type = "response", s = "lambda.min")
    bsas_1_pred <- predict(bsas_1_lasso, newx = data.frame(basis_matrix_valid), type = "response")
    bsas_1_loss <- c(bsas_1_loss, get_loss(bsas_1_pred, dt_valid[, y_col_idx], family))

    # get number of non zero coefficients
    # bsas_1_non_zero <- c(bsas_1_non_zero, sum(coef(bsas_1_lasso) != 0))

    # run bsas 2 ---------------------------------------------------------------
    # print("running bsas 2...")
    # bsas_2_obj <- sHAL$new(X = dt_train[, x_col_idx],
    #                        y = dt_train[, y_col_idx],
    #                        len_candidate_basis_set = 100,
    #                        len_final_basis_set = len_final_basis_set,
    #                        max_rows = max_rows,
    #                        max_degree = max_degree,
    #                        batch_size = batch_size,
    #                        n_batch = n_batch,
    #                        p = 0.5,
    #                        seed = seed,
    #                        weight_function = weight_function,
    #                        family = family,
    #                        loss_prop = loss_prop,
    #                        n_cores = n_cores,
    #                        top_k = TRUE)
    # bsas_2_res <- bsas_2_obj$run(verbose = TRUE, plot = FALSE)
    # bsas_2_lasso <- bsas_2_res[[1]]
    # bsas_2_basis_set <- bsas_2_res[[2]]
#
    # # get loss
    # basis_matrix_valid <- make_design_matrix(bsas_2_basis_set, dt_valid[, x_col_idx])
    # bsas_2_pred <- predict(bsas_2_lasso, newx = basis_matrix_valid, type = "response", s = "lambda.min")
    # bsas_2_loss <- c(bsas_2_loss, get_loss(bsas_2_pred, dt_valid[, y_col_idx], family))
#
    # # get number of non zero coefficients
    # bsas_2_non_zero <- c(bsas_2_non_zero, sum(coef(bsas_2_lasso) != 0))
#
    # gc()
  }

  return(bsas_1_loss)

  # return(list(hal9001_loss,
  #             random_loss,
  #             bsas_1_loss,
  #             bsas_2_loss,
  #             hal9001_non_zero,
  #             random_non_zero,
  #             bsas_1_non_zero,
  #             bsas_2_non_zero))
}

# real data
run_real_data <- function(fpath,
                          y_col_idx,
                          len_candidate_basis_set,
                          len_final_basis_set,
                          max_rows,
                          max_degree,
                          batch_size,
                          n_batch,
                          p,
                          seed,
                          weight_function,
                          family = "gaussian",
                          n_cores) {
  # load data
  dt <- read.csv(fpath)
  dt <- drop_na(dt)

  set.seed(seed)

  # train-test split
  train_indices <- sample(x = 1:nrow(dt), size = floor(0.8 * nrow(dt)))
  dt_train <- dt[train_indices, ]
  dt_test <- dt[-train_indices, ]

  x_col_idx <- setdiff(seq_len(ncol(dt)), y_col_idx)

  # run sHAL -----------------------------------------------------------------
  print("running sHAL...")
  sHAL_obj <- sHAL$new(X = dt_train[, x_col_idx],
                       y = dt_train[, y_col_idx],
                       len_candidate_basis_set = len_candidate_basis_set,
                       len_final_basis_set = len_final_basis_set,
                       max_rows = max_rows,
                       max_degree = max_degree,
                       batch_size = batch_size,
                       n_batch = n_batch,
                       p = p,
                       seed = seed,
                       weight_function = weight_function,
                       family = family,
                       n_cores = n_cores)
  sHAL_res <- sHAL_obj$run(verbose = TRUE, plot = FALSE)
  sHAL_lasso <- sHAL_res[[1]]
  sHAL_basis_set <- sHAL_res[[2]]

  # get rmse
  basis_matrix_test <- make_design_matrix(sHAL_basis_set, dt_test[, x_col_idx])
  sHAL_pred <- predict(sHAL_lasso, newx = basis_matrix_test, type = "response", s = "lambda.min")
  sHAL_true <- dt_test[, y_col_idx]
  sHAL_loss <- ifelse(family == "gaussian",
                      sqrt(mean((sHAL_pred - sHAL_true)^2)),
                      -mean(sHAL_true * log(sHAL_pred) + (1 - sHAL_true) * log(1 - sHAL_pred)))

  # get number of non zero coefficients
  sHAL_num_non_zero <- sum(coef(sHAL_lasso) != 0)

  best_loss_traj <- sHAL_obj$best_loss_traj # loss trajectory

  return(list(sHAL_loss, sHAL_num_non_zero, best_loss_traj, sHAL_basis_set))
}

plot_valid_loss <- function(valid_loss,
                            y_low,
                            y_high,
                            by,
                            title) {
  plt_df <- data.frame(loss = valid_loss, iter = seq(1, length(valid_loss)))
  plt_df <- plt_df[!duplicated(plt_df$loss), ]
  plt_df <- rbind(plt_df, data.frame(loss = min(valid_loss), iter = length(valid_loss)))

  p <- ggplot(plt_df, aes(x = iter, y = loss)) +
    #geom_point(color = "black") +
    geom_step(color = "black", lwd = 1) +
    scale_x_continuous(breaks = seq(0, length(valid_loss), by = 20),
                       limits = c(0, 100)) +
    scale_y_continuous(breaks = seq(y_low, y_high, by = by),
                       limits = c(y_low, y_high)) +
    labs(title = title,
         x = "Iteration",
         y = "Validation loss") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.position = "none")

  return(p)
}

plot_test_loss <- function(test_loss,
                           y_low,
                           y_high,
                           by,
                           title) {
  plt_df <- data.frame(loss = test_loss, prop_loss = seq(0, 1, 0.1))

  p <- ggplot(plt_df, aes(x = prop_loss, y = loss)) +
    geom_point(color = "black", shape = 17) +
    scale_x_continuous(breaks = seq(0, 1, by = 0.2),
                       limits = c(0, 1)) +
    scale_y_continuous(breaks = seq(y_low, y_high, by = by),
                       limits = c(y_low, y_high)) +
    labs(title = title,
         x = "Proportion of loss",
         y = "Test loss") +
    theme_bw() +
    theme(plot.title = element_text(hjust = 0.5),
          text = element_text(size = 14),
          axis.title = element_text(size = 16),
          legend.position = "none")

  return(p)
}
