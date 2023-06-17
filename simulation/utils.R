library(devtools)
load_all()
source("../sim_data.R")

`%+%` <- function(a, b) paste0(a, b)

set.seed(124141)
run_sim <- function(n,
                    type,
                    len_candidate_basis_set,
                    len_final_basis_set,
                    max_rows,
                    max_degree,
                    batch_size,
                    n_batch,
                    p,
                    seed,
                    weight_function,
                    family,
                    n_cores,
                    n_test = 10000) {
  # univariate
  sim_uni_smooth_dt <- sim_uni_smooth(n)
  sim_uni_smooth_dt_test <- sim_uni_smooth(n_test)

  sim_uni_jump_dt <- sim_uni_jump(n)
  sim_uni_jump_dt_test <- sim_uni_jump(n_test)

  sim_uni_sin_dt <- sim_uni_sin(n)
  sim_uni_sin_dt_test <- sim_uni_sin(n_test)

  # trivariate
  sim_tri_smooth_dt <- sim_tri_smooth(n)
  sim_tri_smooth_dt_test <- sim_tri_smooth(n_test)

  sim_tri_jump_dt <- sim_tri_jump(n)
  sim_tri_jump_dt_test <- sim_tri_jump(n_test)

  sim_tri_sin_dt <- sim_tri_sin(n)
  sim_tri_sin_dt_test <- sim_tri_sin(n_test)

  # five-variate
  sim_five_smooth_dt <- sim_five_smooth(n)
  sim_five_smooth_dt_test <- sim_five_smooth(n_test)

  sim_five_jump_dt <- sim_five_jump(n)
  sim_five_jump_dt_test <- sim_five_jump(n_test)

  sim_five_sin_dt <- sim_five_sin(n)
  sim_five_sin_dt_test <- sim_five_sin(n_test)

  dt_list <- list()
  dt_list_test <- list()
  if (type == "univariate") {
    dt_list <- list(sim_uni_smooth_dt, sim_uni_jump_dt, sim_uni_sin_dt)
    dt_list_test <- list(sim_uni_smooth_dt_test, sim_uni_jump_dt_test, sim_uni_sin_dt_test)
  } else if (type == "trivariate") {
    dt_list <- list(sim_tri_smooth_dt, sim_tri_jump_dt, sim_tri_sin_dt)
    dt_list_test <- list(sim_tri_smooth_dt_test, sim_tri_jump_dt_test, sim_tri_sin_dt_test)
  } else if (type == "five-variate") {
    dt_list <- list(sim_five_smooth_dt, sim_five_jump_dt, sim_five_smooth_dt)
    dt_list_test <- list(sim_five_smooth_dt_test, sim_five_jump_dt_test, sim_five_smooth_dt_test)
  }

  for (i in 1:length(dt_list)) {
    print(i %+% " out of " %+% length(dt_list))
    dt <- dt_list[[i]]
    dt_test <- dt_list_test[[i]]
    y_col_idx <- ncol(dt)
    x_col_idx <- setdiff(1:ncol(dt), ncol(dt))

    # initialize HAL
    sHAL_obj <- sHAL$new(X = as.data.frame(dt[, x_col_idx]),
                         y = dt[, y_col_idx],
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
    basis_matrix_test <- make_design_matrix(sHAL_basis_set, as.data.frame(dt_test[, x_col_idx]))
    sHAL_pred <- predict(sHAL_lasso, newx = basis_matrix_test)
    sHAL_true <- dt_test[, y_col_idx]
    print("rmse: " %+% sqrt(mean((sHAL_pred - sHAL_true)^2)))

    # get number of non zero coefficients
    print(sum(coef(sHAL_lasso) != 0))

    best_loss_traj <- sHAL_obj$best_loss_traj # loss trajectory
    best_loss_batch <- sHAL_obj$best_loss_batch # batch number for the best loss

    # save lasso fit and basis set
    save(list = c("sHAL_lasso", "sHAL_basis_set", "best_loss_traj", "best_loss_batch"),
         file = "simulation_" %+% i %+% "_" %+% gsub(" ", "_", weight_function) %+% ".RData")
  }
}
