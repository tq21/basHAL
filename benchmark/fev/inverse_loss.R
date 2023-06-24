devtools::load_all()

fev_inverse_loss <- run_benchmark(fpath = "../../data/small_data/fev.csv",
                                  y_col_idx = 1,
                                  len_candidate_basis_set = 654,
                                  len_final_basis_set = 654,
                                  max_rows = 654,
                                  max_degree = 4,
                                  batch_size = 100,
                                  n_batch = 200,
                                  p = 0.5,
                                  seed = 12941,
                                  weight_function = "inverse loss",
                                  n_cores = 32)
saveRDS(fev_inverse_loss, "out/fev_inverse_loss.RDS")
