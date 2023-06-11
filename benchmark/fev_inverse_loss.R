devtools::load_all()

fev_inverse_loss <- run_benchmark(fpath = "../../data/small_data/fev.csv",
                                  V = 10,
                                  y_col_idx = 1,
                                  len_candidate_basis_set = 100,
                                  len_final_basis_set = 100,
                                  max_rows = 654,
                                  max_degree = 4,
                                  batch_size = 50,
                                  n_batch = 50,
                                  p = 0.5,
                                  seed = 12941,
                                  weight_function = "inverse loss")
saveRDS(fev_inverse_loss, "../out/fev_inverse_loss.RDS")
