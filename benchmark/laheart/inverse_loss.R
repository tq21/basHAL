devtools::load_all()

laheart_inverse_loss <- run_benchmark(fpath = "../../data/small_data/laheart.csv",
                                      y_col_idx = 1,
                                      len_candidate_basis_set = 201,
                                      len_final_basis_set = 201,
                                      max_rows = 201,
                                      max_degree = 11,
                                      batch_size = 200,
                                      n_batch = 200,
                                      p = 0.5,
                                      seed = 12941,
                                      weight_function = "inverse loss",
                                      family = "binomial",
                                      n_cores = 32)
saveRDS(laheart_inverse_loss, "laheart_inverse_loss.RDS")
