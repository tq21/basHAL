devtools::load_all()

oecdpanel_random <- run_benchmark(fpath = "../../data/small_data/oecdpanel.csv",
                                  y_col_idx = 1,
                                  len_candidate_basis_set = 616,
                                  len_final_basis_set = 616,
                                  max_rows = 616,
                                  max_degree = 6,
                                  batch_size = 100,
                                  n_batch = 100,
                                  p = 1,
                                  seed = 12941,
                                  weight_function = "inverse loss",
                                  n_cores = 32)
saveRDS(oecdpanel_random, "oecdpanel_random.RDS")
