devtools::load_all()

laheart_double_weight_v3 <- run_benchmark(fpath = "../../data/small_data/laheart.csv",
                                          V = 10,
                                          y_col_idx = 1,
                                          len_candidate_basis_set = 200,
                                          len_final_basis_set = 200,
                                          max_rows = 200,
                                          max_degree = 5,
                                          batch_size = 100,
                                          n_batch = 100,
                                          p = 0.5,
                                          seed = 12941,
                                          weight_function = "double weight v3",
                                          family = "binomial")
saveRDS(laheart_double_weight_v3, "../out/laheart_double_weight_v3.RDS")
