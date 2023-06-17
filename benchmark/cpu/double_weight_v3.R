devtools::load_all()

cpu_double_weight_v3 <- run_benchmark(fpath = "../../data/small_data/cpu.csv",
                                      y_col_idx = 1,
                                      len_candidate_basis_set = 209,
                                      len_final_basis_set = 209,
                                      max_rows = 209,
                                      max_degree = 6,
                                      batch_size = 100,
                                      n_batch = 100,
                                      p = 0.5,
                                      seed = 12941,
                                      weight_function = "double weight v3",
                                      n_cores = 32)
saveRDS(cpu_double_weight_v3, "../out/cpu_double_weight_v3.RDS")
