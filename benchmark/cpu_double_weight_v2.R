devtools::load_all()

cpu_double_weight_v2 <- run_benchmark(fpath = "../../data/small_data/cpu.csv",
                                      V = 10,
                                      y_col_idx = 1,
                                      len_candidate_basis_set = 209,
                                      len_final_basis_set = 209,
                                      max_rows = 209,
                                      max_degree = 6,
                                      batch_size = 100,
                                      n_batch = 100,
                                      p = 0.5,
                                      seed = 12941,
                                      weight_function = "double weight v2")
saveRDS(cpu_double_weight_v2, "../out/cpu_double_weight_v2.RDS")
