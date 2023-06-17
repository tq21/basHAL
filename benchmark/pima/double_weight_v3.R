devtools::load_all()

pima_double_weight_v3 <- run_benchmark(fpath = "../../data/small_data/pima.csv",
                                       y_col_idx = 1,
                                       len_candidate_basis_set = 392,
                                       len_final_basis_set = 392,
                                       max_rows = 392,
                                       max_degree = 7,
                                       batch_size = 100,
                                       n_batch = 100,
                                       p = 0.5,
                                       seed = 12941,
                                       weight_function = "double weight v3",
                                       #family = "binomial",
                                       n_cores = 32)
saveRDS(pima_double_weight_v3, "pima_double_weight_v3.RDS")
