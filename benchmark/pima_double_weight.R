devtools::load_all()

pima_double_weight <- run_benchmark(fpath = "../../data/small_data/pima.csv",
                                    V = 10,
                                    y_col_idx = 1,
                                    len_candidate_basis_set = 200,
                                    len_final_basis_set = 200,
                                    max_rows = 392,
                                    max_degree = 5,
                                    batch_size = 100,
                                    n_batch = 100,
                                    p = 0.5,
                                    seed = 12941,
                                    weight_function = "double weight",
                                    family = "binomial")
saveRDS(pima_double_weight, "../out/pima_double_weight.RDS")
