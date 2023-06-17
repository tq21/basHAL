devtools::load_all()

boston_double_weight_v3 <- run_real_data(fpath = "../../data/med_data/boston.csv",
                                         y_col_idx = 14,
                                         len_candidate_basis_set = 507,
                                         len_final_basis_set = 507,
                                         max_rows = 507,
                                         max_degree = 13,
                                         batch_size = 100,
                                         n_batch = 100,
                                         p = 0.5,
                                         seed = 12941,
                                         weight_function = "double weight v3",
                                         n_cores = 32)
saveRDS(boston_double_weight_v3, "boston_double_weight_v3.RDS")
