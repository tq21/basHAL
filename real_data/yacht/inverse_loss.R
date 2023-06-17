devtools::load_all()

yacht_inverse_loss <- run_real_data(fpath = "../../data/med_data/yacht.csv",
                                    y_col_idx = 7,
                                    len_candidate_basis_set = 308,
                                    len_final_basis_set = 308,
                                    max_rows = 308,
                                    max_degree = 6,
                                    batch_size = 100,
                                    n_batch = 100,
                                    p = 0.5,
                                    seed = 12941,
                                    weight_function = "inverse loss",
                                    n_cores = 32)
saveRDS(yacht_inverse_loss, "yacht_inverse_loss.RDS")
