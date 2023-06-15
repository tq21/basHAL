devtools::load_all()

oecdpanel_double_weight_v2 <- run_benchmark(fpath = "../../data/small_data/oecdpanel.csv",
                                            V = 5,
                                            y_col_idx = 1,
                                            len_candidate_basis_set = 200,
                                            len_final_basis_set = 200,
                                            max_rows = 616,
                                            max_degree = 5,
                                            batch_size = 500,
                                            n_batch = 200,
                                            p = 0.5,
                                            seed = 12941,
                                            n_cores = 32,
                                            weight_function = "double weight v2")
saveRDS(oecdpanel_double_weight_v2, "../out/oecdpanel_double_weight_v2.RDS")
