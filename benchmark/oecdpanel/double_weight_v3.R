devtools::load_all()

oecdpanel_double_weight_v3 <- run_benchmark(fpath = "../../data/small_data/oecdpanel.csv",
                                            y_col_idx = 1,
                                            len_candidate_basis_set = 616,
                                            len_final_basis_set = 616,
                                            max_rows = 616,
                                            max_degree = 6,
                                            batch_size = 100,
                                            n_batch = 200,
                                            p = 0.5,
                                            seed = 12941,
                                            weight_function = "double weight v3",
                                            n_cores = 32)
saveRDS(oecdpanel_double_weight_v3, "out/oecdpanel_double_weight_v3.RDS")
