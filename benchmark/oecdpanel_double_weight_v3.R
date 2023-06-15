devtools::load_all()

oecdpanel_double_weight_v3 <- run_benchmark(fpath = "../../data/small_data/oecdpanel.csv",
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
                                            weight_function = "double weight v3")
saveRDS(oecdpanel_double_weight_v3, "../out/oecdpanel_double_weight_v3.RDS")
