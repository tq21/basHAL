source("../utils.R")

samp_res <- run_sim(n = 1000,
                    type = "five-variate",
                    len_candidate_basis_set = 200,
                    len_final_basis_set = 200,
                    max_rows = 500,
                    max_degree = 5,
                    batch_size = 100,
                    n_batch = 100,
                    p = 0.5,
                    seed = 141098,
                    weight_function = "double weight v3",
                    family = "gaussian",
                    n_cores = 32)

saveRDS(samp_res, "out/samp_var_5_n_1000_double_weight_v3.RDS")
