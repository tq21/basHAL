source("../utils.R")

rand_res <- run_sim(n = 1000,
                    type = "five-variate",
                    len_candidate_basis_set = 200,
                    len_final_basis_set = 200,
                    max_rows = 500,
                    max_degree = 5,
                    batch_size = 100,
                    n_batch = 100,
                    p = 1,
                    seed = 141098,
                    weight_function = "double weight v3",
                    family = "gaussian",
                    n_cores = 32)

saveRDS(rand_res, "out/rand_var_5_n_1000.RDS")
