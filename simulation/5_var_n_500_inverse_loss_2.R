source("utils.R")

run_sim(n = 500,
        type = "five-variate",
        len_candidate_basis_set = 200,
        len_final_basis_set = 200,
        max_rows = 500,
        max_degree = 5,
        batch_size = 500,
        n_batch = 200,
        p = 0.5,
        seed = 141098,
        weight_function = "inverse loss",
        family = "gaussian",
        n_cores = 32)
