source("utils.R")

run_sim(n = 500,
        type = "trivariate",
        len_candidate_basis_set = 200,
        len_final_basis_set = 200,
        max_rows = 500,
        max_degree = 3,
        batch_size = 200,
        n_batch = 200,
        p = 0.5,
        seed = 141098,
        weight_function = "double weight v3",
        family = "gaussian",
        n_cores = 32)
