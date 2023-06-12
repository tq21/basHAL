library(profvis)

example_run <- function(fpath,
                        y_col_idx,
                        len_candidate_basis_set,
                        len_final_basis_set,
                        max_rows,
                        max_degree,
                        batch_size,
                        n_batch,
                        p,
                        seed,
                        weight_function,
                        family = "gaussian") {
  # load data
  dt <- read.csv(fpath)
  dt <- drop_na(dt)
  x_col_idx <- setdiff(seq_len(ncol(dt)), y_col_idx)

  # run sHAL
  sHAL_obj <- sHAL$new(X = dt[, x_col_idx],
                       y = dt[, y_col_idx],
                       len_candidate_basis_set = len_candidate_basis_set,
                       len_final_basis_set = len_final_basis_set,
                       max_rows = max_rows,
                       max_degree = max_degree,
                       batch_size = batch_size,
                       n_batch = n_batch,
                       p = p,
                       seed = seed,
                       weight_function = weight_function,
                       family = family)
  sHAL_res <- sHAL_obj$run(verbose = TRUE, plot = FALSE)
}

profvis({
  cpu_double_weight_v3 <- example_run(fpath = "data/small_data/cpu.csv",
                                      y_col_idx = 1,
                                      len_candidate_basis_set = 209,
                                      len_final_basis_set = 209,
                                      max_rows = 209,
                                      max_degree = 6,
                                      batch_size = 20,
                                      n_batch = 50,
                                      p = 0.5,
                                      seed = 12941,
                                      weight_function = "double weight v3")
})

cpu_double_weight_v3 <- example_run(fpath = "data/small_data/cpu.csv",
                                    y_col_idx = 1,
                                    len_candidate_basis_set = 209,
                                    len_final_basis_set = 209,
                                    max_rows = 209,
                                    max_degree = 6,
                                    batch_size = 20,
                                    n_batch = 50,
                                    p = 0.5,
                                    seed = 12941,
                                    weight_function = "double weight v3")






fpath = "data/small_data/cpu.csv"
y_col_idx = 1
X = dt[, x_col_idx]
y = dt[, y_col_idx]
len_candidate_basis_set = 209
len_final_basis_set = 209
max_rows = 209
max_degree = 6
batch_size = 20
n_batch = 50
p = 0.5
seed = 12941
weight_function = "double weight v3"
V_folds <- 10
n_jobs <- 5

self <- list()
self$X <- X
self$y <- y
self$len_candidate_basis_set <- len_candidate_basis_set
self$len_final_basis_set <- len_final_basis_set
self$max_rows <- max_rows
self$max_degree <- max_degree
self$batch_size <- batch_size
self$n_batch <- n_batch
self$p <- p
self$alpha <- alpha
self$V_folds <- V_folds
self$n_jobs <- n_jobs
self$seed <- seed
self$weight_function <- weight_function

self$basis_hash_table <- new.env(hash = TRUE)
self$probs_keys <- NULL
self$probs <- NULL
self$family <- "gaussian"


self$generate_basis_set <- generate_basis_set
self$evaluate_candidate <- evaluate_candidate
self$sample_basis_set <- sample_basis_set
self$update_weight <- update_weight











