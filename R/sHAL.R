library(purrr)
library(glmnet)
library(tictoc)

sHAL <- R6Class("sHAL",
  public = list(
    X = NULL,
    y = NULL,
    len_candidate_basis_set = NULL,
    len_final_basis_set = NULL,
    max_rows = NULL,
    max_degree = NULL,
    batch_size = 50,
    n_batch = 50,
    alpha = NULL,
    V_folds = 5,
    n_jobs = 5,
    seed = NULL,
    basis_hash_table = NULL,

    initialize = function(X, y, len_candidate_basis_set, len_final_basis_set,
                          max_rows, max_degree, batch_size = 50, n_batch = 50,
                          alpha = NULL, V_folds = 5, n_jobs = 5, seed = NULL) {
      self$X <- X
      self$y <- y
      self$len_candidate_basis_set <- len_candidate_basis_set
      self$len_final_basis_set <- len_final_basis_set
      self$max_rows <- max_rows
      self$max_degree <- max_degree
      self$batch_size <- batch_size
      self$n_batch <- n_batch
      self$alpha <- alpha
      self$V_folds <- V_folds
      self$n_jobs <- n_jobs
      self$seed <- seed
      self$basis_hash_table <- new.env(hash = TRUE)
    },

    generate_basis_set = function() {
      # sample column indices
      col_indices <- map(seq_len(self$len_candidate_basis_set), function(i) {
        sample(seq_len(ncol(self$X)),
               size = sample(seq_len(self$max_degree), size = 1),
               replace = FALSE)
      })

      # sample row index, get knot points
      knot_points <- map(col_indices, function(col_idx) {
        as.numeric(self$X[sample(seq_len(nrow(self$X)), size = 1), col_idx])
      })

      # make candidate basis set
      basis_set <- map2(col_indices, knot_points, function(.x, .y) {
        Basis$new(.x, .y)
      })

      return(basis_set)
    },

    evaluate_candidate = function(basis_set) {
      X_train <- NULL
      X_valid <- NULL
      y_train <- NULL
      y_valid <- NULL

      if (self$max_rows >= nrow(self$X)) {
        basis_matrix <- make_design_matrix(basis_set, self$X)
        train_indices <- sample(x = 1:nrow(basis_matrix),
                                size = floor(0.8 * nrow(basis_matrix)))
        X_train <- basis_matrix[train_indices,]
        X_valid <- basis_matrix[-train_indices,]
        y_train <- self$y[train_indices]
        y_valid <- self$y[-train_indices]
      } else {
        row_indices <- sample(x = 1:nrow(self$X), size = self$max_rows)
        basis_matrix <- make_design_matrix(basis_set, self$X[row_indices, ])
        train_indices <- sample(x = 1:nrow(basis_matrix),
                                size = floor(0.8 * nrow(basis_matrix)))
        X_train <- basis_matrix[train_indices, ]
        X_valid <- basis_matrix[-train_indices, ]
        y_train <- self$y[row_indices][train_indices]
        y_valid <- self$y[row_indices][-train_indices]
      }

      lasso <- cv.glmnet(X_train, y_train, alpha = 1, nfold = self$V_folds)
      preds <- predict(lasso, newx = X_valid, s = lasso$lambda.min)
      loss <- sqrt(mean((y_valid - preds)^2))

      coef <- coef(lasso, s = lasso$lambda.min)[-1]
      non_zero_indices <- which(coef != 0)
      non_zero_basis <- basis_set[non_zero_indices]

      for (basis in non_zero_basis) {
        # hash and compute weight of basis
        hash_key <- basis$hash()
        weight <- 1 / loss

        if (is.null(self$basis_hash_table[[hash_key]])) {
          # new basis, assign weight
          self$basis_hash_table[[hash_key]] <- weight
        } else {
          # existing basis, update weight
          self$basis_hash_table[[hash_key]] <-
            self$basis_hash_table[[hash_key]] + weight
        }
      }
    },

    get_top_k = function(n_sample) {
      hash_keys <- names(self$basis_hash_table)
      loss_vals <- mget(hash_keys, envir = self$basis_hash_table,
                        inherits = FALSE)
      sorted_keys <- hash_keys[order(unlist(loss_vals), decreasing = TRUE)]
      top_keys <- sorted_keys[1:n_sample]
      top_basis_set <- map(top_keys, function(hash_key) Basis$new(hash_key))

      return(top_basis_set)
    },

    fit_sampled_basis_set = function(sampled_basis_set) {
      basis_matrix <- make_design_matrix(sampled_basis_set, self$X)
      cv_fit <- cv.glmnet(basis_matrix, self$y, nfolds = self$V_folds, alpha = 1)

      return(cv_fit)
    },

    run = function(verbose = FALSE, plot = FALSE) {
      dict_length <- vector()

      for (i in seq_len(self$n_batch)) {
        tic()
        dict_length <- c(dict_length, length(self$basis_hash_table))

        if (verbose) {
          message(paste("Batch", i, "/", self$n_batch,
                        ", Dictionary length: ", length(self$basis_hash_table)))
        }

        # generate a batch of candidate basis sets
        basis_sets <- map(seq_len(self$batch_size),
                          function(x) self$generate_basis_set())

        # fit CV Lasso on each basis set
        walk(basis_sets, self$evaluate_candidate)

        toc()

        # normalize weights in dictionary after each batch
        #total_weight <- sum(unlist(eget(self$basis_hash_table)))
        #for (key in names(self$basis_hash_table)) {
        #  assign(key, get(key, envir = self$basis_hash_table) / total_weight,
        #         envir = self$basis_hash_table)
        #}
      }

      sampled_basis_set <- self$get_top_k(self$len_final_basis_set)

      # fit CV Lasso on the sampled basis set
      final_lasso <- self$fit_sampled_basis_set(sampled_basis_set)

      # plot length of dictionary
      if (plot) {
        plot(dict_length, xlab = "Batch", ylab = "Dictionary length", type = 'l')
      }

      return(list(final_lasso, sampled_basis_set))
    }
  )
)
