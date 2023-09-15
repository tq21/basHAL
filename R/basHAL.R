#' @title Basis adaptive sampling for highly adaptive lasso
#'
#' @description
#' This class implements the basis adaptive sampling algorithm
#' for highly adaptive lasso.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom purrr map map2 walk walk2
#' @importFrom glmnet cv.glmnet
#' @importFrom progress progress_bar
#' @importFrom parallel mclapply
#' @importFrom origami make_folds
#' @importFrom origami folds_montecarlo
#'
#' @export
#'
#' @example
#' # load data
#' data <- mtcars
#'
#' # get X and y
#' y_col_idx <- 1
#' x_col_idx <- setdiff(seq(1, ncol(data)), y_col_idx)
#' X <- as.matrix(data[, x_col_idx])
#' y <- as.matrix(data[, y_col_idx])
#'
#' set.seed(9847)
#'
#' # train-test split
#' indices <- sample(seq_len(nrow(X)), size = 0.2 * nrow(X))
#' X_test <- X[indices,]
#' y_test <- y[indices]
#' X_train <- X[-indices,]
#' y_train <- y[-indices]
#'
#' # initialize basHAL object
#' basHAL_obj <- basHAL$new(X = X_train,
#'                          y = y_train,
#'                          len_candidate_basis_set = nrow(X_train),
#'                          len_final_basis_set = nrow(X_train),
#'                          max_rows = nrow(X_train),
#'                          max_degree = 10,
#'                          batch_size = 50,
#'                          n_batch = 50,
#'                          p = 0.5,
#'                          seed = 29857,
#'                          n_cores = 5,
#'                          cv_loss = TRUE)
#'
#' # run basHAL
#' result <- basHAL_obj$run(verbose = TRUE, plot = FALSE)
#' final_lasso <- result[[1]]
#' final_basis_set <- result[[2]]
#'
#' # generate basis matrix for test data
#' basis_matrix_test <- make_design_matrix(final_basis_set, X_test)
#' pred <- predict(final_lasso, newx = basis_matrix_test)
#' basHAL_rmse <- sqrt(mean((y_test - pred)^2))
#'
#' @return A basHAL object with methods to run the algorithm
#' and extract final basis set.
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Parameters:
#' - \code{X}: Covariate matrix. Rows are observations and columns are
#'   covariates.
#' - \code{y}: Outcome vector. Must be of length equal to the number of
#'   rows in \code{X}.
#' - \code{len_candidate_basis_set}: Number of basis functions \eqn{k}
#'   in each candidate basis set.
#' - \code{len_final_basis_set}: Number of basis functions \eqn{K} in
#'   the final basis set.
#' - \code{max_rows}: Maximum number of rows to sample when fitting
#'   each small model. We recommend setting this to a number smaller than
#'   the number of rows in \code{X} in large data sets for fast computation.
#'   If \code{NULL}, all rows will be used.
#' - \code{max_degree}: Maximum degree of interaction allowed in the basis
#'   functions. By default, \code{max_degree=5}. When the number of covariates
#'   \eqn{d} is smaller than 5, the \code{max_degree} is set to \eqn{d}.
#' - \code{batch_size}: Number of candidate basis sets to generate
#'   in each iteration.
#' - \code{n_batch}: Total number of iterations to run.
#' - \code{p}: Proportion of candidate basis sets in each batch that
#'   will be sampled from the updated (posterior) sampling distribution.
#'   The rest will be sampled from the prior distribution.
#' - \code{V_folds}: Number of folds to split the data to in cross-validations
#'   when evaluating candidate basis sets.
#' - \code{n_cores}: Number of cpu cores to parallel across.
#' - \code{seed}: Seed.
basHAL <- R6Class("basHAL",
  public = list(
    X = NULL, # covariate matrix X
    y = NULL, # outcome vector Y
    len_candidate_basis_set = NULL, # number of basis functions in each candidate basis set
    len_final_basis_set = NULL, # number of basis functions in the final basis set
    max_rows = NULL, # maximum number of rows use to fit small models
    max_degree = NULL, # maximum basis degree of interaction
    batch_size = 50, # number of candidates to generate in each iteration
    n_batch = 50, # number of iterations
    p = 0.5, # epsilon-greedy, epsilon proportion of candidate basis sets in each batch will be greedy
    V_folds = 5, # V-fold cross validation
    n_cores = 1, # number of cores to parallel across
    seed = NULL, # seed
    basis_hash_table = NULL,
    probs_keys = NULL,
    probs = NULL,
    family = "gaussian",
    best_loss = Inf,
    best_basis_set = NULL,
    best_loss_batch = NULL,
    loss_prop = 0.5,
    cv_loss = FALSE,
    small_fit = "glmnet",
    top_K_losses = NULL,
    avg_losses = NULL,
    final_basis_set = NULL,
    final_lasso_fit = NULL,
    method = NULL,
    selected_basis_set = NULL,
    S = NULL,

    initialize = function(X, y, len_candidate_basis_set, len_final_basis_set,
                          max_rows, max_degree, batch_size = 50, n_batch = 50,
                          p = 0.5, V_folds = 5, n_cores = 1, seed = NULL,
                          family = "gaussian", method = NULL,
                          top_K_losses = NULL, best_loss = Inf, best_basis_set = NULL, best_loss_batch = NULL,
                          loss_prop = 0.5, cv_loss = FALSE,
                          small_fit = "glmnet", avg_losses = NULL, final_basis_set = NULL, final_lasso_fit = NULL) {
      self$X <- X
      self$y <- y
      self$len_final_basis_set <- len_final_basis_set
      self$max_rows <- max_rows
      self$max_degree <- max_degree
      self$batch_size <- batch_size
      self$n_batch <- n_batch
      self$p <- p
      self$V_folds <- V_folds
      self$n_cores <- n_cores
      self$seed <- seed
      self$basis_hash_table <- new.env(hash = TRUE)
      self$probs_keys <- NULL
      self$probs <- NULL
      self$family <- family
      self$top_K_losses <- top_K_losses
      self$best_loss <- best_loss
      self$best_basis_set <- best_basis_set
      self$best_loss_batch <- best_loss_batch
      self$loss_prop <- loss_prop
      self$cv_loss <- cv_loss
      self$avg_losses <- avg_losses
      self$final_basis_set <- final_basis_set
      self$final_lasso_fit <- final_lasso_fit
      self$method <- method

      self$S <- list(bases = vector(mode = "list", length = len_final_basis_set),
                     bases_evaluated = vector(mode = "list", length = len_final_basis_set),
                     dot_prods = vector(length = len_final_basis_set))

      if (method == "univariate glm") {
        # univariate regression method to screen basis functions
        self$len_candidate_basis_set <- 1
        self$small_fit <- "glm"
      } else if (method == "supermass") {
        # Super-fast Maximal Angle Subset Selection
        # first: univariate regression to create sampling distribution
        # second: adaptively fining maximal angle to the current subset
        self$len_candidate_basis_set <- 1
        self$small_fit <- "glm"
      } else if (method == "k-variate glmnet") {
        self$len_candidate_basis_set <- len_candidate_basis_set
        self$small_fit <- "glmnet"
      } else if (method == "k-variate glm") {
        # TODO: NOT WORKING AT THE MOMENT
      }
    },

    #' @description
    #' Randomly draw without replacement to form a candidate basis set.
    #'
    #' @return A list of \code{Basis} objects.
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

    #' @description
    #' Draw without replacement from the updated sampling distribution of
    #' basis functions to form a candidate basis set.
    #' from the current basis sampling distribution
    #'
    #' @return A list of \code{Basis} objects.
    sample_basis_set = function() {
      # sample basis keys
      basis_keys <- sample(self$probs_keys,
                           size = self$len_candidate_basis_set,
                           replace = FALSE, prob = self$probs)
      sampled_basis_set <- map(basis_keys, function(hash_key) Basis$new(hash_key))
    },

    #' @description
    #' Update the weights of the given basis functions.
    #'
    #' @param bases A list of \code{Basis} objects to update weights for.
    #' @param weights A vector of weights to update the basis functions with.
    update_weight = function(bases, weights) {
      walk2(bases, weights, function(basis, weight) {
        # get hash key
        hash_key <- basis$hash()

        if (is.null(self$basis_hash_table[[hash_key]])) {
          # new basis, assign weight
          self$basis_hash_table[[hash_key]] <- weight
        } else {
          # existing basis, update weight
          self$basis_hash_table[[hash_key]] <- self$basis_hash_table[[hash_key]] + weight
        }
      })
    },

    #' @description
    #' Fit a cross-validated lasso on the training set,
    #' predict using the validation set.
    #'
    #' @param X_train Training set design matrix.
    #' @param y_train Training set response vector.
    #' @param X_valid Validation set design matrix.
    #'
    #' @return A list of predictions, coefficients,
    #' and indices of bases with non-zero coefficients.
    fit_small_glmnet = function(X_train, y_train, X_valid) {
      lasso_mod <- cv.glmnet(X_train, y_train, alpha = 1,
                             nfold = self$V_folds, family = self$family,
                             standardize = FALSE, lambda.min.ratio = 1e-4)
      y_pred <- predict(lasso_mod, newx = X_valid, s = lasso_mod$lambda.min, type = "response")

      # get basis with non-zero coefficients, need to update their weights later
      coefs <- coef(lasso_mod, s = lasso_mod$lambda.min)[-1] # exclude intercept
      non_zero_basis_idx <- which(coefs != 0)

      return(list(y_pred, coefs, non_zero_basis_idx))
    },

    #' @description
    #' Fit a glm on the training set,
    #' predict using the validation set.
    #'
    #' @todo THIS FUNCTION IS NOT WORKING AT THE MOMENT!!!
    fit_small_glm = function(X_train, y_train, X_valid) {
      data <- data.frame(x = X_train, y = y_train)
      glm_mod <- glm(y ~ x, data = data, family = self$family)
      y_pred <- predict(glm_mod, newdata = data.frame(x = X_valid), type = "response")
      coefs <- coef(glm_mod)[-1]
      coefs[is.na(coefs)] <- 0

      # get top 10% of coefficients
      #ranked_indices <- order(abs(coefs), decreasing = TRUE)
      #top_indices <- ranked_indices[1:ceiling(0.1 * length(coefs))]
      #coefs <- coefs[top_indices]

      basis_idx <- which(coefs != 0)

      return(list(y_pred, coefs, basis_idx))
    },

    #' @description
    #' Evaluate the performance of the given candidate basis set.
    #'
    #' @param basis_set A list of \code{Basis} objects to evaluate.
    #'
    #' @return A list of bases indices, calculated weights, and loss.
    evaluate_candidate = function(basis_set, basis_evaluated=FALSE) {
      # either fit on full data or sampled data
      row_indices <- NULL
      if (self$max_rows >= nrow(self$X)) {
        row_indices <- seq_len(nrow(self$X))
      } else {
        row_indices <- sample(x = 1:nrow(self$X), size = self$max_rows)
      }

      # make design matrix
      basis_matrix <- NULL
      if (basis_evaluated) {
        basis_matrix <- as.matrix(basis_set)
      } else {
        basis_matrix <- make_design_matrix(basis_set,
                                           as.matrix(self$X[row_indices, ]))
      }

      if (all(is.nan(basis_matrix))) {
        return(list(basis_set, 0))
      }

      # initialize variables
      loss <- NULL # CV loss of the candidate basis set
      coefs <- NULL # small model coefficients (TODO: UPCOMING FEATURE)
      basis_idx <- NULL # indices of basis to update weights for

      if (self$cv_loss) {
        # perform V-fold cross-validation to obtain CV loss
        strata_ids <- NULL
        if (self$family == "binomial") {
          strata_ids <- self$y
        }
        folds <- make_folds(n = nrow(basis_matrix), V = self$V_folds, strata_ids = strata_ids)
        fold_res <- map(folds, function(.x) {
          train_indices <- .x$training_set
          valid_indices <- .x$validation_set
          X_train <- basis_matrix[train_indices, ]
          X_valid <- basis_matrix[valid_indices, ]
          y_train <- self$y[row_indices][train_indices]
          y_valid <- self$y[row_indices][valid_indices]

          # fit small model on training set
          fold_fit_res <- NULL
          if (self$small_fit == "glm") {
            suppressWarnings(fold_fit_res <- self$fit_small_glm(X_train, y_train, X_valid))
          } else if (self$small_fit == "glmnet") {
            suppressWarnings(fold_fit_res <- self$fit_small_glmnet(X_train, y_train, X_valid))
          }

          fold_y_pred <- fold_fit_res[[1]]
          fold_non_zero_basis_idx <- fold_fit_res[[3]]
          fold_loss <- get_loss(fold_y_pred, y_valid, self$family)

          return(list(fold_loss, fold_non_zero_basis_idx))
        })

        loss <- mean(unlist(map(fold_res, function(.x) .x[[1]])))
        basis_idx <- unique(unlist(map(fold_res, function(.x) .x[[2]])))

      } else {
        # perform 1-fold cross-validation to obtain CV loss
        strata_ids <- NULL
        if (self$family == "binomial") {
          strata_ids <- self$y
        }
        folds <- make_folds(n = nrow(basis_matrix), V = 1,
                            fold_fun = folds_montecarlo,
                            strata_ids = strata_ids)
        train_indices <- folds[[1]]$training_set
        valid_indices <- folds[[1]]$validation_set
        X_train <- basis_matrix[train_indices, ]
        X_valid <- basis_matrix[valid_indices, ]
        y_train <- self$y[row_indices][train_indices]
        y_valid <- self$y[row_indices][valid_indices]

        # fit small model on training set
        fit_res <- NULL
        if (self$small_fit == "glm") {
          suppressWarnings(fit_res <- self$fit_small_glm(X_train, y_train, X_valid))
        } else if (self$small_fit == "glmnet") {
          suppressWarnings(fit_res <- self$fit_small_glmnet(X_train, y_train, X_valid))
        }
        y_pred <- fit_res[[1]]
        coefs <- fit_res[[2]]
        basis_idx <- fit_res[[3]]
        loss <- get_loss(y_pred, y_valid, self$family)
      }

      # calculate weights
      weights <- get_weights(weight_fun = self$method,
                             loss = loss,
                             base_loss = get_loss(ifelse(self$family == "binomial", 0.5, mean(self$y)),
                                                  self$y,
                                                  self$family),
                             coefs = coefs,
                             num_non_zero = length(basis_idx),
                             total_num = self$len_candidate_basis_set,
                             loss_prop = self$loss_prop)

      return(list(basis_set[basis_idx], weights, loss))
    },

    #' @description
    #' Select the K bases with the highest sampling probability
    #' to form the final basis set.
    #'
    #' @param n_sample The number of bases to select.
    #'
    #' @return A list of \code{Basis} objects.
    get_top_K = function(n_sample) {
      hash_keys <- names(self$basis_hash_table)
      loss_vals <- mget(hash_keys, envir = self$basis_hash_table,
                        inherits = FALSE)
      sorted_keys <- hash_keys[order(unlist(loss_vals), decreasing = TRUE)]
      top_keys <- sorted_keys[1:ifelse(length(sorted_keys) < n_sample,
                                       length(sorted_keys), n_sample)]
      top_basis_set <- map(top_keys, function(hash_key) Basis$new(hash_key))

      return(top_basis_set)
    },

    #' @description
    #' Fit a cross-validated lasso on the final basis set.
    #'
    #' @param final_basis_set The final basis set selected.
    #'
    #' @return A fitted \code{glmnet} object.
    fit_final_basis_set = function(final_basis_set) {
      basis_matrix <- make_design_matrix(final_basis_set, self$X)
      fit <- cv.glmnet(basis_matrix, self$y, nfolds = self$V_folds, alpha = 1,
                       standardize = FALSE,
                       lambda.min.ratio = 1e-4,
                       family = self$family, nlambda = 1000)

      return(fit)
    },

    #' @description
    #' Get the cross-validated loss of a given candidate basis set.
    #'
    #' @todo THIS FUNCTION IS FOR TESTING PURPOSES ONLY.
    #'
    #' @param basis_set A \code{Basis} object for the candidate basis set.
    #'
    #' @return The cross-validated loss of the candidate basis set.
    get_cv_loss = function(basis_set) {

      basis_matrix <- make_design_matrix(basis_set, self$X)

      strata_ids <- NULL
      if (self$family == "binomial") {
        strata_ids <- self$y
      }
      folds <- make_folds(n = nrow(basis_matrix), V = self$V_folds, strata_ids = strata_ids)
      losses <- map(folds, function(.x) {
        train_indices <- .x$training_set
        valid_indices <- .x$validation_set
        X_train <- basis_matrix[train_indices, ]
        X_valid <- basis_matrix[valid_indices, ]
        y_train <- self$y[train_indices]
        y_valid <- self$y[valid_indices]

        cv_fit <- cv.glmnet(X_train, y_train, nfolds = self$V_folds, alpha = 1,
                            standardize = FALSE,
                            lambda.min.ratio = 1e-4,
                            family = self$family)
        y_pred <- predict(cv_fit, newx = X_valid, s = cv_fit$lambda.min, type = "response")

        return(get_loss(y_pred, y_valid, self$family))
      })

      cv_loss <- mean(unlist(losses))

      return(cv_loss)
    },

    predict = function(newx=NULL, type, ...) {
      # TODO: assumes users input raw data, not a HAL design matrix
      # add feature later to check input new data type
      # add relaxed HAL feature
      if (is.null(newx)) {
        # predict on training
         return(predict(self$final_lasso_fit, s = "lambda.min", type = type, ...))
      } else {
        # predict on new data
        # TODO: check newx columns
        basis_matrix_newx <- make_design_matrix(self$final_basis_set, newx)
        return(predict(self$final_lasso_fit, newx = basis_matrix_newx, s = "lambda.min", type = type, ...))
      }
    },

    #' @description
    #' Run the algorithm.
    #'
    #' @param verbose Whether to print progress.
    #' @param plot Whether to plot the rate at which the number of bases
    #' explored increases. (FOR TESTING PURPOSES ONLY AT THE MOMENT,
    #' LATER INCORPORATE INTO PROGRESS BAR)
    run = function(verbose = FALSE, plot = FALSE) {
      dict_length <- vector()

      pb <- progress_bar$new(format = "[:bar] Batch: :current of :total, time elapsed: :elapsedfull",
                             total = ifelse(self$method == "supermass",
                                            self$n_batch + self$len_final_basis_set,
                                            self$n_batch))

      for (i in seq_len(self$n_batch)) {
        dict_length <- c(dict_length, length(self$basis_hash_table))

        if (verbose) pb$tick()
        #print("iteration: " %+% i)

        # generate random candidate basis sets
        n_random <- ifelse(is.null(self$probs) | self$method == "univariate glm" | self$method == "supermass",
                           self$batch_size,
                           round(self$batch_size * self$p))
        random_basis_sets <- map(seq_len(n_random),
                                 function(x) self$generate_basis_set())

        # generate candidate basis sets from sampling distribution
        n_sampling <- self$batch_size - n_random
        sampled_basis_sets <- map(seq_len(n_sampling),
                                  function(x) self$sample_basis_set())

        # evaluate candidate basis sets in parallel
        eval_res <- mclapply(c(random_basis_sets, sampled_basis_sets),
                             self$evaluate_candidate, mc.cores = self$n_cores)

        # update basis weights
        walk(eval_res, function(.x) {
          self$update_weight(.x[[1]], .x[[2]])
        })

        if (self$method == "k-variate glmnet") {
          # get basis keys and their sampling distribution
          self$probs_keys <- names(self$basis_hash_table)
          weights <- unlist(mget(self$probs_keys, envir = self$basis_hash_table))

          # normalize weights to 0-1
          weights <- (weights - min(weights)) / (max(weights) - min(weights))
          self$probs <- weights / sum(weights)
        }
      }

      if (self$method == "univariate glm" | self$method == "supermass") {
        # if univariate glm, only need to get sampling probability after the last iteration
        self$probs_keys <- names(self$basis_hash_table)
        weights <- unlist(mget(self$probs_keys, envir = self$basis_hash_table))

        # normalize weights to 0-1
        weights <- (weights - min(weights)) / (max(weights) - min(weights))
        self$probs <- weights / sum(weights)

        if (self$method == "univariate glm") {
          # get final basis set
          self$final_basis_set <- self$get_top_K(self$len_final_basis_set)
        }
      }

      if (self$method == "supermass") {
        # adaptively finding the basis with maximal angle to current subset
        # add 1st place to subset
        pb$tick()
        self$S$bases[[1]] <- self$get_top_K(1)[[1]]
        self$S$bases_evaluated[[1]] <- as.vector(evaluate_zero_order_basis(self$S$bases[[1]], self$X))
        self$S$dot_prods[1] <- as.numeric( # store dot products to avoid redundancy
          self$S$bases_evaluated[[1]]%*%self$S$bases_evaluated[[1]])

        for (i in 2:self$len_final_basis_set) {
          if (verbose) pb$tick()
          #print("iteration: " %+% i)
          # sample bases from posterior distribution
          sampled_bases <- map(seq_len(self$batch_size),
                               function(x) self$sample_basis_set())
          sampled_bases_evaluated <- mclapply(sampled_bases, function(basis) {
            return(evaluate_zero_order_basis(basis[[1]], self$X))
          }, mc.cores = self$n_cores)

          # project each basis onto the current subset, regress Y on each orthogonalized basis
          sampled_bases_evals <- mclapply(sampled_bases_evaluated, function(basis_evaluated) {
            orthoed <- ortho_basis(basis_evaluated, self$S$bases_evaluated, self$S$dot_prods, i-1)
            return(self$evaluate_candidate(orthoed, basis_evaluated = TRUE))
          }, mc.cores = self$n_cores)

          sampled_bases_weight <- map(sampled_bases_evals, function(.x) {
            return(.x[[2]])
          })

          # add the basis with largest weight to the subset
          max_idx <- which.max(sampled_bases_weight)
          self$S$bases[[i]] <- sampled_bases[[max_idx]][[1]]
          self$S$bases_evaluated[[i]] <- sampled_bases_evaluated[[max_idx]]
          self$S$dot_prods[i] <- as.numeric(
            self$S$bases_evaluated[[i]]%*%self$S$bases_evaluated[[i]])
        }

        self$final_basis_set <- self$S$bases
      }

      # fit CV Lasso on the sampled basis set
      self$final_lasso_fit <- self$fit_final_basis_set(self$final_basis_set)

      # get selected bases (bases with non-zero coefficients)
      self$selected_basis_set <- self$final_basis_set[coef(self$final_lasso_fit)[-1] != 0]

      # plot length of dictionary
      if (plot) {
        plot(dict_length, xlab = "Batch", ylab = "Dictionary length", type = 'l')
      }

      return(invisible(NULL))
    }
  )
)
