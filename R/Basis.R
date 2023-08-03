#' Basis class
#'
#' Class to parameterize a basis function with methods to hash and compare.
#'
#' @docType class
#'
#' @importFrom R6 R6Class
#' @importFrom stringr strsplit
#'
#' @export
#'
#' @format \code{\link{R6Class}} object.
#'
#' @section Parameters:
#' - \code{col_indices}: Integer vector of column indices.
#' - \code{knot_points}: Numeric vector of knot points.
#'
Basis <- R6Class("Basis",
  public = list(
    col_indices = NULL,
    knot_points = NULL,
    hash_key = NULL,
    initialize = function(...) {
      arguments <- list(...)
      if (nargs() == 2) {
        # make Basis from column indices and knot points
        sorted_indices <- order(arguments[[1]])
        self$col_indices <- arguments[[1]][sorted_indices]
        self$knot_points <- arguments[[2]][sorted_indices]
      } else if (nargs() == 1) {
        # make Basis from hash key
        parts <- strsplit(arguments[[1]], "_")[[1]]
        self$col_indices <- as.integer(strsplit(parts[1], ",")[[1]])
        self$knot_points <- as.double(strsplit(parts[2], ",")[[1]])
      }
    },
    hash = function() {
      return(paste0(paste(self$col_indices, collapse = ","), "_",
                    paste(self$knot_points, collapse = ",")))
    },
    equal = function(other) {
      return(all(self$col_indices == other$col_indices) &&
             all(self$knot_points == other$knot_points))
    }
  )
)
