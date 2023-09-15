library(ggplot2)

`%+%` <- function(a, b) paste0(a, b)


#' Orthonormalize a given basis w.r.t previously selected bases
#'
#' @param basis A \code{Basis} object
#' @param cur_bases A list of \code{Basis} objects for selected bases
#' @param dot_prods A vector of dot products for selected bases to avoid
#' redundant computations
#' @param idx Number of bases selected. Note, this may be different from the
#' length of, e.g., \code{cur_bases}, because we pre-allocate memory for
#' \code{cur_bases}
#'
#' @returns A vector for orthonormalized basis
ortho_basis <- function(basis, cur_bases, dot_prods, idx) {
  tmp <- 0
  for (j in 1:idx) {
    tmp <- tmp+as.numeric(basis%*%cur_basis[[j]])/dot_prods[j]*cur_bases[[j]]
  }

  return(basis - tmp)
}

# count the number of degree 1, 2, ..., g-basis functions in a basis set
# count_basis <- function(basis_set) {
#   degrees <- sapply(basis_set, function(basis) length(basis[["col_indices"]]))
#   degrees <- as.factor(degrees)
#   res <- as.data.frame(summary(degrees))
#   res <- cbind(res, round(res[,1]/length(basis_set)*100, 2))
#   colnames(res) <- c("number of basis functions", "percentage")
#
#   return(res)
# }

# ortho_basis_norm <- function(basis, cur_bases, dot_prods, idx) {
#   tmp <- 0
#   for (j in 1:idx) {
#     tmp <- tmp + dot(basis, cur_bases[[j]]) / dot_prods[j] * cur_bases[[j]]
#   }
#
#   orthoed <- basis - tmp
#
#   return(sqrt(dot(orthoed, orthoed)))
# }

# ortho_basis_norm <- function(basis, cur_bases, dot_prods, idx) {
#   tmp <- 0
#   for (j in 1:idx) {
#     tmp <- tmp + dot(basis, cur_bases[[j]]) / dot_prods[j] * cur_bases[[j]]
#   }
#
#   orthoed <- basis - tmp
#
#   return(sqrt(dot(orthoed, orthoed)))
# }

# basis_1 <- Basis$new(c(1,2), c(10,20))
# basis_2 <- Basis$new(c(1,4,6), c(30,20,10))
# basis_3 <- Basis$new(c(2,5,9,10), c(10,20,10,43))
#
# basis_set <- list(basis_1, basis_2, basis_3)
#
# count_basis(basis_set)
