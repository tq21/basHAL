# count the number of degree 1, 2, ..., g-basis functions in a basis set
count_basis <- function(basis_set) {
  degrees <- sapply(basis_set, function(basis) length(basis[["col_indices"]]))
  degrees <- as.factor(degrees)
  res <- as.data.frame(summary(degrees))
  res <- cbind(res, round(res[,1]/length(basis_set)*100, 2))
  colnames(res) <- c("number of basis functions", "percentage")

  return(res)
}

basis_1 <- Basis$new(c(1,2), c(10,20))
basis_2 <- Basis$new(c(1,4,6), c(30,20,10))
basis_3 <- Basis$new(c(2,5,9,10), c(10,20,10,43))

basis_set <- list(basis_1, basis_2, basis_3)

count_basis(basis_set)
