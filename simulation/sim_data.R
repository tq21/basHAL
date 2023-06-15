library(data.table)

# univariate -------------------------------------------------------------------
# smooth
sim_uni_smooth <- function(n){
  X1 <- runif(n, -4, 4)
  Y <- 0.05*X1-0.42*X1^2+rnorm(n)
  return(data.frame(X1 = X1, Y = Y))
}

# jumps
sim_uni_jump <- function(n){
  X1 <- runif(n, -4, 4)
  Y <- -2.7*as.numeric(X1 < -3)+2.5*as.numeric(X1 > -2)-
    2*as.numeric(X1 > 0)+4*as.numeric(X1 > 2)-3*as.numeric(X1 > 3)+rnorm(n)
  return(data.frame(X1 = X1, Y = Y))
}

# sinusoidal
sim_uni_sin <- function(n){
  X1 <- runif(n, -4, 4)
  Y <- 2*sin(pi/2*abs(X1))+2*cos(pi/2*abs(X1))+rnorm(n)
  return(data.frame(X1 = X1, Y = Y))
}

# trivariate -------------------------------------------------------------------
# smooth
sim_tri_smooth <- function(n){
  X1 <- runif(n, -4, 4)
  X2 <- runif(n, -4, 4)
  X3 <- rbinom(n, 1, 0.5)
  Y <- 0.07*X1-0.28*X1^2+0.5*X2+0.25*X2*X3+rnorm(n)
  return(data.frame(X1 = X1, X2 = X2, X3 = X3, Y = Y))
}

# jumps
sim_tri_jump <- function(n){
  X1 <- runif(n, -4, 4)
  X2 <- runif(n, -4, 4)
  X3 <- rbinom(n, 1, 0.5)
  Y <- -2*as.numeric(X1 <- 3)*X3+2.5*as.numeric(X1 > -2)-
    2*as.numeric(X1 > 0)+2.5*as.numeric(X1 > 2)*X3-2.5*as.numeric(X1 > 3) +
    as.numeric(X2 > -1)-4*as.numeric(X2 > 1)*X3+2*as.numeric(X2 > 3)+rnorm(n)
  return(data.frame(X1 = X1, X2 = X2, X3 = X3, Y = Y))
}

# sinusoidal
sim_tri_sin <- function(n){
  X1 <- runif(n, -4, 4)
  X2 <- runif(n, -4, 4)
  X3 <- rbinom(n, 1, 0.5)
  Y <- 4*X3*as.numeric(X2 < 0)*sin(pi/2*abs(X1))+
    4.1*as.numeric(X2 >= 0)*cos(pi/2*abs(X1))+rnorm(n)
  return(data.frame(X1 = X1, X2 = X2, X3 = X3 , Y = Y))
}

# five-variate  ----------------------------------------------------------------
# smooth
sim_five_smooth <- function(n){
  X1 <- runif(n, -4, 4)
  X2 <- runif(n, -4, 4)
  X3 <- rbinom(n, 1, 0.5)
  X4 <- rnorm(n)
  X5 <- rgamma(n, 2, 1)
  Y <- X1/10-0.3*X1^2+0.25*X2+0.5*X2*X3-0.5*X4+0.04*X5^2-0.1*X5+rnorm(n)
  return(data.frame(X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5, Y = Y))
}

# jumps
sim_five_jump <- function(n){
  X1 <- runif(n, -4, 4)
  X2 <- runif(n, -4, 4)
  X3 <- rbinom(n, 1, 0.5)
  X4 <- rnorm(n)
  X5 <- rgamma(n, 2, 1)
  Y <- -as.numeric(X1 < -3)*X3+0.5*as.numeric(X1 > -2)-as.numeric(X1 > 0)+
    2*as.numeric(X1 > 2)*X3-3*as.numeric(X1 > 3)+1.5*as.numeric(X2 > -1)-
    5*as.numeric(X2 > 1)*X3+2*as.numeric(X2 > 3)+2*as.numeric(X4 < 0)-
    as.numeric(X5 > 5) - as.numeric(X4 < 0)*as.numeric(X1 < 0)+2*X3+rnorm(n)
  return(data.frame(X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5, Y = Y))
}

# sinusoidal
sim_five_sin <- function(n){
  X1 <- runif(n, -4, 4)
  X2 <- runif(n, -4, 4)
  X3 <- rbinom(n, 1, 0.5)
  X4 <- rnorm(n)
  X5 <- rgamma(n, 2, 1)
  Y <- 3.8*X3*as.numeric(X2 < 0)* sin(pi/2*abs(X1))+
    4*as.numeric(X2 > 0)*cos(pi/2*abs(X1))+
    0.1*X5*sin(pi*X4)+X3*cos(abs(X4-X5))+rnorm(n)
  return(data.frame(X1 = X1, X2 = X2, X3 = X3, X4 = X4, X5 = X5, Y = Y))
}
