#' convert model from continuous to discrete time
#'
#' Takes in state-space model and makes it continuous
#' @param A State matrix of a discreted ssmodel
#' @param B Input matrix of a discreted ssmodel
#' @param Ts Timestep
#'
#' @examples
#' # X(k+1) = A\*X(k) + B\*U(k) discreted ssModel
#' A <- matrix(c(0.51,0.22,0.47,0.78),nrow = 2, byrow = T)
#' B <- matrix(c(0.61,0.83,0.25,0.39),nrow = 2, byrow = T)
#' csys <- d2c(A,B,600)
#' c2d(csys$A,csys$B,600)
#' c2d(csys$A,csys$B,1200)
#'
#' @return A continuous state-space model
#' @importFrom expm logm
#' @export

d2c <- function(A, B, Ts){
  m <- nrow(A)
  n <- ncol(A)
  nb <- ncol(B)

  if (m == 1){
    if(A == 1){
      a <- 0
      b <- B/Ts
    }
  }

  b <- matrix(0,m,nb)
  nz <- 0
  nonzero <- NULL

  for(i in 1:nb){
    if(any(B[,i] != 0)){
      nonzero <- c(nonzero,i)
      nz <- nz + 1
    }
  }

  logmat <- expm::logm(rbind(cbind(A,B[,nonzero]),cbind(matrix(0,nz,n),diag(nz))))
  logmat <- logmat/Ts

  a <- logmat[1:n,1:n]

  if(length(b)){
    b[,nonzero] <- logmat[1:n,(n+1):(n+nz)]
  }

  return(list(A = a, B = b))
}

