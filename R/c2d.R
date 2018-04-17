#' convert model from continuous to discrete time
#'
#' Takes in state-space model and discretizes it
#' @param A State matrix
#' @param B Input matrix
#' @param Ts Timestep
#'
#' @examples
#' A <- matrix(c(0.51,0.22,0.47,0.78),nrow = 2, byrow = T)
#' B <- matrix(c(0.61,0.83,0.25,0.39),nrow = 2, byrow = T)
#' csys <- d2c(A,B,600)
#' c2d(csys$A,csys$B,600)
#' c2d(csys$A,csys$B,1200)
#' @return A discreted state-space model
#' @importFrom expm expm
#' @export
c2d <- function(A,B,Ts){
  nx <- ncol(A)
  nu <- ncol(B)
  mat <- rbind((cbind(A,B)*Ts),matrix(0,nu,nx+nu))
  expmat <-expm::expm(mat)
  Ad <- expmat[1:nx, 1:nx]
  Bd <- expmat[1:nx, (nx+1):(nx+nu)]
  return(list(A = Ad,B = Bd))
}


# test
# A <- matrix(c(0.51,0.22,0.47,0.78),nrow = 2, byrow = T)
# B <- matrix(c(0.61,0.83,0.25,0.39),nrow = 2, byrow = T)
# csys <- d2c(A,B,600)
#
# c2d(csys$A,csys$B,600)
# c2d(csys$A,csys$B,1200)
