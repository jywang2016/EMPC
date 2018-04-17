#' Storage Unit
#'
#' @description
#' Build, set value of the Storage Unit(i.e. building )
#'
#' @usage
#'
#' model <- storage$new()
#'
#' model$setvalue(ssmodel,...,A_cst,B_cst,h_cst)
#'
#' model$ifcon() #if the state-space is continuous or not
#'
#' @format
#' An object of R6Class
#' @examples
#' search `EMPC` on github, find more examples
#' @importFrom R6 R6Class
#' @importFrom expm expm
#' @export
storage <- R6::R6Class(classname = "storage",
                       public = list(
                         parameters = list(ssM = list(A = NA, Bu = NA),
                                           continuous = FALSE,
                                           timestep = NA,
                                           x0 = NA,
                                           xbmax = NA,
                                           xbmin = NA,
                                           ubmax = NA,
                                           ubmin = NA,
                                           storage_type = "simple_battery",
                                           A_cst = NA,
                                           B_cst = NA,
                                           h_cst = NA),
                         ifcon = function(){
                           iifcon(self)
                         },
                         c2d = function(){
                           ic2d(self)
                         },
                         setvalue = function(ssmodel,timestep,continuous,x0,
                                             xbmax = NA,xbmin = NA,ubmax = NA,ubmin = NA,
                                             storage_type = "simple_battery",
                                             A_cst = NA, B_cst = NA, h_cst = NA){
                           iset_value(self,ssmodel,timestep,continuous,x0,
                                      xbmax,xbmin,ubmax,ubmin,storage_type,
                                      A_cst,B_cst,h_cst)
                         }
                         ))

iifcon <- function(self){
  if(self$parameters$continuous == F){
    print(paste("the state-space model is discrete"))
  }else if(self$parameters$continuous == T){
    print(paste("the state-space model is continuous"))
  }else{
    warning(paste("The continuous attribute should be bool type"))
  }
}
ic2d <- function(self){
  if(self$parameters$continuous == T){
    require(expm)
    A <- self$parameters$ssM$A
    B <- self$parameters$ssM$B
    Ts <- self$parameters$timestep
    nx <- nrow(as.data.frame(B))
    nu <- ncol(as.data.frame(B))
    M  <- rbind(cbind(A,B), matrix(0,nu,nx+nu))
    eM <- expm(M*Ts)
    Ad <- eM[1:nx,1:nx]
    Bd <- eM[1:nx,(nx+1):(nx+nu)]
    self$parameters$ssM <- list(A = Ad,
                              B = Bd)
    print("the state-space model has been discretized!")
    self$parameters$continuous <- F
    print(self$parameters$ssM)
  }else{
    self$ifcon()
  }

}

iset_value <- function(self,ssmodel,timestep,continuous,x0,xbmax,xbmin,ubmax,ubmin,
                       storage_type,A_cst,B_cst,h_cst){
  self$parameters$ssM <- ssmodel
  self$parameters$continuous <- continuous
  self$parameters$timestep <- timestep
  self$parameters$x0 <- x0
  self$parameters$xbmax <- xbmax
  self$parameters$xbmin <- xbmin
  self$parameters$ubmax <- ubmax
  self$parameters$ubmin <- ubmin
  self$parameters$storage_type <- storage_type
  self$parameters$A_cst <- A_cst
  self$parameters$B_cst <- B_cst
  self$parameters$h_cst <- h_cst
}

