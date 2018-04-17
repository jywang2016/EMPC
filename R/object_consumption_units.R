#' Consumption Unit
#'
#' @description
#' Build, set value of the Consumption Unit(i.e. building )
#'
#' @usage
#'
#' model <- consumption$new()
#'
#' model$setvalue(ssmodel,...,x0,name = NA)
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
consumption <- R6::R6Class(classname = "consumption",
                           public = list(
                             parameters = list(
                               #plant parameters
                               disturbance = NA,
                               ssM = list(A = NA, Bu = NA, Bd = NA, C = NA), #ssM
                               continuous = FALSE, #default
                               timestep = NA, #should be numeric
                               x0 = NA, #init state
                               name = list(x = NA, y = NA, u = NA, Dist = NA)
                             ),

                             ifcon = function()
                               iifcon(self),

                             setvalue = function(ssmodel,disturbance,timestep,
                                                 continuous=FALSE,x0,name=NA){
                               isetvalue(self,ssmodel,disturbance,timestep,
                                         continuous,x0,name)
                             },

                             c2d = function(){
                               ic2d(self)
                             }

                           )

                           )


iifcon <- function(self)
{
  if(self$parameters$continuous == F)
  {
    print(paste("the state-space model is discrete"))
  }
  else if(self$parameters$continuous == T)
  {
    print(paste("the state-space model is continuous"))
  }
  else
  {
    warning(paste("The continuous attribute should be bool type"))
  }
}

isetvalue <- function(self,ssmodel,disturbance,timestep,continuous,x0,name)
{
  self$parameters$ssM <- ssmodel
  self$parameters$disturbance <- disturbance
  self$parameters$timestep <- timestep
  self$parameters$continuous <- continuous
  self$parameters$x0 <- x0
  self$parameters$name <- name
}

ic2d <- function(self)
{
  if(self$parameters$continuous == T)
  {
    # Discretize a continuous time state space model:
    # dx/dt = A * x + Bu * u + Bd * d, y = C x, (sampling period Ts)
    require(expm)
    A <- self$parameters$ssM$A
    Bu <- self$parameters$ssM$Bu
    Bd <- self$parameters$ssM$Bd
    C <- self$parameters$ssM$C
    Ts <- self$parameters$timestep
    nx <- nrow(Bu)
    nu <- ncol(Bu)
    nd <- ncol(Bd)
    M  <- rbind(cbind(A,Bu,Bd), matrix(0,nu+nd,nx+nu+nd))
    eM <- expm(M*Ts)
    Ad <- eM[1:nx,1:nx]
    Bud <- eM[1:nx,(nx+1):(nx+nu)]
    Bdd <- eM[1:nx,(nx+nu+1):(nx+nu+nd)]
    self$parameters$ssM <- list(A=Ad,Bu=Bud,Bd=Bdd,C=C)
    self$parameters$continuous <- F
    print("the state-space model has been discretized!")
  }
  else{
    self$ifcon()
  }
}

