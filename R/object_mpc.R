#' mpc controller
#' @description
#' solve mpc for building climate control or "cost-optimal" control
#' @note
#' If your focus is energyhub which contains building unit, production or storage unit
#' , ehubmpc object maybe a better choice. mpc only supports building unit without other units involved.
#' Besides, time-variant constraints, such as temperature setpoints and range, electricity cost,
#' equipment power (heating/cooling power) are supported.
#'
#' @examples
#' #model <- mpc$new()
#' #model$initialize()
#' #model$set_parameters(N,Tsim,obj,cost,ymin,ymax,yref,ECR,umax,umin)
#' #model$print_para()
#' #model$set_mpc_constraint()
#' #model$solve_mpc(control = ecos.control())
#'
#' @param
#' N prediction horizon
#' @param
#' Tsim simulation horizon; Tsim >= N
#' @param
#' obj control purpose; obj should be one of "cost" or "comfort". "cost" means "cost-optimal"
#' econimic mpc while "comfort" means temperature/humidity control. Different value respresents
#' different objective in linear programming
#' @param
#' cost electricity price; it should be a matrix of electricity price whether the price is changed
#' or not
#' @param
#' ymin/ymax lower/upper limit of temperature/humidity; a matrix
#' @param
#' yref reference y, i.e. temperature setpoint; a matrix
#' @param
#' ECR penalty factor for slack variable s; default 1e6
#' @param
#' umax/umin lower/upper limits of heating/cooling power
#' @param
#' control ecos.control; control parameters for ECOS solver. More details can be seen in ECOSolver
#' package
#' @import R6
#' @import CVXR
#' @import ECOSolveR
#' @export
mpc <- R6::R6Class(classname = "mpc",
                   public = list(
                     parameters = list(
                       N = NA, #prediction horizon
                       Tsim = NA, #control horizon
                       obj = "cost",
                       cost = NA,
                       ymin = NA,
                       ymax = NA,
                       yref = NA,
                       ECR = NA,
                       umax = NA,
                       umin = NA
                     ),
                     building = NA,
                     initialize = function(){
                       self$building <- consumption$new()
                     },
                     set_parameters = function(N,Tsim,obj = self$parameters$obj,cost,ymin,ymax,yref,
                                               ECR,umax,umin){
                       iset_para(self,N,Tsim,obj,cost,ymin,ymax,yref,ECR,umax,umin)
                     },

                     print_para = function(){
                       iprint_para(self)
                     },

                     set_mpc_constraint = function(){
                       if(self$parameters$obj =="comfort"){
                         iset_constraint_comf(self,private)
                       }else if(self$parameters$obj == "cost"){
                         iset_constraint_cost(self,private)
                       }else{
                         warning("check your *obj*: cost or comfort")
                       }
                     },

                     solve_mpc = function(control = ecos.control()){
                       if(self$parameters$obj =="comfort"){
                           isolve_mpc_comf(self,private,control)
                       }else if(self$parameters$obj == "cost"){
                         isolve_mpc_cost(self,private,control)
                       }else{
                         warning("check your *obj*: cost or comfort")
                       }
                     }
                   ),
                   private = list(
                     prob_data = NA
                   ))

isolve_mpc_comf <- function(self,private,control){
  prob_data <- private$prob_data
  solver_output <- ECOS_csolve(c = prob_data[["c"]],
                               G = prob_data[["G"]],
                               h = prob_data[["h"]],
                               dims = prob_data[["dims"]],
                               A = prob_data[["A"]],
                               b = prob_data[["b"]])
  #print(paste("Iteration",1,solver_output$infostring))
  ssM <- self$building$parameters$ssM
  N <- self$parameters$N
  Dist <- t(self$building$parameters$disturbance)

  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  Tsim <- self$parameters$Tsim
  U <- matrix(rep(0,nu*Tsim),nrow= nu)
  Y <- matrix(rep(0,ny*Tsim),nrow = ny)

  #cost <- self$parameters$cost
  yref <- self$parameters$yref
  ymax <- self$parameters$ymax
  ymin <- self$parameters$ymin
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  U[,1] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
  Y[,1] <- solver_output$x[(nx*N + 1):(nx*N + ny)]

  x0 <- solver_output$x[(nx+1):(nx*2)]
  nh <- length(prob_data$h)
  yrefN <- yref[,1:N]
  nyrefN <- length(yrefN)
  if(yrefN[nyrefN] != 0)
  {
    yrefactor <- prob_data$h[nh]/yrefN[nyrefN]
  }else
  {
    yrefactor <- -2
  }


  for (i in 2:Tsim)
  {

    #costN <- cost[i:(N+i-1),]
    yminN <- ymin[,i:(N+i-1)]
    ymaxN <- ymax[,i:(N+i-1)]
    yrefN <- yref[,i:(N+i-1)]
    umaxN <- umax[,i:(N+i-1)]
    uminN <- umin[,i:(N+i-1)]

    prob_data$h[1:(ny*N)] <- as.vector(yminN) * (-1) #modify yminN
    prob_data$h[(ny*N+1):(ny*N+ny*N)] <- as.vector(ymaxN) #modify ymaxN
    prob_data$h[(nh+1-ny*N):nh] <- as.vector(yrefN)*yrefactor #yref
    #prob_data$c[(nx*N+ny*N+1):(nx*N+ny*N+nu*N)] <- as.vector(t(costN)*nu) #modify cost
    prob_data$h[(2*ny*N+1):(2*ny*N + nu*N)] <- as.vector(umaxN) #umax
    prob_data$h[(2*ny*N+nu*N+1):(2*ny*N + 2*nu*N)] <- as.vector(uminN)*(-1) #umin

    prob_data$b[1:nx] <- x0 # modify x0
    prob_data$b[(nx+1):(nx*N)] <- as.vector(Bd %*% Dist[,i:(N+i-2)])

    solver_output <- ECOS_csolve(c = prob_data[["c"]],
                                 G = prob_data[["G"]],
                                 h = prob_data[["h"]],
                                 dims = prob_data[["dims"]],
                                 A = prob_data[["A"]],
                                 b = prob_data[["b"]],
                                 control = control)

    #print(paste("Iteration",i,solver_output$infostring))
    U[,i] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
    Y[,i] <- solver_output$x[(nx*N + 1):(nx*N + ny)]
    x0 <- solver_output$x[(nx+1):(nx*2)]
  }
  list(U = U, Y = Y)
}

isolve_mpc_cost <- function(self,private,control){
  prob_data <- private$prob_data
  solver_output <- ECOS_csolve(c = prob_data[["c"]],
                               G = prob_data[["G"]],
                               h = prob_data[["h"]],
                               dims = prob_data[["dims"]],
                               A = prob_data[["A"]],
                               b = prob_data[["b"]])
  #print(paste("Iteration",1,solver_output$infostring))
  ssM <- self$building$parameters$ssM
  N <- self$parameters$N
  Dist <- t(self$building$parameters$disturbance)

  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  Tsim <- self$parameters$Tsim
  U <- matrix(rep(0,nu*Tsim),nrow= nu)
  Y <- matrix(rep(0,ny*Tsim),nrow = ny)

  cost <- self$parameters$cost
  ymax <- self$parameters$ymax
  ymin <- self$parameters$ymin
  #yref <- self$parameters$yref
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  U[,1] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
  Y[,1] <- solver_output$x[(nx*N + 1):(nx*N + ny)]

  x0 <- solver_output$x[(nx+1):(nx*2)]
  costN <- cost[1:N,]
  if(as.vector(t(costN))[length(costN)] != 0){
    costfactor <- prob_data$c[(nx*N+ny*N+nu*N)]/as.vector(t(costN))[length(costN)]
  }else{
    costfactor <- nu
  }


  for (i in 2:Tsim)
  {
    #costfactor <- prob_data$c[(nx*N+ny*N+nu*N)]/as.vector(t(costN))[length(costN)]

    costN <- cost[i:(N+i-1),]
    yminN <- ymin[,i:(N+i-1)]
    ymaxN <- ymax[,i:(N+i-1)]
    #yrefN <- yref[,i:(N+i-1)]
    uminN <- umin[,i:(N+i-1)]
    umaxN <- umax[,i:(N+i-1)]

    prob_data$h[1:(ny*N)] <- as.vector(yminN) * (-1) #modify yminN
    prob_data$h[(ny*N+1):(ny*N+ny*N)] <- as.vector(ymaxN) #modify ymaxN
    prob_data$c[(nx*N+ny*N+1):(nx*N+ny*N+nu*N)] <- as.vector(t(costN)*costfactor) #modify costN
    prob_data$h[(2*ny*N+1):(2*ny*N + nu*N)] <- as.vector(umaxN) #umaxN
    prob_data$h[(2*ny*N+nu*N+1):(2*ny*N + 2*nu*N)] <- as.vector(uminN)*(-1) #uminN
    prob_data$b[1:nx] <- x0 # modify x0
    prob_data$b[(nx+1):(nx*N)] <- as.vector(Bd %*% Dist[,i:(N+i-2)])

    solver_output <- ECOS_csolve(c = prob_data[["c"]],
                                 G = prob_data[["G"]],
                                 h = prob_data[["h"]],
                                 dims = prob_data[["dims"]],
                                 A = prob_data[["A"]],
                                 b = prob_data[["b"]],
                                 control = control)

    #print(paste("Iteration",i,solver_output$infostring))
    U[,i] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
    Y[,i] <- solver_output$x[(nx*N + 1):(nx*N + ny)]
    x0 <- solver_output$x[(nx+1):(nx*2)]
  }
  list(U = U, Y = Y)
}

iset_constraint_comf <- function(self,private){
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM
  N <- self$parameters$N
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C

  Dist <- t(self$building$parameters$disturbance)

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  yref <- self$parameters$yref

  #umax <- matrix(rep(umax,nu*N),nrow = nu)
  #umin <- matrix(rep(umin,nu*N),nrow = nu)
  ymax <- self$parameters$ymax
  ymin <- self$parameters$ymin

  x <- Variable(rows = nx, cols = N)
  y <- Variable(rows = ny, cols = N)
  u <- Variable(rows = nu, cols = N)
  s <- Variable(rows = ny, cols = N)

  ECR <- self$parameters$ECR

  yminN <- ymin[,1:N]
  ymaxN <- ymax[,1:N]
  yrefN <- yref[,1:N]
  umaxN <- umax[,1:N]
  uminN <- umin[,1:N]

  obj <- Minimize(sum_squares(y-yrefN))#delete +sum(s*ECR)

  constraints <- list(y >= yminN-s,
                      y <= ymaxN+s,
                      u <= umaxN,
                      u >= uminN,
                      x[,1]== x0,
                      x[,2:N] == A %*% x[,1:(N-1)] + Bu %*% u[,1:(N-1)] + Bd %*% Dist[,1:(N+1-2)],
                      y == C %*% x,
                      s >= 0)

  prob <- Problem(obj,constraints)
  private$prob_data <- get_problem_data(prob,solver = "ECOS")
}

iset_constraint_cost <- function(self,private){
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM
  N <- self$parameters$N
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C

  Dist <- t(self$building$parameters$disturbance)

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  #umax <- matrix(rep(umax,nu*N),nrow = nu)
  #umin <- matrix(rep(umin,nu*N),nrow = nu)

  x <- Variable(rows = nx, cols = N)
  y <- Variable(rows = ny, cols = N)
  u <- Variable(rows = nu, cols = N)
  s <- Variable(rows = ny, cols = N)

  ymax <- self$parameters$ymax
  cost <- self$parameters$cost
  ymin <- self$parameters$ymin

  costN <- cost[1:N,]
  ymaxN <- ymax[,1:N]
  yminN <- ymin[,1:N]
  yrefN <- yref[,1:N]
  umaxN <- umax[,1:N]
  uminN <- umin[,1:N]

  ECR <- self$parameters$ECR
  obj <- Minimize(sum(s*ECR) + sum(u %*% costN))

  constraints <- list(y >= yminN-s,
                      y <= ymaxN+s,
                      u <= umaxN,
                      u >= uminN,
                      x[,1]== x0,
                      x[,2:N] == A %*% x[,1:(N-1)] + Bu %*% u[,1:(N-1)] + Bd %*% Dist[,1:(N+1-2)],
                      y == C %*% x,
                      s >= 0)

  prob <- Problem(obj,constraints)
  private$prob_data <- get_problem_data(prob,solver = "ECOS")
}

iprint_para <- function(self){
  print(paste("prediction horizon:", self$parameters$N ,"*",self$building$parameters$timestep,"s"))
  print(paste("simulation horizon:", self$parameters$Tsim,"*",self$building$parameters$timestep,"s"))
  print(paste("state-space continuous:",ifelse(self$building$parameters$continuous,"continuous","discrete")))
  #print(paste("optimal object is:", self$parameters$obj))
  #print(paste("penalty factor of slack variable:",self$parameters$ECR))
}

iset_para <- function(self,N,Tsim,obj,cost,ymin,ymax,yref,ECR,umax,umin){
  self$parameters$N <- N
  self$parameters$Tsim <- Tsim
  self$parameters$obj <- obj
  self$parameters$cost <- cost
  self$parameters$ymin <- ymin
  self$parameters$ymax <- ymax
  self$parameters$yref <- yref
  self$parameters$ECR <- ECR
  self$parameters$umax <- umax
  self$parameters$umin <- umin
}




