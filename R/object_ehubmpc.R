#' ehubmpc controller
#' @description
#' solve ehubmpc for "cost-optimal" control in energyhub with storage(i.e. battery, ice storage)
#' or/and production(i.e. PV, wind turbine) units
#' @note
#' Two battery kinds, namely simple model and lead-acid battery model are support at present.
#'
#' @usage
#' model <- ehubmpc$new()
#' model$initialize()
#' model$set_parameters(N,Tsim,obj,cost,ymin,ymax,yref,ECR,umax,umin)
#' model$print_para()
#' model$set_mpc_constraint()
#' model$solve_mpc(unit.index = c(1,0),control = ecos.control())
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
#' unit.index a vector indicates whether storage or production units exist or not.
#' Default value is c(1,0), which means there is storage unit without production unit.
#' @param
#' control ecos.control; control parameters for ECOS solver. More details can be seen in ECOSolver
#' package
#' @format
#' list of equipment power(U),temperature values(Y),battery charge/discharge power(Ub),
#' net electricity power(E), battery capacity(Xb)
#' @examples
#' search `EMPC` on github, find more examples
#' @importFrom R6 R6Class
#' @importFrom CVXR Variable
#' @importFrom CVXR Minimize
#' @importFrom CVXR Problem
#' @importFrom CVXR get_problem_data
#' @importFrom CVXR sum_squares
#' @importFrom ECOSolveR ECOS_csolve
#' @importFrom ECOSolveR ecos.control
#' @export
ehubmpc <- R6::R6Class(classname = "ehubmpc",
                     inherit = mpc, #inherit class:mpc
                     public = list(
                       battery = NA,
                       onsite = NA,
                       initialize = function(){
                         self$building <- consumption$new()
                         self$battery <- storage$new()
                         self$onsite <- production$new()
                       },
                       set_mpc_constraint = function(unit.index = c(1,0)){
                         if(self$battery$parameters$storage_type == "simple_battery"){
                           if(unit.index[2] == 1){
                             iset_ehubmpc_constraint11(self,private)
                           }else{
                             iset_ehubmpc_constraint(self,private)
                           }
                         }else if(unit.index[2] == 0&self$battery$parameters$storage_type == "lead_acid"){
                           iset_ehubmpc_constraint_leadacid(self,private)
                         }
                       },
                       solve_mpc = function(unit.index = c(1,0),control = ecos.control()){
                         if(self$battery$parameters$storage_type == "simple_battery"){
                           if(unit.index[2] == 1){
                             isolve_ehubmpc11(self,private,control)
                           }else{
                             isolve_ehubmpc(self,private,control)
                           }
                         }else if(unit.index[2] == 0&self$battery$parameters$storage_type == "lead_acid"){
                           isolve_ehubmpc_leadacid(self,private,control)
                         }
                       }
                     ),
                     private = list(
                       prob_data = NA,
                       matrixbind = function(M1,M2){
                         imatrixbind(self,M1,M2)
                       }
                     ))
imatrixbind <- function(self,M1,M2){
  A <- M1
  B <- M2
  temp1 <- cbind(A,matrix(0,nrow(A),ncol(B)))
  temp2 <- cbind(matrix(0,nrow(B),ncol(A)),B)
  temp3 <- rbind(temp1,temp2)
  return(temp3)
}

isolve_ehubmpc_leadacid <- function(self,private,control){
  # cosumption component
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM

  # consumption component state-space mdoel
  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C
  Dist <- t(self$building$parameters$disturbance)

  # storage component
  ssMs <- self$battery$parameters$ssM
  Ab <- ssMs$A
  Bub <- ssMs$Bu

  #initate state
  x0 <- self$building$parameters$x0
  xb0 <- self$battery$parameters$x0

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  N <- self$parameters$N
  Tsim <- ncol(Dist) - N

  #lead acid
  nxb <- 2
  nub <- 2

  ymax <- self$parameters$ymax
  cost <- self$parameters$cost
  ymin <- self$parameters$ymin
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  ne <- 1
  U  <- matrix(0,nrow= nu, ncol = Tsim)
  Y  <- matrix(0,nrow = ny, ncol = Tsim)
  Ub <- matrix(0,nrow = nub, ncol = Tsim)
  E  <- matrix(0,nrow = ne, ncol = Tsim)
  Xb <- matrix(0,nrow = nxb, ncol = Tsim)

  prob_data <- private$prob_data

  solver_output <- ECOS_csolve(c = prob_data[["c"]],
                               G = prob_data[["G"]],
                               h = prob_data[["h"]],
                               dims = prob_data[["dims"]],
                               A = prob_data[["A"]],
                               b = prob_data[["b"]])

  U[,1] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
  Y[,1] <- solver_output$x[(nx*N + 1):(nx*N + ny)]
  Ub[,1] <- solver_output$x[((nx+ny+nu+ny+nxb)*N+1):((nx+ny+nu+ny+nxb)*N+nub)]
  E[,1] <- solver_output$x[((nx+ny+nu+ny+nxb+nub)*N+1 ):((nx+ny+nu+ny+nxb+nub)*N+ne)]
  Xb[,1] <- solver_output$x[((nx+ny+nu+ny)*N+1):((nx+ny+nu+ny)*N + nxb)]
  x0 <- solver_output$x[(nx+1):(nx*2)]
  xb0 <- solver_output$x[((nx+ny+nu+ny)*N+nxb+1):((nx+ny+nu+ny)*N+2*nxb)]

  for(i in 2:Tsim){

    costN <- cost[i:(N+i-1),]
    yminN <- ymin[,i:(N+i-1)]
    ymaxN <- ymax[,i:(N+i-1)]
    umaxN <- umax[,i:(N+i-1)]
    uminN <- umin[,i:(N+i-1)]

    prob_data$h[1:(ny*N)] <- as.vector(yminN) * (-1)
    prob_data$h[(ny*N+1):(ny*N+ny*N)] <- as.vector(ymaxN)
    prob_data$h[(2*ny*N+1):(2*ny*N + nu*N)] <- as.vector(umaxN) #umax
    prob_data$h[(2*ny*N+nu*N+1):(2*ny*N + 2*nu*N)] <- as.vector(uminN)*(-1) #umin
    prob_data$c[((nx+ny+nu+ny+nxb+nub)*N+1):((nx+ny+nu+ny+nxb+nub)*N+ne*N)] <- as.vector(t(costN[,1]))
    prob_data$b[1:nx] <- x0
    prob_data$b[(nx+1):(nx*N)] <- as.vector(Bd %*% Dist[,i:(N+i-2)])
    prob_data$b[((nx+ny)*N+1):( (nx+ny)*N + nxb )] <- xb0

    solver_output <- ECOS_csolve(c = prob_data[["c"]],
                                 G = prob_data[["G"]],
                                 h = prob_data[["h"]],
                                 dims = prob_data[["dims"]],
                                 A = prob_data[["A"]],
                                 b = prob_data[["b"]],
                                 control = control)

    print(paste("Iteration",i,solver_output$infostring))
    U[,i] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
    Y[,i] <- solver_output$x[(nx*N + 1):(nx*N + ny)]
    Ub[,i] <- solver_output$x[((nx+ny+nu+ny+nxb)*N+1):((nx+ny+nu+ny+nxb)*N+nub)]
    E[,i] <- solver_output$x[((nx+ny+nu+ny+nxb+nub)*N+1 ):((nx+ny+nu+ny+nxb+nub)*N+ne)]
    Xb[,i] <- solver_output$x[((nx+ny+nu+ny)*N+1):((nx+ny+nu+ny)*N + nxb)]
    x0 <- solver_output$x[(nx+1):(nx*2)]
    xb0 <- solver_output$x[((nx+ny+nu+ny)*N+nxb+1):((nx+ny+nu+ny)*N+2*nxb)]
  }
  return(list(U = U, Y = Y, Ub = Ub, E = E, Xb = Xb))
}

iset_ehubmpc_constraint_leadacid <- function(self,private){
  # cosumption component
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM

  # consumption component state-space mdoel
  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C
  Dist <- t(self$building$parameters$disturbance)

  # storage component
  ssMs <- self$battery$parameters$ssM
  Ab <- ssMs$A
  Bub <- ssMs$Bu

  #initate state
  x0 <- self$building$parameters$x0
  xb0 <- self$battery$parameters$x0

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  N <- self$parameters$N
  Tsim <- ncol(Dist) - N

  x <- Variable(rows = nx, cols = N)
  y <- Variable(rows = ny, cols = N)
  u <- Variable(rows = nu, cols = N)
  s <- Variable(rows = ny, cols = N)

  #lead acid
  nxb <- 2
  nub <- 2
  xb <- Variable(rows = nxb, cols = N)
  ub <- Variable(rows = nub, cols = N)

  #electricity useage
  ne <- 1
  e <- Variable(rows = ne, cols = N)

  ymax <- self$parameters$ymax
  cost <- self$parameters$cost
  ymin <- self$parameters$ymin
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  costN <- cost[1:N,]
  ymaxN <- ymax[,1:N]
  yminN <- ymin[,1:N]
  umaxN <- umax[,1:N]
  uminN <- umin[,1:N]

  A_cst <- self$battery$parameters$A_cst
  B_cst <- self$battery$parameters$B_cst
  h_cst <- self$battery$parameters$h_cst
  h_cst <- matrix(rep(h_cst,N),ncol = N)

  ECR <- self$parameters$ECR

  obj <- Minimize(sum(s*ECR) + sum(e%*%costN[,1]))

  constraints <- list(y >= yminN-s,
                      y <= ymaxN+s,
                      u <= umaxN,
                      u >= uminN,
                      x[,1]== x0,
                      x[,2:N] == A %*% x[,1:(N-1)] + Bu %*% u[,1:(N-1)] + Bd %*% Dist[,1:(N+1-2)],
                      y == C %*% x,
                      s >= 0,
                      xb[,1]==xb0,
                      xb[,2:N] == Ab %*% xb[,1:(N-1)] + Bub %*% ub[,1:(N-1)],
                      A_cst %*% xb + B_cst %*% ub <= h_cst,
                      ub[1,] >= 0,
                      ub[2,] <= 0,
                      xb >= 0,
                      e == sum_entries(u,axis = 2) + sum_entries(ub,axis = 2),
                      e >= 0)
  #trasform constraints
  prob <- Problem(obj,constraints)
  private$prob_data <- get_problem_data(prob,solver = "ECOS")
}

isolve_ehubmpc <- function(self,private,control){
  prob_data <- private$prob_data
  solver_output <- ECOS_csolve(c = prob_data[["c"]],
                               G = prob_data[["G"]],
                               h = prob_data[["h"]],
                               dims = prob_data[["dims"]],
                               A = prob_data[["A"]],
                               b = prob_data[["b"]],
                               control = control)
  print(paste("Iteration",1,solver_output$infostring))

  # cosumption component
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM

  # consumption component state-space mdoel
  Ac <- ssM$A
  Buc <- ssM$Bu
  Bdc <- ssM$Bd
  Cc <- ssM$C
  Distc <- t(self$building$parameters$disturbance)

  # storage component
  ssMs <- self$battery$parameters$ssM
  Ab <- ssMs$A
  Bub <- ssMs$Bu #Bdb and Cb need be constructed
  Bdb <- matrix(0,nrow(Bub),1)
  Cb <- diag(nrow(Ab))

  #new state-space model
  A <- private$matrixbind(Ac,Ab)
  Bu <- private$matrixbind(Buc,Bub)
  Bd <- private$matrixbind(Bdc,Bdb)
  C <- private$matrixbind(Cc,Cb)

  #initate state
  #x0c <- self$building$parameters$x0
  #x0b <- self$battery$parameters$x0
  #x0 <- c(x0c,x0b)

  #dist
  Distb <- matrix(0,nrow = 1,ncol = ncol(Distc))
  Dist <- rbind(Distc,Distb)

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  nxc <- ncol(Ac)
  nyc <- nrow(Cc)
  nuc <- ncol(Buc)
  ndc <- ncol(Bdc)

  N <- self$parameters$N
  Tsim <- ncol(Dist) - N

  U <- matrix(0,nrow= nu,ncol = Tsim)
  Y <- matrix(0,nrow = ny,ncol = Tsim)

  U[,1] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)] #u
  Y[,1] <- solver_output$x[(nx*N + 1):(nx*N + ny)] #y

  x0 <- solver_output$x[(nx+1):(nx*2)]

  ymax <- self$parameters$ymax
  xbmax <- matrix(self$battery$parameters$xbmax,nrow = 1,ncol = ncol(ymax))
  ymax <- rbind(ymax,xbmax)

  cost <- self$parameters$cost
  cost <- cbind(cost,cost[,1])

  ymin <- self$parameters$ymin
  xbmin <- matrix(self$battery$parameters$xbmin,nrow = 1,ncol = ncol(ymin))
  ymin <- rbind(ymin,xbmin)

  #yref <- self$parameters$yref
  umax <- self$parameters$umax
  ubmax <- matrix(self$battery$parameters$ubmax,nrow = 1,ncol = ncol(umax))
  umax <- rbind(umax,ubmax)

  umin <- self$parameters$umin
  ubmin <- matrix(self$battery$parameters$ubmin,nrow = 1,ncol = ncol(umin))
  umin <- rbind(umin,ubmin)

  for (i in 2:Tsim)
  {

    costN <- cost[i:(N+i-1),]
    yminN <- ymin[,i:(N+i-1)]
    ymaxN <- ymax[,i:(N+i-1)]
    #yrefN <- yref[,i:(N+i-1)]
    umaxN <- umax[,i:(N+i-1)]
    uminN <- umin[,i:(N+i-1)]

    prob_data$h[1:(ny*N)] <- as.vector(yminN) * (-1) #modify yminN
    prob_data$h[(ny*N+1):(ny*N+ny*N)] <- as.vector(ymaxN) #modify ymaxN
    prob_data$c[(nx*N+ny*N+1):(nx*N+ny*N+nu*N)] <- as.vector(t(costN)*nu) #modify cost
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

    print(paste("Iteration",i,solver_output$infostring))
    U[,i] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
    Y[,i] <- solver_output$x[(nx*N + 1):(nx*N + ny)]
    x0 <- solver_output$x[(nx+1):(nx*2)]
  }
  list(U = U, Y = Y)
}

iset_ehubmpc_constraint <- function(self,private){
  # cosumption component
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM

  # consumption component state-space mdoel
  Ac <- ssM$A
  Buc <- ssM$Bu
  Bdc <- ssM$Bd
  Cc <- ssM$C
  Distc <- t(self$building$parameters$disturbance)

  # storage component
  ssMs <- self$battery$parameters$ssM
  Ab <- ssMs$A
  Bub <- ssMs$Bu #Bdb and Cb need be constructed
  Bdb <- matrix(0,nrow(Bub),1)
  Cb <- diag(nrow(Ab))

  #new state-space model
  A <- private$matrixbind(Ac,Ab)
  Bu <- private$matrixbind(Buc,Bub)
  Bd <- private$matrixbind(Bdc,Bdb)
  C <- private$matrixbind(Cc,Cb)

  #initate state
  x0c <- self$building$parameters$x0
  x0b <- self$battery$parameters$x0
  x0 <- c(x0c,x0b)


  #dist
  Distb <- matrix(0,nrow = 1,ncol = ncol(Distc))
  Dist <- rbind(Distc,Distb)

  # #if (units.index[2] == 1) ...
  # if(units.index[2] == 1){
  #   #onsite generation
  #   Ap <- as.matrix(0)
  #   Bup <- as.matrix(0)
  #   Bdp <- as.matrix(1)
  #   Cp <- as.matrix(1)
  #   x0p <- 0
  #
  #   A  <- private$matrixbind(A,Ap)
  #   Bu <- private$matrixbind(Bu,Bup)
  #   Bd <- private$matrixbind(Bd,Bdp)
  #   C  <- private$matrixbind(C,Cp)
  #
  #   x0 <- c(x0,x0p)
  #
  #   Distp <- self$onsite$generation
  #   Dist <- rbind(Dist, Distp)
  # }

  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  nxc <- ncol(Ac)
  nyc <- nrow(Cc)
  nuc <- ncol(Buc)
  ndc <- ncol(Bdc)

  N <- self$parameters$N
  Tsim <- ncol(Dist) - N

  x <- Variable(rows = nx, cols = N)
  y <- Variable(rows = ny, cols = N)
  u <- Variable(rows = nu, cols = N)
  s <- Variable(rows = ny, cols = N)

  ymax <- self$parameters$ymax
  xbmax <- matrix(self$battery$parameters$xbmax,nrow = 1,ncol = ncol(ymax))
  ymax <- rbind(ymax,xbmax)

  cost <- self$parameters$cost
  cost <- cbind(cost,cost[,1])

  ymin <- self$parameters$ymin
  xbmin <- matrix(self$battery$parameters$xbmin,nrow = 1,ncol = ncol(ymin))
  ymin <- rbind(ymin,xbmin)

  #yref <- self$parameters$yref
  umax <- self$parameters$umax
  ubmax <- matrix(self$battery$parameters$ubmax,nrow = 1,ncol = ncol(umax))
  umax <- rbind(umax,ubmax)

  umin <- self$parameters$umin
  ubmin <- matrix(self$battery$parameters$ubmin,nrow = 1,ncol = ncol(umin))
  umin <- rbind(umin,ubmin)

  costN <- cost[1:N,]
  ymaxN <- ymax[,1:N]
  yminN <- ymin[,1:N]
  umaxN <- umax[,1:N]
  uminN <- umin[,1:N]

  ECR <- self$parameters$ECR

  obj <- Minimize(sum(s*ECR) + sum(u%*%costN))

  constraints <- list(y >= yminN-s,
                      y <= ymaxN+s,
                      u <= umaxN,
                      u >= uminN,
                      x[,1]== x0,
                      x[,2:N] == A %*% x[,1:(N-1)] + Bu %*% u[,1:(N-1)] + Bd %*% Dist[,1:(N+1-2)],
                      y == C %*% x,
                      s[1:nyc,] >= 0,
                      s[(nyc+1):ny,] == 0,
                      sum_entries(u,axis = 2) >= 0)
  #trasform constraints
  prob <- Problem(obj,constraints)
  private$prob_data <- get_problem_data(prob,solver = "ECOS")
}

iset_ehubmpc_constraint11 <- function(self,private){
  # cosumption component
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM

  # consumption component state-space mdoel
  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C
  Dist <- t(self$building$parameters$disturbance)

  #initate state
  x0 <- self$building$parameters$x0
  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  N <- self$parameters$N
  Tsim <- ncol(Dist) - N

  x <- Variable(rows = nx, cols = N)
  y <- Variable(rows = ny, cols = N)
  u <- Variable(rows = nu, cols = N)
  s <- Variable(rows = ny, cols = N)

  xb <- Variable(rows = 2, cols = N)
  ub <- Variable(rows = 2, cols = N)
  e <- Variable(rows = 1, cols = N)

  ymax <- self$parameters$ymax
  cost <- self$parameters$cost
  ymin <- self$parameters$ymin
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  #battery & production state-space model
  Ab <- matrix(0,2,2)
  Ab[1,1] <- self$battery$parameters$ssM$A
  Bub <- diag(2)
  Bub[1,1] <- self$battery$parameters$ssM$Bu

  xbmax <- self$battery$parameters$xbmax
  xbmax <- matrix(xbmax,nrow = 2, ncol = (Tsim + N))
  xbmax[2,] <- 0
  xbmin <- self$battery$parameters$xbmin
  xbmin <- matrix(xbmin,nrow = 2, ncol = (Tsim + N))
  xbmin[2,] <- (self$onsite$parameters$generation[1:(Tsim+N)])*(-1)

  ubmax <- self$battery$parameters$ubmax
  ubmax <- matrix(ubmax,nrow = 2, ncol = (Tsim + N))
  ubmax[2,] <- 0
  ubmin <- self$battery$parameters$ubmin
  ubmin <- matrix(ubmin,nrow = 2, ncol = (Tsim + N))
  ubmin[2,] <- (self$onsite$parameters$generation[1:(Tsim+N)])*(-1)

  xb0 <- matrix(0,2,1)
  xb0[1,1] <- self$battery$parameters$x0

  costN <- cost[1:N,]
  ymaxN <- ymax[,1:N]
  yminN <- ymin[,1:N]
  umaxN <- umax[,1:N]
  uminN <- umin[,1:N]
  xbmaxN <- xbmax[,1:N]
  xbminN <- xbmin[,1:N]
  ubmaxN <- ubmax[,1:N]
  ubminN <- ubmin[,1:N]

  ECR <- self$parameters$ECR

  obj <- Minimize(sum(s*ECR) + sum(e%*%costN[,1]))

  constraints <- list(y >= yminN-s,
                      y <= ymaxN+s,
                      u <= umaxN,
                      u >= uminN,
                      x[,1] == x0,
                      x[,2:N] == A %*% x[,1:(N-1)] + Bu %*% u[,1:(N-1)] + Bd %*% Dist[,1:(N+1-2)],
                      y == C %*% x,
                      s >= 0,
                      xb[,1] == xb0,
                      xb[,2:N] == Ab %*% xb[,1:(N-1)] + Bub %*% ub[,1:(N-1)],
                      xb >= xbminN,
                      xb <= xbmaxN,
                      ub >= ubminN,
                      ub <= ubmaxN,
                      e == sum_entries(u,axis = 2) + sum_entries(ub,axis = 2),
                      e >= 0)
  #trasform constraints
  prob <- Problem(obj,constraints)
  private$prob_data <- get_problem_data(prob,solver = "ECOS")
}

isolve_ehubmpc11 <- function(self,private,control){
  # cosumption component
  x0 <- self$building$parameters$x0
  ssM <- self$building$parameters$ssM

  # consumption component state-space mdoel
  A <- ssM$A
  Bu <- ssM$Bu
  Bd <- ssM$Bd
  C <- ssM$C
  Dist <- t(self$building$parameters$disturbance)

  #initate state
  x0 <- self$building$parameters$x0
  nx <- ncol(A)
  ny <- nrow(C)
  nu <- ncol(Bu)
  nd <- ncol(Bd)

  N <- self$parameters$N
  Tsim <- ncol(Dist) - N

  ymax <- self$parameters$ymax
  cost <- self$parameters$cost
  ymin <- self$parameters$ymin
  umax <- self$parameters$umax
  umin <- self$parameters$umin

  #battery & production state-space model
  Ab <- matrix(0,2,2)
  Ab[1,1] <- self$battery$parameters$ssM$A
  Bub <- diag(2)
  Bub[1,1] <- self$battery$parameters$ssM$Bu

  xbmax <- self$battery$parameters$xbmax
  xbmax <- matrix(xbmax,nrow = 2, ncol = (Tsim + N))
  xbmax[2,] <- 0
  xbmin <- self$battery$parameters$xbmin
  xbmin <- matrix(xbmin,nrow = 2, ncol = (Tsim + N))
  xbmin[2,] <- (self$onsite$parameters$generation[1:(Tsim+N)])*(-1)

  ubmax <- self$battery$parameters$ubmax
  ubmax <- matrix(ubmax,nrow = 2, ncol = (Tsim + N))
  ubmax[2,] <- 0
  ubmin <- self$battery$parameters$ubmin
  ubmin <- matrix(ubmin,nrow = 2, ncol = (Tsim + N))
  ubmin[2,] <- (self$onsite$parameters$generation[1:(Tsim+N)])*(-1)

  prob_data <- private$prob_data
  solver_output <- ECOS_csolve(c = prob_data[["c"]],
                               G = prob_data[["G"]],
                               h = prob_data[["h"]],
                               dims = prob_data[["dims"]],
                               A = prob_data[["A"]],
                               b = prob_data[["b"]],
                               control = control)
  print(paste("Iteration",1,solver_output$infostring))

  U  <- matrix(0,nrow= nu, ncol = Tsim)
  Y  <- matrix(0,nrow = ny, ncol = Tsim)
  Ub <- matrix(0,nrow = 2, ncol = Tsim)
  E  <- matrix(0,nrow = 1, ncol = Tsim)
  Xb <- matrix(0,nrow = 2, ncol = Tsim)

  nxb <- 2
  nub <- 2
  ne <- 1

  U[,1] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
  Y[,1] <- solver_output$x[(nx*N + 1):(nx*N + ny)]
  Ub[,1] <- solver_output$x[((nx+ny+nu+ny+nxb)*N+1):((nx+ny+nu+ny+nxb)*N+nub)]
  E[,1] <- solver_output$x[((nx+ny+nu+ny+nxb+nub)*N+1 ):((nx+ny+nu+ny+nxb+nub)*N+ne)]
  Xb[,1] <- solver_output$x[((nx+ny+nu+ny)*N+1):((nx+ny+nu+ny)*N + nxb)]
  x0 <- solver_output$x[(nx+1):(nx*2)]
  xb0 <- solver_output$x[((nx+ny+nu+ny)*N+nxb+1):((nx+ny+nu+ny)*N+2*nxb)]

  for (i in 2:Tsim)
  {
    costN <- cost[i:(N+i-1),]
    yminN <- ymin[,i:(N+i-1)]
    ymaxN <- ymax[,i:(N+i-1)]
    umaxN <- umax[,i:(N+i-1)]
    uminN <- umin[,i:(N+i-1)]
    #xbmaxN <- xbmax[,i:(N+i-1)]
    xbminN <- xbmin[,i:(N+i-1)]
    #ubmaxN <- ubmax[,i:(N+i-1)]
    ubminN <- ubmin[,i:(N+i-1)]

    prob_data$h[1:(ny*N)] <- as.vector(yminN) * (-1) #modify yminN
    prob_data$h[(ny*N+1):(ny*N+ny*N)] <- as.vector(ymaxN) #modify ymaxN
    prob_data$c[((nx+ny+nu+ny+nxb+nub)*N+1):((nx+ny+nu+ny+nxb+nub)*N+ne*N)] <- as.vector(t(costN[,1])) #modify cost？？
    prob_data$h[(2*ny*N+1):(2*ny*N + nu*N)] <- as.vector(umaxN) #umax
    prob_data$h[(2*ny*N+nu*N+1):(2*ny*N + 2*nu*N)] <- as.vector(uminN)*(-1) #umin
    prob_data$b[1:nx] <- x0 # modify x0
    prob_data$b[(nx+1):(nx*N)] <- as.vector(Bd %*% Dist[,i:(N+i-2)])
    prob_data$b[((nx+ny)*N+1):( (nx+ny)*N + nxb )] <- xb0
    prob_data$h[((2*ny+2*nu+ny)*N+1):((2*ny+2*nu+ny+nxb)*N)] <- as.vector(xbminN)*(-1) #xbminN
    prob_data$h[((2*ny+2*nu+ny+2*nxb)*N+1):((2*ny+2*nu+ny+2*nxb+nub)*N)] <- as.vector(ubminN)*(-1) #ubminN

    solver_output <- ECOS_csolve(c = prob_data[["c"]],
                                 G = prob_data[["G"]],
                                 h = prob_data[["h"]],
                                 dims = prob_data[["dims"]],
                                 A = prob_data[["A"]],
                                 b = prob_data[["b"]],
                                 control = control)

    print(paste("Iteration",i,solver_output$infostring))
    U[,i] <- solver_output$x[(nx*N + ny*N + 1):(nx*N + ny*N + nu)]
    Y[,i] <- solver_output$x[(nx*N + 1):(nx*N + ny)]
    Ub[,i] <- solver_output$x[((nx+ny+nu+ny+nxb)*N+1):((nx+ny+nu+ny+nxb)*N+nub)]
    E[,i] <- solver_output$x[((nx+ny+nu+ny+nxb+nub)*N+1 ):((nx+ny+nu+ny+nxb+nub)*N + ne)]
    Xb[,i] <- solver_output$x[((nx+ny+nu+ny)*N+1):((nx+ny+nu+ny)*N + nxb)]
    x0 <- solver_output$x[(nx+1):(nx*2)]
    xb0 <- solver_output$x[((nx+ny+nu+ny)*N+nxb+1):((nx+ny+nu+ny)*N+2*nxb)]
  }
  return(list(U = U, Y = Y, Ub = Ub, E = E, Xb = Xb))
}
