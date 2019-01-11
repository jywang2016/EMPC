rm(list = ls())
# invariant plant + variant constraints(ymax,ymin) + variant objective function(cost)
library(EMPC)
library(dplyr)
library(CVXR)
library(ECOSolveR)

#***************************************************************
# data load
#***************************************************************
load("./test/ssM.Rdata")
ssmodel <- list(A = ssM$A %>% as.matrix(),
                Bu = ssM$Bu %>% as.matrix(),
                Bd = ssM$Bd %>% as.matrix(),
                C = ssM$C %>% as.matrix())

#***************************************************************
# build a new mpc object and set value for the building object
#***************************************************************
mpc2 <- mpc$new()
mpc2$initialize()
mpc2$building$setvalue(ssmodel = ssmodel,
                       disturbance = as.matrix(ssM$Disturbance),
                       timestep = as.matrix(ssM$timestep),
                       x0 = as.matrix(ssM$x0),
                       continuous = F)

mpc2$building$ssM

#***************************************************************
# mpc:constraints set and problem solve
#***************************************************************
N <- 72
Tsim <- 504
nu <- ncol(ssM$Bu)
ny <- nrow(ssM$C)

#umax <- 15;umin <- 0


ECR <- 1e6
cost <- matrix(0.2, ncol = nu, nrow = (N + Tsim))
ymax <- matrix(26, nrow = ny, ncol = (N + Tsim))
ymin <- matrix(22, nrow = ny, ncol = (N + Tsim))
yref <- matrix(24, nrow = ny, ncol = (N + Tsim))
umax <- matrix(15, nrow = ny, ncol = (N + Tsim))
umin <- matrix(0 , nrow = ny, ncol = (N + Tsim))

timestep <- ssM$timestep %>% as.numeric()
time <- (1:nrow(cost))*timestep
for (i in time) {

  ifelse(i %% 86400 > 10*3600 & i %% 86400 <=16*3600,
         cost[i/timestep,] <- rep(0.2,nu),
         cost[i/timestep,] <- rep(0.04,nu))
  ifelse(i %% 86400 <= 8*3600 | i %% 86400 > 18*3600,
         ymax[,i/timestep] <- rep(30,ny),
         ymax[,i/timestep] <- rep(26,ny))
  ifelse(i %% 86400 <= 8*3600 | i %% 86400 > 18*3600,
         ymin[,i/timestep] <- rep(18,ny),
         ymin[,i/timestep] <- rep(22,ny))
  ifelse(i %% 86400 <= 8*3600 | i %% 86400 > 18*3600,
         yref[,i/timestep] <- rep(24,ny),
         yref[,i/timestep] <- rep(20,ny))

}

mpc2$set_parameters(N = N,
                    Tsim = Tsim,
                    obj = "cost",
                    cost = cost,
                    ymin = ymin,
                    ymax = ymax,
                    yref = yref,
                    ECR = ECR,
                    umax = umax,
                    umin = umin)
mpc2$print_para()

mpc2$parameters$obj
class(mpc2$parameters$cost)

mpc2$set_mpc_constraint()

solu <- mpc2$solve_mpc(control = ecos.control(maxit = 500L,
                                              feastol = 8e-6,
                                              reltol = 8e-5))

#***************************************************************
# plot
#***************************************************************

temp <- data.frame(time = 1:Tsim,
                   room1 = t(solu$Y)[,1],
                   room2 = t(solu$Y)[,2],
                   room3 = t(solu$Y)[,3])
ele  <- data.frame(time = 1:Tsim,
                   room1 = t(solu$U)[,1],
                   room2 = t(solu$U)[,2],
                   room3 = t(solu$U)[,3])

library(reshape2)
library(ggplot2)

hfactor <- 3600/as.numeric(ssM$timestep )

temp %>% melt(id = "time") %>%
  ggplot(aes(x = time/hfactor , y = value ,color = variable)) +
  geom_line(size = 1) +
  theme_bw()+
  xlab("time/h") + ylab("temperature/degC")

ele %>% melt(id = "time") %>%
  ggplot(aes(x = time/hfactor , y = value ,color = variable)) +
  geom_line(size = 1) +
  theme_bw()+
  xlab("time/h") + ylab("electricity/kw")
