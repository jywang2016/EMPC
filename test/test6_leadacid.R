rm(list = ls())
# invariant everything + battery
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
#mpc2 <- mpc$new()
mpc3 <- ehubmpc$new()
#mpc2$initialize()
mpc3$initialize()
mpc3$building$setvalue(ssmodel = ssmodel,
                       disturbance = as.matrix(ssM$Disturbance),
                       timestep = as.matrix(ssM$timestep),
                       x0 = as.matrix(ssM$x0),
                       continuous = F)

mpc3$building$parameters$ssM

#lead_acid_battery
#ssM d2c2d
Ab <- matrix(c(0.51,0.22,0.47,0.78),nrow = 2, byrow = T)
Bub <-matrix(c(0.61,0.83,0.25,0.39),nrow = 2, byrow = T)
plant <- d2c(Ab,Bub,600)
Ab <- c2d(plant$A,plant$B,1200)$A
Bub <- c2d(plant$A,plant$B,1200)$B

A_cst <- matrix(c(-1,-1,1,1,0,0,-0.61,-0.26,0.73,0.73),byrow = T,ncol = 2)
B_cst <- matrix(c(0,0,0,0,1,0,1,0,0,-1),byrow = T,ncol = 2)
h_cst <- matrix(c(-1,5,8,0,4.39),ncol = 1)

mpc3$battery$setvalue(ssmodel = list(A = Ab, Bu = Bub),
                      continuous = FALSE,
                      timestep = 1200,
                      x0 = matrix(c(0,1),ncol = 1),
                      storage_type = "lead_acid",
                      A_cst = A_cst,
                      B_cst = B_cst,
                      h_cst = h_cst)

mpc3$battery$parameters
#***************************************************************
# mpc:constraints set and problem solve
#***************************************************************
N <- 72
Tsim <- 504

#umax <- 15;umin <- 0


ECR <- 1e6
cost <- matrix(0.2, ncol = 3, nrow = (N+Tsim))
ymax <- matrix(26, nrow = 3, ncol = (N+Tsim))
ymin <- matrix(22, nrow = 3, ncol = (N+Tsim))
umax <- matrix(15, nrow = 3, ncol = (N+Tsim))
umin <- matrix(0, nrow = 3, ncol = (N+Tsim))
#yref <- matrix(24, nrow = 4, ncol = (N+Tsim))
#
# ymax[4,] <- 20
# ymin[4,] <- 0
# umax[4,] <- 20
# umin[4,] <- -20

timestep <- ssM$timestep %>% as.numeric()
time <- (1:nrow(cost))*timestep
for (i in time) {

  ifelse(i %% 86400 > 10*3600 & i %% 86400 <=16*3600,
         cost[i/timestep,] <- 0.2,
         cost[i/timestep,] <- 0.04)
  ifelse(i %% 86400 <= 8*3600 | i %% 86400 > 18*3600,
         ymax[,i/timestep] <- 30,
         ymax[,i/timestep] <- 26)
  ifelse(i %% 86400 <= 8*3600 | i %% 86400 > 18*3600,
         ymin[,i/timestep] <- 18,
         ymin[,i/timestep] <- 22)
  # ifelse(i %% 86400 <= 8*3600 | i %% 86400 > 18*3600,
  #        umax[4,i/timestep] <- 20,
  #        umax[4,i/timestep] <- 15)
  # ifelse(i %% 86400 <= 8*3600 | i %% 86400 > 18*3600,
  #        umin[4,i/timestep] <- -20,
  #        umin[4,i/timestep] <- -15)

}

mpc3$set_parameters(N = N,
                    Tsim = Tsim,
                    obj = "cost",
                    cost = cost,
                    ymin = ymin,
                    ymax = ymax,
                    yref = NA,
                    ECR = ECR,
                    umax = umax,
                    umin = umin)
mpc3$print_para() #use for print prediction horizon, control horizon, and ssM is continuous or not

mpc3$parameters$obj

class(mpc3$parameters$cost)

mpc3$set_mpc_constraint() #obj has only two avaliable value cost or control

solu <- mpc3$solve_mpc(control = ecos.control(maxit = 1000L,
                                              feastol = 5e-5,
                                              reltol = 1.5e-5))

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
nete <- data.frame(time = 1:Tsim,
                   nete = t(solu$E))
battery_V <- data.frame(time = 1:Tsim,
                        Uin = t(solu$Ub)[,1],
                        Uout = t(solu$Ub)[,2])
battery_E <- data.frame(time = 1:Tsim,
                        X1 = t(solu$Xb)[,1],
                        X2 = t(solu$Xb)[,2])
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

nete %>%
  ggplot(aes(x = time/hfactor, y = nete)) +
  geom_line(size = 1) +
  theme_bw()+
  xlab("time/h") + ylab("purchase electricity/kw")

battery_V %>%  melt(id ="time") %>%
  ggplot(aes(x = time/hfactor, y = value, color = variable)) +
  geom_line(size = 1) +
  theme_bw()+
  xlab("time/h") + ylab("battery/kw")

battery_E %>% melt(id = "time") %>%
  ggplot(aes(x = time/hfactor, y = value, color = variable)) +
  geom_line(size = 1) +
  theme_bw()+
  xlab("time/h") + ylab("battery/kw")
