rm(list = ls())

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
mpc3 <- ehubmpc$new()
mpc3$initialize()
mpc3$building$setvalue(ssmodel = ssmodel,
                       disturbance = as.matrix(ssM$Disturbance),
                       timestep = as.matrix(ssM$timestep),
                       x0 = as.matrix(ssM$x0),
                       continuous = F)

mpc3$building$ssM

mpc3$battery$setvalue(ssmodel = list(A = as.matrix(0.9800), Bu = as.matrix(0.3333)),
                      continuous = FALSE,
                      timestep = 1200,
                      x0 = 0,
                      xbmax = 20,
                      xbmin = 0,
                      ubmax = 20,
                      ubmin = -20)

mpc3$battery$parameters

#发电模块的设定
pvraw <-read.csv("./test/pv.csv")
pv <- as.matrix(t(pvraw))
mpc3$onsite$setvalue(generation = pv,
                     timestep = 1200)

#***************************************************************
# mpc:constraints set and problem solve
#***************************************************************
N <- 72
Tsim <- 504

ECR <- 1e6
cost <- matrix(0.2, ncol = 3, nrow = (N+Tsim))
ymax <- matrix(26, nrow = 3, ncol = (N+Tsim))
ymin <- matrix(22, nrow = 3, ncol = (N+Tsim))
umax <- matrix(15, nrow = 3, ncol = (N+Tsim))
umin <- matrix(0, nrow = 3, ncol = (N+Tsim))

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

mpc3$set_mpc_constraint(unit.index = c(1,1)) #obj has only two avaliable value cost or control
# prob <- mpc3$.__enclos_env__$private$prob_data
# write.csv(prob$h,"./test/h.csv",row.names = F)
solu <- mpc3$solve_mpc(unit.index = c(1,1),
                       control = ecos.control(maxit = 1000L,
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
                        Upv = -t(solu$Ub)[,2])
battery_E <- data.frame(time = 1:Tsim,
                        Xbattery = t(solu$Xb)[,1],
                        Xpv = -t(solu$Xb)[,2])
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
