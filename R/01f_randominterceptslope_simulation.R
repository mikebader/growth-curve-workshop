#### SIMULATION EXAMPLE: RANDOM INTERCEPTS & SLOPES ####
## Description: This file simulates a dataset where trends start at different
##              intercepts and changes at different rate for 150 metros and
##              analyzes that data
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(lme4)
library(ggplot2)
library(MASS)

#### SIMULATION EXAMPLE: RANDOM INTERCEPTS & SLOPES
## PLAN THE POPULATION
month <- c(0:23)
N  <- 150
N_t <- length(month)
t  <- rep(month,N)
i  <- rep(c(1:N),each=N_t)

gamma_00 <- log(117)
gamma_10 <- 0.005
sigma_ti <- 0.002
tau_00   <- 0.10^2  ## Note that t_00, tau_11, and t_01 represent
tau_11   <- 0.005^2  ## *variances/covariances* not standard deviations
tau_01   <- 0
Tau   <- matrix(c(tau_00,tau_01,tau_01,tau_11),nrow=2) 

## CONJURE THE POPULATION
## Set metropolitan area characteristics
tau <- mvrnorm(N,c(0,0),Tau)
var(tau)
beta_0i <- gamma_00 + tau[,1]
beta_1i <- gamma_10 + tau[,2]

## Create individual change trajectories
lnvalue_ti <- rep(beta_0i,each=N_t) + rep(beta_1i,each=N_t)*t + rnorm(N*N_t,0,sigma_ti)
d.sim <- data.frame(i,t,lnvalue_ti)
head(d.sim,30)

## DESCRIBE THE DATA
g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_ti,group=i)) + geom_line()
g.sim


## Show a random sample of trajectories to see individual lines
g.samp <- ggplot(d.sim[d.sim$i%in%sample(i,20),],
                 aes(x=t,y=lnvalue_ti,group=i)) + geom_line()
g.samp

## Estimate multilevel model
m.sim <- lmer(lnvalue_ti ~ t + (1 + t|i),data=d.sim)
summary(m.sim)
m.sim.fe <- fixef(m.sim)     ## Deterministic
m.sim.re <- ranef(m.sim)$i   ## Stochastic
var(m.sim.re)
cbind(var(m.sim.re), var(tau))  ## Compare to `Tau` above

g.pred <- geom_abline(intercept=m.sim.fe[1],slope=m.sim.fe[2],
                      color="orange",size=1.5)
g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_ti,group=i)) +
    geom_line() + g.pred +
    scale_x_continuous(breaks=seq(0,23,1),labels=rep(month.abb,3)[3:26])
g.sim

## Record predicted values and total error
d.sim$lnvalue_ti_hat <- predict(m.sim,re.form=NA)
d.sim$e_tot <- d.sim$lnvalue_ti - d.sim$lnvalue_ti_hat ## Total error

## Record stochastic components of the model
d.sim$r_0i <- predict(m.sim,re.form=~(1|i),random.only=TRUE)
d.sim$r_1i <- ranef(m.sim)$i[,2][d.sim$i]
d.sim$r_1iXt <- predict(m.sim,re.form=~(0+t|i),random.only=TRUE)
d.sim$e_ti <- (d.sim$e_tot - d.sim$r_0i - d.sim$r_1iXt)


i_ex <- sample(unique(d.sim$i), 5)
d.sim$i <- factor(d.sim$i)
g.ex <- ggplot(d.sim[d.sim$i%in%i_ex,],aes(x=t,y=lnvalue_ti,group=i,linetype=i)) +
    g.pred + geom_line() +
    scale_linetype_manual(values=1:5,labels=letters[1:5])
g.ex

library(knitr)
d.ex <- m.sim.re[i_ex,]
rownames(d.ex) <- letters[1:5]
names(d.ex) <- c("beta\\_0","beta\\_1")
kable(d.ex,digits=4, format="pandoc")


