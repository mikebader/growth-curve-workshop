#### SIMULATION EXAMPLE: SLOPE PREDICTOR ####
## Description: This file simulates a constant predictor on the slope
## Author: Michael Bader

## PLAN THE POPULATION
rm(list=ls())
library(MASS)
library(lme4)
library(ggplot2)

month <- c(0:12)
N <- 150
T <- length(month)
t <- rep(month,N)
i <- rep(c(1:N),each=T)

lnpop_i <- rnorm(N,0,1.4) + 13
lnpop_i <- rep(lnpop_i,each=T)

gamma_00 <- log(117)
gamma_10 <- 0.005
gamma_20 <- 0.001
sigma_ti <- 0.002
tau_0i <- 0.10
tau_1i <- 0.004
tau_01 <- 0



## CONJURE THE POPULATION
metros <- mvrnorm(N,c(0,0),matrix(c(tau_0i,tau_01,tau_01,tau_1i),nrow=2))
var(metros)
beta_0 <- gamma_00 + metros[,1]
beta_1 <- gamma_10 + metros[,2]
beta_2 <- gamma_20

lnvalue_ti <- rep(beta_0,each=T) + rep(beta_1,each=T)*t + rep(beta_2,each=T)*lnpop_i +rnorm(N*T,0,sigma_ti)
d.sim <- data.frame(i,t,lnvalue_ti)

## ANALYZE DATA
m.sim <- lmer(lnvalue_ti ~ t + I(t*lnpop_i) + (1 + t|i),data=d.sim)
summary(m.sim)

m.sim.fe <- fixef(m.sim)
m.sim.re <- ranef(m.sim)$i

mean_pop <- mean(lnpop_i)

m.sim.cen <- lmer(lnvalue_ti ~ t + I(t*(lnpop_i-mean_pop)) + (1 + t|i),data=d.sim)
summary(m.sim.cen)

m.sim.fe <- fixef(m.sim.cen)
m.sim.re <- ranef(m.sim.cen)$i

g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_ti,group=i)) +
    geom_line() +
    geom_abline(intercept=m.sim.fe[1],slope=m.sim.fe[2],color="orange",size=1.5) +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])
g.sim

sd1_pop <- sd(lnpop_i) + mean_pop
g.sim.diff <- g.sim + geom_abline(
    intercept=m.sim.fe[1],
    slope=I(m.sim.fe[2] + m.sim.fe[3]*sd1_pop),
    color="red",size=1.5
    )
g.sim.diff
d.sim$lnvalue_ti_hat <- predict(m.sim)
d.sim[,c("r_0i","r_1i")] <- apply(m.sim.re,1,rep,each=T)
d.sim$e_ti <- d.sim$lnvalue_ti - (d.sim$lnvalue_ti_hat + d.sim$r_0i + d.sim$r_1i)
d.sim$rand_slope <- d.sim$r_1i * t

sapply(list(mean=mean(d.sim$r_0i),sd=sd(d.sim$r_0i)),round,4)
sapply(list(mean=mean(d.sim$r_1i),sd=sd(d.sim$r_1i)),round,4)

ggplot(d.sim,aes(x=t,y=rand_slope,group=i)) +
    geom_line() +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])


