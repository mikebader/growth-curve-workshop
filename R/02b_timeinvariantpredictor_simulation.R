#### SIMULATION EXAMPLE: TIME-INVARIANT PREDICTORS ####
## Description: This file simulates a dataset on which a time-invariant
##              predictor (population) is associated with different
##              intercepts and slopes in 150 simulated metropolitan areas
## Author: Michael Bader

## PLAN THE POPULATION
rm(list=ls())
library(MASS)
library(lme4)
library(ggplot2)

month <- c(0:12)
N <- 150
N_t <- length(month)
t <- rep(month,N)
i <- rep(c(1:N),each=N_t)

lnpop_i <- rnorm(N,0,1.4) + 13

gamma_00 <- log(117)
gamma_01 <- 0.2
gamma_10 <- 0.005
gamma_11 <- 0.001
sigma_ti <- 0.002
tau_00 <- 0.005
tau_11 <- 0.004
tau_01 <- 0
Tau <- matrix(c(tau_00, tau_01, tau_01, tau_11), nrow=2)

## CONJURE THE POPULATION
metros <- mvrnorm(N, c(0,0), Tau)
var(metros)
beta_0 <- gamma_00 + gamma_01*lnpop_i + metros[,1]
beta_0i <- rep(beta_0, each= N_t)
beta_1 <- gamma_10 + gamma_11*lnpop_i + metros[,2]
beta_1i <- rep(beta_1, each= N_t)
d.sim <- data.frame(
    i, t,
    lnpop_i = rep(lnpop_i, each=N_t), 
    lnvalue_ti = beta_0i + beta_1i + rep(lnpop_i, each=N_t)
)

## DESCRIBE DATA
ggplot(data=d.sim,aes(x=t,y=lnvalue_ti,group=i)) +
    geom_line(aes(color=lnpop_i))

## ANALYZE DATA
## Analyze data predicting both stochastic component on intercept & on slope
m.sim <- lmer(lnvalue_ti ~ lnpop_i + t + I(t*lnpop_i) + (1 + t|i),data=d.sim)
summary(m.sim)
m.sim.fe <- fixef(m.sim)
m.sim.re <- ranef(m.sim)$i

mean_pop <- mean(lnpop_i)

## Reanalyze same data centering on the mean population (because
## estimating at population=1 does not help interpretation)
m.sim.cen <- lmer(lnvalue_ti ~ t + I(t*(lnpop_i-mean_pop)) + (1 + t|i),
                  data=d.sim)
summary(m.sim.cen)
m.sim.fe <- fixef(m.sim.cen)
m.sim.re <- ranef(m.sim.cen)$i

g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_ti,group=i)) +
    geom_line() +
    geom_abline(intercept=m.sim.fe[1],slope=m.sim.fe[2], color="orange", size=1.5) +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])
g.sim

sd1_pop <- sd(lnpop_i) + mean_pop
g.sim.diff <- g.sim +
    geom_abline(
        intercept=m.sim.fe[1], slope=m.sim.fe[2] + m.sim.fe[3]*sd1_pop,
        color="red",size=1.5
    )
g.sim.diff
d.sim$lnvalue_ti_hat <- predict(m.sim)
d.sim[,c("r_0i","r_1i")] <- apply(m.sim.re,1,rep,each=N_t)
d.sim$e_ti <- d.sim$lnvalue_ti - (d.sim$lnvalue_ti_hat + d.sim$r_0i + d.sim$r_1i)
d.sim$rand_slope <- d.sim$r_1i * t

sapply(list(mean=mean(d.sim$r_0i),sd=sd(d.sim$r_0i)),round,4)
sapply(list(mean=mean(d.sim$r_1i),sd=sd(d.sim$r_1i)),round,4)

ggplot(d.sim,aes(x=t,y=rand_slope,group=i)) +
    geom_line() +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])


