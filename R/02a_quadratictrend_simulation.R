#### SIMULATION EXAMPLE: QUADRATIC TRENDS ####
## Description: This file simulates a dataset where trends start at different
##              intercept and change at different rate for 150 metros, and
##              change is nonlinear (quadratic), then analyzes that data
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(MASS)
library(lme4)
library(ggplot2)

## PLAN POPULATION
month <- c(0:66)
N <- 150
T <- length(month)
t <- rep(month,N)
i <- rep(c(1:N),each=T)

gamma_00 <- log(117)
gamma_10 <- 0.005
gamma_20 <- -0.0002

sigma_ti <- 0.002
tau_0i <- 0.10
tau_1i <- 0.004
tau_01 <- 0

Sigma <- sapply(c(tau_0i,tau_01,tau_01,tau_1i),function(x){x^2})

## CONJURE THE POPULATION
metros <- mvrnorm(N,c(0,0),matrix(Sigma,nrow=2))
var(metros)
beta_0 <- gamma_00 + metros[,1]
beta_1 <- gamma_10 + metros[,2]
beta_2 <- gamma_20

lnvalue_ti <- rep(beta_0,each=T) + rep(beta_1,each=T)*t + rep(gamma_20,each=T)*t^2 + rep(rnorm(N*T,0,sigma_ti))
d.sim <- data.frame(i,t,lnvalue_ti)

## ANALYZE DATA
m.sim <- lmer(lnvalue_ti ~ t + I(t^2) + (1 + t|i),data=d.sim)
## Notice error that the data should be scaled; let's divide t^2 by 100
m.sim <- lmer(lnvalue_ti ~ t + I((t^2)/100) + (1 + t|i),data=d.sim)
summary(m.sim)
m.sim.fe <- fixef(m.sim)
m.sim.re <- ranef(m.sim)$i

## When fitting the model, need to remember to scale by same factor as modeled
d.fit <- data.frame(month=c(0:T),month2=((c(0:T)^2)/100))
d.fit$best_fit <- matrix(c(rep(1,length(0:T)),d.fit$month,d.fit$month2),ncol=3) %*% matrix(m.sim.fe,ncol=1)

g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_ti,group=i)) +
    geom_line() +
    geom_line(
        data=d.fit,
        aes(x=month,y=best_fit,group=NULL),
        col="orange",size=1.5
    ) +
    scale_x_continuous(breaks=seq(0,T,12),labels=paste("Apr",c(2004:2009)))
g.sim


