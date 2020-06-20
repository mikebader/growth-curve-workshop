#### SIMULATION EXAMPLE: LINEAR TREND ####
## Description: This file simulates a dataset with a single linear trend and
##              estimates that trend
## Author: Michael Bader

rm(list=ls())
source("R/_functions.R")
library(ggplot2) # This loads a library that makes prettier plots than standard R

## PLAN POPULATION
month <- c(0:23)
beta0 <- log(500)     ## $500/square foot
beta1 <- 0.03/12      ## 3% annual increase
sigma <- 0.03/48      ## 0.25% fluctuation off of trend in given month

## CONJURE POPULATION
epsilon_t = rnorm(24,0,sigma)
price_t <- beta0 + beta1*month + epsilon_t
d.nyc.sim <- data.frame(month,price_t)
qplot(x=month,y=price_t)

## ANALYZE POPULATION
m.nyc.sim <- lm(price_t ~ month)
summary(m.nyc.sim)
betahats <- coef(m.nyc.sim)
d.nyc.sim$price_t_hat <- predict(m.nyc.sim)
d.nyc.sim
ggplot(d.nyc.sim,aes(x=month,y=price_t)) +
    geom_point(size=.75)+
    geom_abline(intercept=betahats[1],slope=betahats[2],color="orange") +
    scale_x_continuous(breaks=seq(0,23,1),labels=rep(month.abb,3)[4:27]) +
    labs(
        y="Logged price",
        x="Month"
    )

d.nyc.sim$e_t <- d.nyc.sim$price_t - d.nyc.sim$price_t_hat
sapply(list(sum=sum(d.nyc.sim$e_t),sd=sd(d.nyc.sim$e_t)),round,4)
