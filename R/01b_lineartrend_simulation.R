#### SIMULATION EXAMPLE: LINEAR TREND ####
## Description: This file simulates a dataset with a single linear trend and
##              estimates that trend
## Author: Michael Bader

rm(list=ls())
source("../_functions.R")
library(ggplot2) # This loads a library that makes prettier plots than standard R

## PLAN POPULATION
month <- c(0:12)
b_0 <- log(500)     ## $500/square foot
b_1 <- 0.03/12      ## 3% annual increase
sigma = 0.03/48     ## 0.25% fluctuation in given month

## CONJURE POPULATION
price_t <- b_0 + b_1*month + rnorm(13,0,sigma)
d.nyc.sim <- data.frame(month,price_t)
qplot(x=month,y=price_t)

## ANALYZE POPULATION
m.nyc.sim <- lm(price_t ~ month)
m.nyc.sim
betas <- coef(m.nyc.sim)
d.nyc.sim$price_t_hat <- predict(m.nyc.sim)
d.nyc.sim
ggplot(d.nyc.sim,aes(x=month,y=price_t)) +
    geom_point()+
    geom_abline(intercept=betas[1],slope=betas[2],color="orange") +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17]) +
    labs(
        y="Logged price",
        x="Month"
    )

d.nyc.sim$e_t <- d.nyc.sim$price_t - d.nyc.sim$price_t_hat
sapply(list(sum=sum(d.nyc.sim$e_t),sd=sd(d.nyc.sim$e_t)),round,4)
