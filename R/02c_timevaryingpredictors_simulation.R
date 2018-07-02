#### SIMULATION EXAMPLE: TIME-VARYING PREDICTORS ####
## Description: This file simulates three different types of time-varying
##              predictors to demonstrate the kinds of time-related
##              changes that can be modeled simulating a class with 32
##              sessions
##              Simulation 1: Simulates an intervention that increases
##                            the level of comprehension after 16 sessions
##              Simulation 2: Simulates increase in rate of comprehension
##                            after 16 sessions
##              Simulation 3: Simulates increases in both the level of
##                            comprehension and the rate of comprehension
##                            after 16 sessions
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2)
set.seed(613589556)

T <- 32
t <- c(1:T)

gamma_00 <- 20
gamma_10 <- 1.5
sigma_t <- 1.5
e_t <- rnorm(T,0,sigma_t)

conf.orig <- gamma_00 + gamma_10*t + e_t

df <- data.frame(t,conf.orig)

g.base <- ggplot(data=df,aes(x=t,y=conf.orig)) +
    geom_smooth(method="lm",se=FALSE,col="white",size=2) +
    geom_point()
g.base

## Intervention 1: Intervention after class session 16 (t=16) &
##   increases  by 2
df$day2 <- c(rep(FALSE,16),rep(TRUE,T-16))
gamma_20 <- 2
df$conf.day2 <- df$conf.orig + gamma_20*df$day2

m.sim <- lm(conf.day2 ~ t + day2,data=df)
summary(m.sim)

df$conf.day2.pred <- predict(m.sim)

g.adj <- g.base + geom_line(data=df,aes(y=conf.day2.pred),color="red")
g.adj

## Intervention 2: Intervention after class session #8,
##   increases slope by .25
df$code <- df$t - 8
df$code[df$code<0] <- 0
gamma_20 <- .25
df$conf.code <- df$conf.orig + gamma_20*df$code

m.sim <- lm(conf.code ~ t + code ,data=df)
summary(m.sim)

df$conf.code.pred <- predict(m.sim)

g.defl <- g.adj + geom_line(data=df,aes(y=conf.code.pred), color="#00aa00") ## Green
g.defl

## Intervention 3: getting code adjusted level of confidence
##   and increased rate of improvement on day 2
gamma_30 <- 2
df$conf.both <- df$conf.orig + gamma_20*df$code + gamma_30*df$day2

d.sim <- data.frame(conf.both=rowsum(gamma))
m.sim <- lm(conf.both ~ t + code + day2 ,data=df)
summary(m.sim)

df$conf.both.pred <- predict(m.sim)

g.both <- g.defl + geom_line(data=df,aes(y=conf.both.pred), color="blue")
g.both
