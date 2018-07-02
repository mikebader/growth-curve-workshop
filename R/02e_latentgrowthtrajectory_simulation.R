#### SIMULATION EXAMPLE: LATENT GROWTH TRAJECTORY ANALYSIS ####
## Description: This file simulates a dataset of compliant and noncompliant
##              respondents and then creates different outcome trajectories
##              based on their compliance
## Author: Michael Bader

rm(list=ls())
source("R/_functions.R")
library(lme4)

## SIMULATE DATA BASED ON KNOWN PARAMETERS
## Set the number of individuals and the number of observations for each
## individual
N.i.com <- 40
N.t <- 10
t.fin <- N.t - 1 # This is the value of time for the final observation since
                 # time is indexed by 0

N.i.non <- 10

## Set deterministic parameters of the model. This is what we had before.
gamma00 <- 70
gamma10 <- 10

## Now we are going to make sure that we indicate which class they belong to
gamma00.com <- gamma00
gamma10.com <- gamma10

## Now we are going to create values for the non-compliant group
gamma00.non <- 80
gamma10.non <- 0

## Set stochastic parameters of the model. We are going to allow those
## to be the same across models because of our assumption of conditional
## independence
tau0    <- 10
tau1    <- 3
sigma   <- 2

## Simulate stochastic values for intercept and slope for commpliant group
u0.com <- rnorm(N.i.com,gamma00.com,tau0)
u0.i.com <- rep(u0.com,each=N.t)
u1.com <- rnorm(N.i.com,gamma10.com,tau1)
u1.i.com <- rep(u1.com,each=N.t)

## Do the same for noncompliant group
u0.non <- rnorm(N.i.non,gamma00.non,tau0)
u0.i.non <- rep(u0.non,each=N.t)
u1.non <- rnorm(N.i.non,gamma10.non,tau1)
u1.i.non <- rep(u1.non,each=N.t)

## Simulate stochastic values for individual observations
e  <- rnorm((N.i.com+N.i.non)*N.t,0,sigma)

## Make a dataset combining our compliant and noncompliant groups
df <- matrix(c(u0.i.com,u1.i.com),length(u0.i.com),2)
df <- rbind(df,matrix(c(u0.i.non,u1.i.non),length(u0.i.non),2))
df <- as.data.frame(df)
names(df) <- c('u0.i','u1.i')
df$e <- e

## Create time variable and person label
df$time <- rep(0:(N.t-1),N.i.com+N.i.non)
df$i    <- rep(1:(N.i.com+N.i.non),each=N.t)

## Create our outcome variable (this is what we actually observe in real life)
df$y <- df$u0.i + df$u1.i*df$time + df$e

## Plot each of the individual trajectories
## Compliant group: red
## Noncompliant group: blue
f.df <- by(df,df$i,function(data) fitted(lm(y~time,data=data)))
plot(0,0,type='n',
     xlim=c(-1,10),ylim=c(min(unlist(f.df)),max(unlist(f.df))),
     ylab='y',xlab='time')
l <- sapply(f.df[1:N.i.com],
            function(data) lines(c(0:t.fin),data,lty=2,col='red'))
l <- sapply(f.df[-1:-N.i.com],
            function(data) lines(c(0:t.fin),data,lty=2,col='blue'))

## What would happen if we estimated a single growth trajectory model?
m0 <- lmer(df$y ~ df$time + (1 + df$time | df$i))
summary(m0)
m0.fe <- fixef(m0)
lines(c(0,t.fin),c(m0.fe[1],m0.fe[1]+t.fin*m0.fe[2]),lwd=4)

## Now we estimate the latent growth trajectory model
library('lcmm')
m1.hlme <-hlme(y~time,subject='i',ng=2,mixture=~time,idiag=TRUE,data=df)
summary(m1.hlme)
lines(c(0,t.fin),c(m1.hlme$best[3],m1.hlme$best[3]+t.fin*m1.hlme$best[5]),
      lty=2,lwd=3,col='red')
lines(c(0,t.fin),c(m1.hlme$best[2],m1.hlme$best[2]+t.fin*m1.hlme$best[4]),
      lty=2,lwd=3,col='blue')

## Now we estimate the growth mixture model that includes stochastic terms
## (Growth Mixture Model)
m2.hlme <-hlme(y~time,subject='i',random=~time,ng=2,mixture=~time,idiag=TRUE,data=df)
summary(m2.hlme)
lines(c(0,t.fin),c(m2.hlme$best[2],m2.hlme$best[2]+t.fin*m2.hlme$best[4]),lty=3,lwd=3,col='green')
lines(c(0,t.fin),c(m2.hlme$best[3],m2.hlme$best[3]+t.fin*m2.hlme$best[5]),lty=3,lwd=3,col='orange')

