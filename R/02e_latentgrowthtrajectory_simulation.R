#### SIMULATION EXAMPLE: LATENT GROWTH TRAJECTORY ANALYSIS ####
## Description: This file simulates a dataset of compliant and noncompliant
##              respondents and then creates different outcome trajectories
##              based on their compliance
## Author: Michael Bader

rm(list=ls())
set.seed(-1864853683) ## Set seed to ensure reproducibility of simulations
library(lme4)
library(ggplot2)

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
gamma10 <- 12.5

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
df <- data.frame(
    grp=c(rep("com", N.i.com*N.t), rep("non", N.i.non*N.t)),
    u0.i= c(u0.i.com, u0.i.non),
    u1.i= c(u1.i.com, u1.i.non),
    e.it= e
)

## Create time variable and person label
df$time <- rep(0:(N.t-1),N.i.com+N.i.non)
df$i    <- rep(1:(N.i.com+N.i.non),each=N.t)

## Create our outcome variable (this is what we actually observe in real life)
df$y <- df$u0.i + df$u1.i*df$time + df$e

## Plot each of the individual trajectories
## Compliant group: red
## Noncompliant group: blue
f.df <- by(df,df$i,function(data) fitted(lm(y~time,data=data)))
g.base <- ggplot(df, aes(x=time, y=y, group=i)) +
    geom_smooth(method="lm", se=FALSE, size=.5, linetype=2, 
                color="#bc509088") 
g.base

## What would happen if we estimated a single growth trajectory model?
m0 <- lmer(df$y ~ df$time + (1 + df$time | df$i))
summary(m0)
m0.fe <- lme4::fixef(m0)
g.lga <- g.base + 
    geom_abline(intercept=m0.fe[1], slope=m0.fe[2], color="#bc5090ff", size=1.5)
g.lga

## Now we estimate the growth mixture model model
library('lcmm')
m1.hlme <-hlme(y~time, subject='i', ng=2, 
               mixture=~1+time, idiag=TRUE, random=~1+time, data=df)
summary(m1.hlme)
gammas1 <- m1.hlme$best[c(2,4)] ## Gamma values for class 1
gammas2 <- m1.hlme$best[c(3,5)] ## Gamma values for class 2

## Plot trajectories for each class
g.gmm = g.lga +
    geom_abline(aes(intercept = gammas1[1], slope = gammas1[2]), 
                color="#ffa600", size=2) +
    geom_abline(aes(intercept = gammas2[1], slope = gammas2[2]), 
                color="#003f5c", size=2) 
g.gmm

## Gather the predicted probabilities of class membership
## Variables:
##   i = Grouping variable (will have the same name as the variable used 
##       in option `subject` in the model call above)
##   class: Class with the highest predicted probability of membership
##   prob<N>: Probability that the group belongs to class <N>
pprob <- m1.hlme$pprob

## Since we know "Truth", we can see how well the model predicts class
## membership into the classes that we assigned
pred <- merge(df, pprob, "i")
table(pred$grp, pred$class)

## We can also graph the trajectories based on predicted class membership 
## to see how well each trajectory summarizes individuals predicted to be 
## in the class
g.classes <- ggplot(pred, aes(x=time, y=y, color=factor(class), group=i)) +
    geom_line(linetype=2) +
    geom_abline(aes(intercept = gammas1[1], slope = gammas1[2]), 
                color="#003f5c", size=2) +
    geom_abline(aes(intercept = gammas2[1], slope = gammas2[2]), 
                color="#ffa600", size=2) +
    scale_color_manual(values = c("#374c80", "#ff764a"))
g.classes

## CHECK NUMBER OF CLASSES
## Run the model estimating one additional class and compare the AIC/BIC
## values
m2.hlme <-hlme(y~time, subject='i', ng=3, 
               mixture=~1+time, idiag=TRUE, random=~1+time, data=df)
summary(m2.hlme)

list("AIC" = c(`two class`=m1.hlme$AIC, `three class`=m2.hlme$AIC),
     "BIC" = c(`two class`=m1.hlme$BIC, `three class`=m2.hlme$BIC))
