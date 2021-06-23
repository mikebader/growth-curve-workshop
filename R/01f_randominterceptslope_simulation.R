#### SIMULATION EXAMPLE: RANDOM INTERCEPTS & SLOPES ####
## Description: This file simulates a dataset where trends start at different
##              intercepts and changes at different rate for 150 metros and
##              analyzes that data
## Author: Michael Bader

source('_functions.R')
library(tidyverse) 
library(lme4)      ## Used to estimate growth models
library(MASS)      ## This library provides a method to draw from a multivariate 
## normal distribution

#### SIMULATION EXAMPLE: RANDOM INTERCEPTS & SLOPES
## PLAN THE POPULATION
month <- c(0:23)
N  <- 150
N_t <- length(month)
t  <- rep(month,N)
i  <- as.factor(rep(c(1:N),each=N_t))

gamma_00 <- log(117)
gamma_10 <- 0.005
sigma_ti <- 0.002
tau_00   <- 0.02^2    ## Note that t_00, tau_11, and t_01 represent
tau_11   <- 0.001^2  ## *variances/covariances* not standard deviations
corr_01  <- 0.4        ## *Correlation* between rho0i and rho1i 
## Multiply by sd of rho0i and rho1i to get covariance
tau_01   <- corr_01 * sqrt(tau_00) * sqrt(tau_11)
Tau   <- matrix(c(tau_00,tau_01,tau_01,tau_11),nrow=2) 

## CONJURE THE POPULATION
## Set metropolitan area characteristics
Rho <- mvrnorm(N,c(0,0),Tau)
## Compare the draw from multivariate normal distribution to our values
var(Rho)
c("t00" = tau_00, "t11" = tau_11, "t01" = tau_01)

## Assign beta values to each of our simulated metropolitan areas
beta_0i <- gamma_00 + Rho[,1]
beta_1i <- gamma_10 + Rho[,2]

## Create individual change trajectories
lnvalue_ti <- rep(beta_0i,each=N_t) + rep(beta_1i,each=N_t)*t + rnorm(N*N_t,0,sigma_ti)
d.sim <- tibble(i,t,lnvalue_ti)
print(d.sim, n = 30)

## DESCRIBE THE DATA
## List values for each parameter
g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_ti,group=i)) + 
    geom_line() +
    labs(
        title = "Simulated data with random intercepts and slopes",
        x = "Time",
        y = "Outcome"
    )
g.sim

## Show a random sample of trajectories to see individual lines
g.samp <- ggplot(filter(d.sim, i %in% sample(levels(i), 40)),
                 aes(x=t,y=lnvalue_ti,group=i)) + geom_line()
g.samp

## Estimate multilevel model
m.sim <- lmer(lnvalue_ti ~ t + (1 + t|i),data=d.sim)
summary(m.sim)

## Record estiamtes of the deterministic and stochastic components
m.sim.fe <- fixef(m.sim)     ## Deterministic (note length=2)
m.sim.re <- ranef(m.sim)$i   ## Stochastic (note dimensions=150 rows, 2 cols)

var(m.sim.re)  ## Calculate the variance-covariance of random effects
Tau            ## Compare to `Tau` above

## Plot predicted values from the fixed effects estimates of the model
g.pred <- geom_abline(intercept=m.sim.fe[1],slope=m.sim.fe[2],
                color="orange",size=1.5) 
g.sim + g.pred
ggsave("../images/sims/random_intercepts_slopes.pdf", plot = g.sim + g.pred,
       width = 9, height = 6, units = 'in')

## Record predicted values and total error
d.sim$lnvalue_ti_hat <- predict(m.sim,re.form=NA) ## Predicted values
d.sim$e_tot <- d.sim$lnvalue_ti - d.sim$lnvalue_ti_hat ## Total error for each time

## Record stochastic components of the model
d.sim$r_0i   <- predict(m.sim,re.form=~(1|i),random.only=TRUE)   # rho_0i
d.sim$r_1i   <- ranef(m.sim)$i[,2][d.sim$i]                      # rho_1i
d.sim$r_1iXt <- predict(m.sim,re.form=~(0+t|i),random.only=TRUE) # rho_1i X time
d.sim$e_ti   <- (d.sim$e_tot - d.sim$r_0i - d.sim$r_1iXt)        # e_it

## Draw 5 example trends and show how they relate to the predicted trend
i_ex <- sample(unique(d.sim$i), 5)
d.sim$i <- factor(d.sim$i)
g.ex <- ggplot(d.sim[d.sim$i%in%i_ex,],aes(x=t,y=lnvalue_ti,group=i,linetype=i)) +
    g.pred + geom_line() +
    scale_linetype_manual(values=1:5,labels=letters[1:5])
g.ex

## Create table of estimates of beta_0i and beta_1i for each of the example
## trends
d.ex <- m.sim.re[i_ex,]
rownames(d.ex) <- letters[1:5]
names(d.ex) <- c("beta_0i","beta_1i")
print(d.ex, digits=4)
