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

N <- 150
N_t <- 12
pop_i <- rgamma(N, 1.5, 0.1)*20000 ## Fake distribution of metro populations

gamma_00 <- 0        ## Value is zero when population equals zero
gamma_01 <- .37      ## Effect of lnpop on intercept
gamma_10 <- 0.03/12    ## Monthly percentage growth
gamma_11 <- 0.01/12   ## Additional growth for each percent increase in lnpop
sigma_e  <- 0.0001
tau_00   <- 0.100^2
tau_11   <- 0.005^2
tau_01   <- 0
Tau      <- matrix(c(tau_00, tau_01, tau_01, tau_11), nrow=2)

## Draw random sample of level-2 components 
rhos <- mvrnorm(N, c(0,0), Tau)

beta_0 <- rep(gamma_00 + gamma_01*log(pop_i) + rhos[,1], each=N_t)
beta_1 <- rep(gamma_10 + gamma_11*log(pop_i) + rhos[,2], each=N_t)

d.sim <- data.frame(
    i=rep(1:N, each=N_t), 
    month=rep(0:(N_t-1), N),
    lnpop_i=rep(log(pop_i), each=N_t)
)
d.sim$lnvalue_ti <- beta_0 + beta_1*d.sim$month + rnorm(N*N_t, 0, sigma_e)
head(d.sim, 24)

## DESCRIBE DATA
## Plot individual trajectories
g.desc <- ggplot(d.sim, aes(x=month, y=lnvalue_ti, group=i)) + 
    geom_line() 
g.desc 

## Estimate regression for each group i, accounting for lnpop
betas <- sapply(unique(d.sim$i), function(i){
    coef(lm(lnvalue_ti ~ month, data=d.sim[d.sim$i==i,]))
})

## Plot distribution of intercepts
qplot(betas[1,], bins=15) +
    geom_vline(xintercept = mean(betas[1,]), color="orange", size=1.5) 

## Plot distribution of slopes
qplot(betas[2,], bins=15) + 
    geom_vline(xintercept = mean(betas[2,]), color="orange", size=1.5) +
    scale_x_continuous(breaks=c(-.02,.03,.005))

## ANALYZE THE DATA
m.sim <- lmer(lnvalue_ti ~ lnpop_i*month + (1 + month | i), data=d.sim)
summary(m.sim)

## It might help to interpret the results if the intercept was not 
## when (logged) population equals zero, but at the mean population across 
## metropolitan areas
d.sim$lnpop_i_orig <- d.sim$lnpop_i
d.sim$lnpop_i <- d.sim$lnpop_i - mean(d.sim$lnpop_i)
m.ctr <- lmer(lnvalue_ti ~ lnpop_i*month + 
                  (1 + month | i), data=d.sim)
summary(m.ctr)
huxtable::huxreg(m.sim, m.ctr)

## Let's use the centered model to plot the predicted value on top of the 
## descriptive data that we plotted before to see how well the model 
## represents the data 
g.centered <- g.desc + 
    geom_abline(
        intercept=fixef(m.ctr)[1],
        slope=fixef(m.ctr)[3],
        color="orange", size=3
    )
g.centered

## We can show that this centered model is the same as the original model
## evaluated at the mean value of the logged population
g.orig <- g.centered +
    geom_abline(
        intercept=fixef(m.sim)[1] + fixef(m.sim)[2]*mean(d.sim$lnpop_i_orig),
        slope=fixef(m.sim)[3] + fixef(m.sim)[4]*mean(d.sim$lnpop_i_orig),
        color="red", size=1
    )
g.orig

## To show the influence of population size on the change in housing values
## we will show the predicted values of logged housing prices at one
## standard deviation above and below the mean population
m <- 0:12
sd_lnpop <- sd(d.sim$lnpop_i)
g.pres <- ggplot() +
    scale_x_continuous(limits = c(0,11)) +
    scale_y_continuous(limits = c(4, 5.1)) +
    geom_abline(intercept = fixef(m.ctr)[1], slope = fixef(m.ctr)[3]) + ## Mean pop
    geom_abline(
        intercept = fixef(m.ctr)[1] + fixef(m.ctr)[2]*sd_lnpop,
        slope = fixef(m.ctr)[3] + fixef(m.ctr)[4]*sd_lnpop,
        linetype = 2
    ) +
    geom_abline(
        intercept = fixef(m.ctr)[1] + fixef(m.ctr)[2]*-1*sd_lnpop,
        slope = fixef(m.ctr)[3] + fixef(m.ctr)[4]*-1*sd_lnpop,
        linetype = 3
    ) +
    labs(
        title = "Predicted logged values of price/sq.ft. of metro areas",
        subtitle = "At mean and one standard deviation above/below of logged population",
        caption = "Note: Based on entirely fake, simulated data",
        y = "Logged price/sq.ft.",
        x = "Month"
    )
g.pres 
