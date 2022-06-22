#### SIMULATION EXAMPLE: RANDOM INTERCEPTS ####
## Description: This file simulates a dataset where trends start at different
##              intercept but change at same rate for 150 metros and analyzes
##              that data
## Author: Michael Bader

source("_functions.R")
library(tidyverse)
library(broom)

## PLAN POPULATION
month <- c(0:12)
N   <- 150
N_t <- length(month)
t   <- rep(month, N)
i   <- as.factor(rep(c(1:N), each=N_t)) 

sigma_it <- 0.002
tau_0i   <- 0.10

beta_0  <- log(100) + rnorm(N,0,tau_0i)
beta_1  <- 0.005

## CONJURE POPULATION
beta_0i <- rep(beta_0,each=N_t)
lnvalue_it <- beta_0i + beta_1*t + rnorm(N*N_t,0,sigma_it)
d.sim <- data.frame(i,t,lnvalue_it)

g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_it,group=i)) + geom_line() +
    labs(
        title = "Simulated data with random intercepts",
        y = "Outcome", 
        x = "Time"
    )
g.sim

## PREDICT DATA
m.sim.i <- d.sim %>%
    nest_by(i)%>%
    mutate(mod = list(lm(lnvalue_it ~ t, data = data))) %>%
    summarize(tidy(mod)) %>%
    pivot_wider(
        id_cols = "i", names_from = "term", values_from = "estimate"
    ) %>%
    rename(beta_0 = `(Intercept)`, beta_1 = t) %>%
    ungroup()

ggplot(m.sim.i, aes(x = beta_0)) +
    geom_histogram(bins=12) +
    geom_vline(xintercept = mean(m.sim.i$beta_0), size=1, color = "orange") +
    labs(
        title = expression("Distribution of "*beta[1]*"s in simulated data"),
        x = expression(beta[0]),
        y = "Count"
    )

gamma_00 <- mean(m.sim.i$beta_0)
gamma_01 <- mean(m.sim.i$beta_1)
round(c(gamma_00,gamma_01),4)

m.sim.i$rho_i <- m.sim.i$beta_0 - gamma_00
sapply(list(mean=mean(m.sim.i$rho_i),sd=sd(m.sim.i$rho_i)),round,4)


d.sim <- merge(d.sim,m.sim.i,by="i")
d.sim$lnvalue_it_hat <- gamma_00 + gamma_01*t
d.sim$e_it <- d.sim$lnvalue_it - d.sim$lnvalue_it_hat - d.sim$rho_i

sapply(list(mean=mean(d.sim$e_it),sd=sd(d.sim$e_it)),round,4)


library(lme4)
m.sim <- lmer(lnvalue_it ~ t + (1 | i),data=d.sim)
summary(m.sim)
m.sim.fe <- round(fixef(m.sim),4)

g.sim.pred  <- g.sim + geom_abline(
                        intercept=m.sim.fe[["(Intercept)"]],
                        slope=m.sim.fe[["t"]],col="orange",size=1.2
                        )
g.sim.pred
ggsave("../images/sims/random_intercepts.pdf", plot = g.sim.pred,
       width = 9, height = 6, units = "in")
