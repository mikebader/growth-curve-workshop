#### SIMULATION EXAMPLE: GROWTH MIXTURE MODELS ####
## Description: This file simulates a dataset of neighborhoods with three 
##              different trajectories of change in the share of the white 
##              population and simulates a growth mixture model
## Author: Michael Bader

source("_functions.R")
library(tidyverse)
library(MASS)
library(lme4)
library(huxtable)
library(lcmm)

theme_set(theme_minimal())

## CONJURE THE POPULATION
## There are three groups of tracts:
## 100 tracts that start off 90% white and stay 90% white after three decades
##  50 tracts that start off 10% white and stay 10% white after three decades
##  25 tracts that start off 90% white and end up 10% white after three decades
## Time is measured by decades
N_t <- 3
t <- 0:2
sigma = 0.1

N_wht <- 100       ## Number of tracts simulated to follow this trajectory
gamma00_wht <- 90  ## Intercept
gamma10_wht <- 0   ## Slope
tau00_wht <- 2^2   ## Variance around intercept
tau11_wht <- 0.5^2 ## Variance around slope
tau10_wht <- .2 * sqrt(tau00_wht*tau11_wht) ## Covariance of rhos
Tau_wht <- matrix(c(tau00_wht, tau10_wht, tau10_wht, tau11_wht), nrow=2)
Rho_wht <- MASS::mvrnorm(N_wht, c(0, 0), Tau_wht)
wht <- rep(gamma00_wht + Rho_wht[,1], each = N_t) +                 # intercepts
       rep(gamma10_wht + Rho_wht[,2], each = N_t) * rep(t, N_wht) + # slopes
       rnorm(N_wht * N_t, 0, sigma)

N_blk <- 50
gamma00_blk <-  10
gamma10_blk <-   0
tau00_blk <- 2^2
tau11_blk <- 1^2 
tau10_blk <- -.2 * sqrt(tau00_blk * tau11_blk)
Tau_blk <- matrix(c(tau00_blk, tau10_blk, tau10_blk, tau11_blk), nrow=2)
Rho_blk <- MASS::mvrnorm(N_blk, c(0,0), Tau_blk)
blk <- rep(gamma00_blk + Rho_blk[,1], each = N_t) +                 # intercepts
       rep(gamma10_blk + Rho_blk[,2], each = N_t) * rep(t, N_blk) + # slopes
       rnorm(N_blk * N_t, 0, sigma)

N_chg <- 25
gamma00_chg <-  90
gamma10_chg <- -40
tau00_chg <- 1.25^2
tau11_chg <- 3^2
tau10_chg <- -.2 * sqrt(tau00_chg * tau11_chg)
Tau_chg <- matrix(c(tau00_chg, tau10_chg, tau10_chg, tau11_chg), nrow=2)
Rho_chg <- MASS::mvrnorm(N_chg, c(0, 0), Tau_chg)
chg <- rep(gamma00_chg + Rho_chg[,1], each = N_t) +                 # intercepts
       rep(gamma10_chg + Rho_chg[,2], each = N_t) * rep(t, N_chg) + # slopes
       rnorm(N_chg * N_t, 0, sigma)

N <- sum(N_wht, N_blk, N_chg)
d_sim <- tibble(
    i     = rep(c(1:N), each=N_t),
    nhd_i = as.factor(i),
    t     = rep(t, N),
    pwht  = c(wht, blk, chg)
)
print(d_sim, n=20)

p <- ggplot(d_sim, aes(x=t, y=pwht, group=nhd_i)) + 
    geom_line(size=.1) +
    scale_y_continuous(limits=c(0,100)) +
    labs(
        title = "175 simulated neighborhoods",
        x = "Decade",
        y = "Percent non-Hispanic white"
    )
p

## ANALYZE THE POPULATION
d_sim <- as.data.frame(d_sim) # Data frames rather than tibbles are required for
# hlme command

# Model a single class
m1 <- hlme(pwht ~ t, subject = 'i', random = ~t, data = d_sim, ng = 1)

# Model two classes (note the mixture parameter is the same as the random 
# component when we estimated our model using `lmer`)
m2 <- hlme(pwht ~ t, subject = 'i', data = d_sim, ng = 2, 
           mixture = ~1 + t, random = ~1 + t, B=random(m1))
# Compare the model fit
summarytable(m1, m2,
             which = c("G", "loglik", "conv", "AIC", "BIC", "entropy", "%class"))

# Model three classes and compare the model fit
m3 <- hlme(pwht ~ t, subject = 'i', data = d_sim, ng = 3, 
           mixture = ~1 + t, random = ~1 + t, B=random(m1))
summarytable(m1, m2, m3,
             which = c("G", "loglik", "conv", "AIC", "BIC", "entropy", "%class"))

# Model four classes and compare the model fit
m4 <- hlme(pwht ~ t, subject = 'i', data = d_sim, ng = 4, 
           mixture = ~1 + t, random = ~1 + t, B=random(m1))
summarytable(m1, m2, m3, m4,
             which = c("G", "loglik", "conv", "AIC", "BIC", "entropy", "%class"))

## Summarize the best-fitting model
summary(m3)

## Plot the predicted values over the observed data
m3p <- predictY(m3, data.frame(t=0:2), var.time = 't')
pr <- as_tibble(m3p$pred) %>% 
    mutate(t=as.vector(m3p$times[,1])) %>%
    pivot_longer(cols=starts_with("Ypred"), names_to="class", values_to="pwht")

p + geom_line(data=pr, aes(x=t, y=pwht, color=class, group=class), size=1.2) +
    labs(
        title = "Estimated trajectories for classes of LGCA",
        subtitle = "175 simulated tracts"
    )

g.pred <- p + 
    geom_line(data=pr, aes(x=t, y=pwht, color=class, group=class), size=1.2) +
    labs(
        title = "Simulated data following a mixed growth distribution",
        x = "Time",
        y = "Outcome"
    ) +
    theme(legend.position = "bottom")
g.pred
ggsave("../images/sims/gmm.pdf", plot = g.pred, 
       height = 6, width = 9, units = "in")
