#### SIMULATION EXAMPLE: LATENT CLASS GROWTH ANALYSIS ####
## Description: This file simulates a dataset of neighborhoods with three 
##              different trajectories of change in the share of the white 
##              population and simulates a latent class growth analysis
## Author: Michael Bader

source("_functions.R")
library(tidyverse)
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

N_wht <- 100
beta0_wht <-  90
beta1_wht <-   0
wht <- beta0_wht + beta1_wht*rep(t, N_wht) + runif(N_wht, min=-3, max=3)

N_blk <- 50
beta0_blk <-  10
beta1_blk <-   0
blk <- beta0_blk + beta1_blk*rep(t, N_blk) + runif(N_blk, min=-3, max=3)

N_chg <- 25
beta0_chg <-  90
beta1_chg <- -40
chg <- beta0_chg + beta1_chg*rep(t, N_chg) + runif(N_chg, min=-3, max=3)

N <- sum(N_wht, N_blk, N_chg)
d_sim <- tibble(
    i     = rep(c(1:N), each=N_t),
    nhd_i = as.factor(i),
    t     = rep(t, N),
    pwht  = c(wht, blk, chg)
)
head(d_sim)

p <- ggplot(d_sim, aes(x=t, y=pwht, group=nhd_i)) + 
    geom_line(size=.1) +
    scale_y_continuous(limits=c(0,100)) +
    labs(
        title = "175 simulated neighborhoods",
        x = "Decade",
        y = "Percent non-Hispanic white"
    )
p

m0 <- lmer(pwht ~ t + (1 + t | nhd_i), data=d_sim)
huxreg(m0, coefs=c("Intercept"="(Intercept)", "Decade" = "t"))

m0_fe <- lme4::fixef(m0)
p + geom_abline(
    intercept = m0_fe[1], slope = m0_fe[2],
    color="orange", size=1.2
    )

## Plot distribution of neighborhood-specific intercept errors
m0_re <- lme4::ranef(m0)
names(m0_re$nhd_i) <- c("rho0i", "rho1i")
ggplot(m0_re$nhd_i, aes(x=rho0i)) +
    geom_histogram(bins=100) +
    labs(
        title = "Distribution of tract-specific intercept errors (rho_0i)",
        x = expression(rho['0i'], parse=TRUE)
    )
## Plot distribution of neighborhood-specific slope errors
ggplot(m0_re$nhd_i, aes(x=rho1i)) +
    geom_histogram(bins=100) +
    labs(
        title = "Distribution of tract-specific slope errors (rho_1i)",
        x = expression(rho['1i'], parse=TRUE)
    )

## Plot relationship between neighborhood-specific intercept & slope errors
ggplot(m0_re$nhd, aes(x=rho0i, y=rho1i)) +
    geom_density_2d() +
    geom_point(size=.5) +
    labs(
        title = "Relationship between intercept and slope errors",
        x = expression(rho['0i'], parse=TRUE),
        y = expression(rho['1i'], parse=TRUE)
    )

## ANALYZE THE POPULATION
d_sim <- as.data.frame(d_sim) # Data frames rather than tibbles are required for
                              # hlme command

# Model a single class
m1 <- hlme(pwht ~ t, subject = 'i', data = d_sim, ng = 1)

# Model two classes (note the mixture parameter is the same as the random 
# component when we estimated our model using `lmer`)
m2 <- hlme(pwht ~ t, subject = 'i', data = d_sim, ng = 2, 
           mixture = ~1 + t, B=random(m1))
# Compare the model fit
summarytable(m1, m2,
             which = c("G", "loglik", "conv", "AIC", "BIC", "entropy", "%class"))

# Model three classes and compare the model fit
m3 <- hlme(pwht ~ t, subject = 'i', data = d_sim, ng = 3, 
           mixture = ~1 + t, B=random(m1))
summarytable(m1, m2, m3,
             which = c("G", "loglik", "conv", "AIC", "BIC", "entropy", "%class"))

# Model four classes and compare the model fit
m4 <- hlme(pwht ~ t, subject = 'i', data = d_sim, ng = 4, 
           mixture = ~1 + t, B=random(m1))
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
        title = "Simulated data following a latent class growth distribution",
        x = "Time",
        y = "Outcome"
    ) +
    theme(legend.position = "bottom")
g.pred
ggsave("../images/sims/lcga.pdf", plot = g.pred, 
       height = 6, width = 9, units = "in")

## Calculate probabilities for each class
## The estimates are stored in the attribute `best` of the model object
denom <- sum(sapply(c(m3$best[1:2], 0), exp))
pi_i1 <- exp(m3$best[1])/denom
pi_i2 <- exp(m3$best[2])/denom
pi_i3 <- exp(0)/denom
c(pi_i1, pi_i2, pi_i3)



