#### ANALYSIS EXAMPLE: LATENT CLASS GROWTH ANALYSIS ####
## Description: This file analyses a dataset of neighborhoods in DC 
##              different trajectories of change in the share of the white 
##              population from 1990 to 2010
## Author: Michael Bader

source("_functions.R")
library(tidyverse)
library(lcmm)

load("../data/dc-nhw-population.Rdata")

## DESCRIBE THE DATA
p_base <- ggplot(dcpop, aes(x = t, y = pnhw, group = i)) +
    labs(
        title = "Percent non-Hispanic white in DC neighborhoods 1990-2010",
        y = "Percent non-Hispanic white",
        x = "Year") +
    ylim(0, 100) +
    scale_x_continuous(breaks = 0:2, labels = seq(1990, 2010, 10))
p_ana <- p_base + geom_line(size=0.1) 
p_ana

## ANALYZE THE DATA USING LCGA
fitnames <- c("G", "npm", "loglik", "conv", "AIC", "BIC", "entropy", "%class")
dcpop <- as.data.frame(dcpop)
m1 <- hlme(pnhw ~ t, subject = "i", data = dcpop)
m2 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, data = dcpop, ng = 2, B = random(m1))
m3 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, data = dcpop, ng = 3, B = random(m1))
summarytable(m2, m3,
             which = fitnames)
m4 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, data = dcpop, ng = 4, B = random(m1))
summarytable(m2, m3, m4,
             which = fitnames)

m5 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, data = dcpop, ng = 5, B = random(m1))
summarytable(m2, m3, m4, m5,
             which = fitnames)
m5g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, data = dcpop, ng = 5), 
    rep=100, maxiter=30, minit=m1)
summarytable(m2, m3, m4, m5, m5g,
             which = fitnames)

m6g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, data = dcpop, ng = 6), 
                  rep=100, maxiter=30, minit=m1)
summarytable(m2, m3, m4, m5, m5g, m6g,
             which = fitnames)

m7g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, data = dcpop, ng = 7), 
                         rep=100, maxiter=30, minit=m1)
summarytable(m2, m3, m4, m5, m5g, m6g, m7g,
             which = fitnames)



## Summarize the best-fitting model
summary(m6g)

## Plot predicted values of classes
m <- m6g
mp <- predictY(m, data.frame(t=0:2), var.time = 't')
pr <- as_tibble(mp$pred) %>% 
    mutate(t=as.vector(mp$times[,1])) %>%
    pivot_longer(cols=starts_with("Ypred"), names_to="class", values_to="pwht",
                 names_pattern = ".+(\\d)$")
p_pr <- p_ana + 
    geom_line(data=pr, aes(x=t, y=pwht, color=class, group=class), size=1.2)
p_pr

## Calculate the probabilities of neighborhoods being in each class
denom <- sum(sapply(c(m$best[1:(m$ng-1)], 0), exp))
pi_ic <- sapply(c(m$best[1:(m$ng-1)], 0), function(x) exp(x)/denom)
names(pi_ic) <- paste0("pi_i(c=", 1:m$ng, ")")
pi_ic

## Add information about the probabilities of class membership to data
d_res<- dcpop %>%
    left_join(mutate(m$pprob, across(where(is.numeric), round, 4), by="i")) %>%
    mutate(
        class = factor(as.character(class))
    )

## Plot observed neighborhood trajectory classified by most probable class 
## membership
p_cls <- p_base + 
    geom_line(data=d_res, aes(color=class), size=0.1) +
    geom_line(data=pr, aes(x=t, y=pwht, color=class, group=class), size=1.2) +
    labs(
        subtitle="Light lines trajectories of neighborhoods colored by most probable class membership"
    )
p_cls

## Find neighborhoods with some probability of classification across 
## multiple classes
d_mult <- filter(d_res, across(starts_with("prob"), ~ .x<0.99))
p_mult <- p_base +
    geom_line(data=d_mult, aes(color=class), size=0.5, linetype=2) +
    geom_line(data=pr, aes(x=t, y=pwht, color=class, group=class), size=1.2) +
    labs(
        subtitle="Dashed lines represent neighborhoods with <99% probability of most likely class membership"
    )
p_mult
