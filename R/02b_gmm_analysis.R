#### ANALYSIS EXAMPLE: GROWTH MIXTURE MODEL ####
## Description: This file analyses a dataset of neighborhoods in DC 
##              different trajectories of change in the share of the white 
##              population from 1990 to 2010
## Author: Michael Bader

source("_functions.R")
estimate_grid = TRUE   ## Set to FALSE after the first time you run this code
                       ## (the code below saves a file with the model estimates
                       ## and allows you to load the estimates later without 
                       ## the need to wait for the estimates to converge)
library(tidyverse)
library(lcmm)

load("../data/dc-nhw-population.Rdata")

## ANALYZE THE DATA USING LCGA
fitnames <- c("G", "npm", "loglik", "conv", "AIC", "BIC", "entropy", "%class")
dcpop <- as.data.frame(dcpop)
m1 <- hlme(pnhw ~ t, subject = "i", random = ~t, data = dcpop)
m2 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, data = dcpop, ng = 2, B = random(m1))
m3 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, data = dcpop, ng = 3, B = random(m1))
summarytable(m2, m3,
             which = fitnames)
m4 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, data = dcpop, ng = 4, B = random(m1))
summarytable(m2, m3, m4,
             which = fitnames)

m5 <- hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, data = dcpop, ng = 5, B = random(m1))
summarytable(m2, m3, m4, m5,
             which = fitnames)

nrep = 100
if(isTRUE(estimate_grid)) {
    m5g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, 
                           data = dcpop, ng = 5), 
                      rep=nrep, maxiter=30, minit=m1)
    summarytable(m2, m3, m4, m5, m5g,
                 which = fitnames)
    
    m6g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t,
                           data = dcpop, ng = 6), 
                      rep=nrep, maxiter=30, minit=m1)
    summarytable(m2, m3, m4, m5, m5g, m6g,
                 which = fitnames)
    
    m7g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, data = dcpop, ng = 7), 
                      rep=nrep, maxiter=30, minit=m1)
    summarytable(m2, m3, m4, m5, m5g, m6g, m7g,
                 which = fitnames)
    
    m8g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, data = dcpop, ng = 8), 
                      rep=nrep, maxiter=30, minit=m1)
    summarytable(m2, m3, m4, m5, m5g, m6g, m7g, m8g,
                 which = fitnames[1:7])
    
    m9g <- gridsearch(hlme(pnhw ~ t, subject = "i", mixture = ~t, random = ~t, data = dcpop, ng = 9), 
                      rep=nrep, maxiter=30, minit=m1)
    save(m5g, m6g, m7g, m8g, m9g, file = "../data/gmm-grid-estimates.Rdata")
} else { 
    load("../data/gmm-grid-estimates.Rdata")
} #end if estimate_grid

## Summarize model fit parameters for classes from K=2 to K=9
summarytable(m2, m3, m4, m5, m5g, m6g, m7g, m8g, m9g,
             which = fitnames[1:7])
## Summarize the probability of class membership for models with K=2 to K=9
summarytable(m2, m3, m4, m5, m5g, m6g, m7g, m8g, m9g,
             which = fitnames[8])

## Summarize the best-fitting model
m <- m7g
summary(m)

## Plot predicted values of classes
mp <- predictY(m, data.frame(t=0:2), var.time = 't')
pr <- as_tibble(mp$pred) %>% 
    mutate(t=as.vector(mp$times[,1])) %>%
    pivot_longer(cols=starts_with("Ypred"), names_to="class", values_to="pwht",
                 names_pattern = ".+(\\d)$")
p_base <- ggplot(dcpop, aes(x = t, y = pnhw, group = i)) +
    labs(
        title = "Percent non-Hispanic white in DC neighborhoods 1990-2010",
        y = "Percent non-Hispanic white",
        x = "Year") +
    ylim(0, 100) +
    scale_x_continuous(breaks = 0:2, labels = seq(1990, 2010, 10))

p_pr <- p_base +
    geom_line(size=.1) +
    geom_line(data=pr, aes(x=t, y=pwht, color=class, group=class), size=1.2)
p_pr

## Calculate the probabilities of neighborhoods being in each class
## Calculate the denominator of the multinomial logit distribution
denom <- sum(sapply(c(m$best[1:(m$ng-1)], 0), exp))

## Calculate the probability of membership for each class given the log-odds
## estimated by the model
pi_ic <- sapply(c(m$best[1:(m$ng-1)], 0), function(x) exp(x)/denom)
names(pi_ic) <- paste0("pi_i(c=", 1:m$ng, ")")
pi_ic

## Add information about the probabilities of class membership to data
## The model contains a data frame called `pprob` that has the variables:
##     class: class in which unit i has the highest probability of membership
##     probK: probability of unit i belonging to class K
d_res<- dcpop %>%
    left_join(mutate(m$pprob, across(where(is.numeric), round, 4), by="i")) %>%
    mutate(
        class = factor(as.character(class))
    )
head(d_res)

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
d_mult <- filter(d_res, across(starts_with("prob"), ~ .x<0.95))
p_mult <- p_base +
    geom_line(data=d_mult, aes(color=class), size=0.25, linetype=2) +
    geom_line(data=pr, aes(x=t, y=pwht, color=class, group=class), size=1.2) +
    labs(
        subtitle="Dashed lines represent neighborhoods with <95% probability of most likely class membership"
    )
p_mult

## Analyze the tracts with less than 99% probability of being in most likely class
length(table(d_mult$i)) ## Count the number of tracts
length(table(d_mult$i)) / length(table(d_res$i))  ## Percent <99% probability

## Plot the distribution of maximum probabilities
d_mult <- d_mult %>%
    rowwise() %>%
    mutate(
        maxprob = max(across(starts_with("prob")))
    ) %>%
    ungroup()

d_close <- filter(d_mult, maxprob < 0.6)
p_hilite <- p_mult +
    geom_line(data=d_close, aes(color=NULL), color='yellow', alpha=0.5, size=1.2)
p_hilite

print(select(d_close, i, pnhw, starts_with("prob")))
