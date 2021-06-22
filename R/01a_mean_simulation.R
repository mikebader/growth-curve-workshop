#### SIMULATION EXAMPLE: MEAN ####
## Description: This file simulates a dataset and takes the mean of
##              that dataset
## Author: Michael Bader

rm(list=ls())
library(ggplot2) ## This loads a library that makes prettier plots than standard R
set.seed(9876)

## PREPARE THE POPULATION
N_t  <- 24*12  ## Number of months since April 1996 per metropolitan area
beta0 <- 4.6
sd   <- 0.46

## CONJURE THE POPULATION
epsilon_t <- rnorm(N_t,0,sd)
length(epsilon_t)
y_t = beta0 + epsilon_t  

## ANALYZE THE POPULATION
(simpop_plt <- ggplot() + 
        geom_histogram(aes(x=y_t),bins=30, fill="darkblue") +
    geom_vline(aes(xintercept=mean(y_t)), color="cyan3", size=1))
print(c(mean(y_t),sd(y_t)))

## DRAW 1000 REPEATED SAMPLES OF 24 VALUES
samples <- lapply(1:100, function(samp){sample(y_t, 24)})
beta0hats <- sapply(samples, mean)
(simsamp_plt <- simpop_plt + 
        geom_histogram(aes(x=beta0hats), bins=50, fill="orange") + 
        geom_vline(aes(xintercept=mean(y_t)), 
                   color="cyan3", size=1) + ## Redraws population mean 
        geom_vline(aes(xintercept=mean(beta0hats)), color="gold", size=1))

## NOW SHOW WHAT WE WOULD OBSERVE FROM A SINGLE SAMPLE
our_sample <- sample(samples, 1)
our_sample <- unlist(our_sample) ## Data management
(simsamp_plt +
        geom_histogram(aes(x=our_sample), bins=8, fill="gold", alpha=.75)+
        geom_vline(aes(xintercept=mean(our_sample)), color="yellow", size=1))
