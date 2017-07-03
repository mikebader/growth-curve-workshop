#### SIMULATION EXAMPLE: MEAN ####
## Description: This file simulates a dataset and takes the mean of that dataset
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2) # This loads a library that makes prettier plots than standard R

## PREPARE THE POPULATION
N  <- 200
mu <- 81
sd <- 3

## CONJURE THE POPULATION
e_i <- rnorm(N,0,sd)
length(e_i)
y_i = mu + e_i  ## Notice how this looks *exactly* like our model above?

## ANALYZE THE POPULATION
qplot(y_i,bins=20)
print(c(mean(y_i),sd(y_i)))
