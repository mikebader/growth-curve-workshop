#### ANALYSIS EXAMPLE: MEAN ####
## Description: This file analyzes the mean of the Zillow dataset
## Author: Michael Bader

rm(list=ls())
source('R/_functions.R')
library(ggplot2)

## GATHER DATA
## Load data from Zillow downloaded from _init.R
load("../data/zillow.RData")
zillow.may17 <- zillow[,c('RegionID','RegionName','X2017.05')]
zillow.may17[c(1:5,146:150),]

## DESCRIBE DATA
qplot(zillow.may17$X2017.05,bins=15)

zillow.may17$x_i <- log(zillow.may17$X2017.05)
qplot(zillow.may17$x_i,bins=15)

## ANALYZE DATA
zillow.may17$x.bar <- mean(zillow.may17$x_i)
zillow.may17$e_i <- zillow.may17$x_i - zillow.may17$x.bar
mean.sd(zillow.may17,'x_i','e_i')
qplot(zillow.may17$e_i,bins=15)

## INTERPRET DATA
## Count the number of metros for which prices fall within one s.d.
sd.range <- c(
    exp(mean(zillow.may17$x_i)-sd(zillow.may17$e_i)),
    exp(mean(zillow.may17$x_i)+sd(zillow.may17$e_i))
)
sd.range
sum(zillow.may17$X2017.05>=sd.range[1] & zillow.may17$X2017.05<=sd.range[2]) / 150
