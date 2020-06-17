#### ANALYSIS EXAMPLE: MEAN ####
## Description: This file analyzes the mean of the Zillow dataset
## Author: Michael Bader

rm(list=ls())
source('R/_functions.R')
library(ggplot2)

## GATHER DATA
## Load data from Zillow downloaded from _init.R
load("../data/zillow.RData")
lastcol <- names(zillow)[ncol(zillow)]
zillow_1m <- zillow[,c('RegionID','RegionName',lastcol)]
names(zillow_1m) <- c('RegionID','RegionName','psqf')
zillow_1m[c(1:5,146:150),]

## DESCRIBE DATA
qplot(zillow_1m$psqf,bins=15)

zillow_1m$y_i <- log(zillow_1m$psqf)
qplot(zillow_1m$y_i,bins=15)

## ANALYZE DATA
zillow_1m$y.bar <- mean(zillow_1m$y_i)
zillow_1m$e_i <- zillow_1m$y_i - zillow_1m$y.bar
mean.sd(zillow_1m,'y_i','e_i')
qplot(zillow_1m$e_i,bins=15)

## INTERPRET DATA
## Count the number of metros for which prices fall within one s.d.
sd.range <- c(
    exp(mean(zillow_1m$y_i)-sd(zillow_1m$e_i)),
    exp(mean(zillow_1m$y_i)+sd(zillow_1m$e_i))
)
sd.range
sum(zillow_1m$psqf>=sd.range[1] & zillow_1m$psqf<=sd.range[2]) / 150
