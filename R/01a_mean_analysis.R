#### ANALYSIS EXAMPLE: MEAN ####
## Description: This file analyzes the mean of the Zillow dataset
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(ggplot2)
library(cowplot)

## GATHER DATA
## Load data from Zillow downloaded from _init.R
load("../data/zillow.RData")
lastcol <- names(zillow)[ncol(zillow)]
zillow_long <- reshape(zillow, direction="long", idvar="RegionID",
                       v.names="y", varying=list(4:ncol(zillow)))

## DESCRIBE DATA
qplot(zillow_long$y) ## Not normally distributed

y_t <- log(zillow_long$y)
(
    yhist <- ggplot() + 
        geom_histogram(aes(x=y_t),bins=30, fill="gold") +
        geom_vline(aes(xintercept=mean(y_t, na.rm=TRUE)), color="goldenrod")
)
## ANALYZE DATA
beta0hat <- mean(y_t, na.rm=TRUE) 
e_t <- y_t - beta0hat

list(mean=mean(e_t, na.rm=TRUE), sd=sd(e_t, na.rm=TRUE))
(
    ehist <- ggplot() +
        geom_histogram(aes(x=e_t),bins=30)
)
plot_grid(yhist, ehist, nrow=2)

## INTERPRET DATA
## Count the number of metros for which prices fall within one s.d.
sd.range <- c(
    beta0hat - sd(e_t, na.rm=TRUE),
    beta0hat + sd(e_t, na.rm=TRUE)
    )
sd.range
sum(y_t>=sd.range[1] & y_t<=sd.range[2], na.rm=TRUE) / sum(!is.na(y_t))
