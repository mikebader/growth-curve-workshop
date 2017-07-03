#### ANALYSIS EXAMPLE: LINEAR TREND ACROSS TWO METROS ####
## Description: This file analyzes data over past year in NYC and LA
##              metros and estimates that trend
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2) # This loads a library that makes prettier plots than standard R

##### ANALYSIS ####
## GATHER DATA
load("../data/zillow_long.RData")
zillow.long$lnvalue_t <- log(zillow.long$value_t)

## Keep data for the past year
## Create list of months in past year
## (function get.past_year is defined in _functions.R)
past_year <- get.past_year(zillow.long)
zillow.long <- zillow.long[zillow.long$month %in% past_year, ]

## DESCRIBE THE DATA
d.ana <- zillow.long[grep("New York|Los Angeles",zillow.long$RegionName),]
d.ana$city <- factor(rep(c("nyc","la"),each=13))
g.base <- ggplot(d.ana, aes(x=month,y=lnvalue_t,col=city)) +
    geom_point() +
    scale_x_continuous(breaks=past_year,labels=rep(month.abb,2)[5:17]) +
    labs(
        y="Logged home value/sq. ft.",
        x="Month"
    )
g.base

## ANALYZE THE DATA
m.ana <- lm(lnvalue_t ~ month,data=d.ana)
summary(m.ana)
m.ana.coef <- m.ana$coefficients

d.ana$lnvalue_t_hat <- predict(m.ana)
d.ana$e_t <- d.ana$lnvalue_t - d.ana$lnvalue_t_hat
d.ana[,c("city","e_t")]

g.fit <- g.base +
    geom_smooth(method="lm",se=FALSE,size=.5) +
    geom_smooth(aes(col=NULL),method="lm",se=FALSE)
g.fit



