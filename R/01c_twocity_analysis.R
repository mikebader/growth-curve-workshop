#### ANALYSIS EXAMPLE: LINEAR TREND ACROSS TWO METROS ####
## Description: This file analyzes data over past year in NYC and
##              Philadelphia metros and estimates that trend
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2)  # This loads a library that makes prettier plots than standard R
library(cowplot)  # To combine plots into single plot
library(huxtable) # To present pretty regression tables

##### ANALYSIS ####
## GATHER DATA
load("../data/zillow_long.RData")
zillow.long$lnvalue_t <- log(zillow.long$value_t)

## Keep data for the past year
## Create list of months in past two years
## (function `get.past_year()` is defined in _functions.R)
past_year <- get.past_year(zillow.long)
zillow.long <- zillow.long[zillow.long$month %in% past_year, ]
zillow.long <- zillow.long[order(zillow.long$SizeRank),]

## DESCRIBE THE DATA
d.ana <- zillow.long[grep("New York|Washington",zillow.long$RegionName),]
d.ana$city <- factor(rep(c("nyc","was"),each=25))
last_month <- match(tail(d.ana$month.abbr,1),month.abb)
month_labels <- rep(month.abb,3)[seq(last_month,last_month+24)]
g.base <- ggplot(d.ana, aes(x=month,y=lnvalue_t,col=city)) +
    geom_point(size=1) +
    scale_x_continuous(breaks=past_year,labels=month_labels) +
    labs(
        y="Logged home value/sq. ft.",
        x="Month",
        color="Metro"
    )
g.base

## ANALYZE THE DATA
m.ana <- lm(lnvalue_t ~ month,data=d.ana)
summary(m.ana)


## Notice that the intercept does not fall anywhere the observed data. 
## The data have time starting in March 1996 
## Let's rescale the variable so that zero is at the start of our period,
## then re-analyze the data
d.ana$monthc <- d.ana$month - min(zillow.long$month)
m.ana <- lm(lnvalue_t ~ monthc, data=d.ana) ## Notice small change here
summary(m.ana)
m.ana.coef <- m.ana$coefficients

## Plot the residuals by metro
d.ana$lnvalue_t_hat <- predict(m.ana)
d.ana$e_t <- d.ana$lnvalue_t - d.ana$lnvalue_t_hat
d.ana[,c("city","e_t")]
e_t.nyc <- qplot(d.ana$e_t[d.ana$city=="nyc"], bins=200) + xlim(-.22, .225)
e_t.was <- qplot(d.ana$e_t[d.ana$city=="was"], bins=200) + xlim(-.22, .225)
plot_grid(e_t.nyc, e_t.was, nrow=2)

## Estimate individual models for each metro
m.nyc <- lm(lnvalue_t ~ monthc, data=d.ana[d.ana$city=="nyc",])
m.was <- lm(lnvalue_t ~ monthc, data=d.ana[d.ana$city=="was",])
huxreg(m.nyc, m.was)

g.fit <- g.base +
    geom_smooth(method="lm", se=FALSE, size=.5, alpha=.75) +
    geom_smooth(aes(col=NULL), method="lm", se=FALSE)
g.fit



