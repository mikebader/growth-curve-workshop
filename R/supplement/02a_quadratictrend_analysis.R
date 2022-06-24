#### ANALYSIS EXAMPLE: QUADRATIC TRENDS ####
## Description: This file analyzes Zillow data during boom and bust years
##              assuming trends start at different intercepts and change at
##              different, non-linear rates for 150 metros
##              Sets boom to begin in Apr 2004, bust to end in Oct 2009
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(tidyverse)
library(MASS)
library(lme4)

## GATHER DATA
load("../data/zillow_long.RData")
zillow.long.all$lnvalue_ti <- log(zillow.long.all$value_t)

## Analyze only the boom/bust months (Apr 2004 to Oct 2009)
boom_bust <- zillow.long.all[zillow.long.all$month%in%c(92:158),]
boom_bust$month <- boom_bust$month-92
N <- length(unique(boom_bust$RegionID))
N_t <- length(unique(boom_bust$month))

## DESCRIBE THE DATA
g.quad <- ggplot(boom_bust,aes(x=month,y=lnvalue_ti,group=RegionID)) +
    geom_line(size = .2) +
    scale_x_continuous(
        breaks=seq(0,N_t,12), 
        labels=paste("Jan",c(2004:2009))
    )
g.quad

## ANALYZE THE DATA
## Random intercept & slope with fixed quadratic (note transformed t^2)
m.ana <- lmer(
    lnvalue_ti ~ month + I(month^2/100) + (1 | RegionID),
    data=boom_bust
)
summary(m.ana)
m.ana.fe <- fixef(m.ana)
m.ana.fe
m.ana.fe[3] <- m.ana.fe[3]/100 ## Transforms fixef into original scale
m.ana.fe
boom_bust$lnvalue_ti_hat <- NA
boom_bust$lnvalue_ti_hat[!is.na(boom_bust$lnvalue_ti)] <- predict(m.ana)

## INTERPRET THE DATA
d.fit <- data.frame(month=c(0:N_t),month2=c(0:N_t)^2)
d.fit$best_fit <- matrix(c(rep(1,length(0:N_t)),d.fit$month,d.fit$month2),ncol=3) %*% matrix(m.ana.fe,ncol=1)
g.fit <- g.quad + geom_line(
    data=d.fit,
    aes(x=month,y=best_fit,group=NULL),
    col="orange", size=1.5
    )
g.fit
