#### ANALYSIS EXAMPLE: RANDOM INTERCEPTS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              but constant slope for 150 largest metros
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2)
library(lme4)

## GATHER THE DATA
load('../Data/zillow_long.RData')
zillow.long$lnvalue_ti <- log(zillow.long$value_t)

## Get values for the past year
## (get.past_year function defined in _functions.R)
past_year <- get.past_year(zillow.long)
zillow.long <- zillow.long[zillow.long$month %in% past_year, ]
month.abbrs <- zillow.long$month.abbr[1:length(past_year)]

## DESCRIBE THE DATA
g.ana <- ggplot(data=zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID)) +
    geom_line() +
    scale_x_continuous(breaks=past_year,labels=month.abbrs)
g.ana

d.int <- zillow.long[zillow.long$month==head(past_year,1),]
qplot(d.int[,"lnvalue_ti"],bins=15) +
      labs(
          x="Logged median home value/sq. ft. in May 2016"
      )

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti ~ month + (1 | RegionID),data=zillow.long)
summary(m.ana)
m.ana.fe <- round(fixef(m.ana),4)

## INTERPRET THE DATA
g.pred <- g.ana + geom_abline(
    intercept=m.ana.fe[["(Intercept)"]],
    slope=m.ana.fe[["month"]],
    col="orange",size=1.2
)
g.pred
