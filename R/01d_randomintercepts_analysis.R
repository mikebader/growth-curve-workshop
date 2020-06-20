#### ANALYSIS EXAMPLE: RANDOM INTERCEPTS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              but constant slope for 150 largest metros
## Author: Michael Bader

rm(list=ls())
source("R/_functions.R")
library(ggplot2)
library(lme4)

## GATHER THE DATA
load('../Data/zillow_long.RData')
zillow.long$lnvalue_ti <- log(zillow.long$value_t)

## Get values for the past year
## (get.past_year function defined in _functions.R)
past_year <- get.past_year(zillow.long)
zillow.long <- zillow.long[zillow.long$month %in% past_year, ]
nrow(zillow.long)

## Subtract minimum value of month to set intercept to t=0
zillow.long$month <- zillow.long$month - min(zillow.long$month)

## DESCRIBE THE DATA
month.abbrs <- zillow.long$month.abbr[1:length(past_year)]
g.ana <- ggplot(data=zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID)) +
    geom_line() +
    scale_x_continuous(breaks=past_year,labels=month.abbrs)
g.ana

d.int <- zillow.long[zillow.long$month==0,]
qplot(d.int[,"lnvalue_ti"],bins=15) +
      labs(
          x=paste0("Logged median home value/sq. ft. in ",
                  month.abbrs[1],", ",
                  zillow.long$year[1])
      )

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti ~ month + (1 | RegionID),data=zillow.long)
summary(m.ana)

## INTERPRET THE DATA
g.pred <- g.ana + geom_abline(
    intercept=m.ana.fe[["(Intercept)"]],
    slope=m.ana.fe[["month"]],
    col="orange",size=1.2
)
g.pred


## Ignore Below (used for writing values to my lecture notes)
f <- file("lecture/_0103-analysis-estimates.tex")
writeLines(c(
    paste0("\\newcommand{\\intercept}{",round(m.ana.fe[1],3),"}"),
    paste0("\\newcommand{\\slope}{",m.ana.fe[2],"}"),
    paste0("\\newcommand{\\varlevone}{",round(summary(m.ana)$sigma,3),"}"),
    paste0("\\newcommand{\\varlevtwo}{",round(summary(m.ana)$varcor$RegionID[1],3),"}")
    ),f)
close(f)
