#### ANALYSIS EXAMPLE: RANDOM INTERCEPTS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros
## Author: Michael Bader

## Show that slopes do vary across metro areas in Zillow data
source('01d_randomintercepts_analysis.R')
betas <- by(zillow.long,zillow.long$RegionID,function(d) lm(lnvalue_ti~month,data=d))
slopes <- t(sapply(betas,coef))[,"month"]
qplot(slopes,bins=15)

## Prepare environment
rm(list=ls()[!(ls()%in% "zillow.long")]) ## Removes all variables from
                                         ## environment except zillow.long
source('_functions.R')
library(MASS)
library(lme4)
library(ggplot2)

## GATHER & DESCRIBE THE DATA
## The data are the same as those used in the random intercept model

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti ~ month + (1 + month | RegionID),data=zillow.long)
summary(m.ana)
m.ana.fe <- fixef(m.ana)
m.ana.re <- ranef(m.ana)

## INTERPRET THE DATA
g.pred <- ggplot(data=zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID)) +
            geom_line() +
            geom_abline(
                intercept=m.ana.fe[1],slope=m.ana.fe[2],
                col="orange",size=1.2
                ) +
            scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])
g.pred

## Show slopes from initial value
## (i.e., examine change without initial differences)
slopes <- m.ana.fe[2] + m.ana.re[[1]][,2]
d.ana <- data.frame(RegionName=zillow.long$RegionName,b_1=rep(slopes,each=13),month=rep(c(0:12),150))
d.ana$pred <- d.ana$b_1*d.ana$month
g.slopes <- ggplot(d.ana,aes(x=month,y=pred,by=RegionName)) +
    geom_line() +
    geom_abline(intercept=0,slope=m.ana.fe[2],color="orange",size=1.5) +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17]) +
    labs(
        y="Change from initial value"
    )
g.slopes
