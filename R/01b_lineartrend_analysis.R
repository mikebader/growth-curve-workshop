#### ANALYSIS EXAMPLE: LINEAR TREND ####
## Description: This file estimates a single linear trend of NYC metro housing
##              values over past 12 months
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2)

## GATHER DATA
## Relabel variables to indicate that they represent housing values
load("../data/zillow.RData")
X.idx <- grep("X",names(zillow))
names(zillow)[X.idx]<-sub("X","val.",sub("\\.","",names(zillow)[X.idx]))

## Create list of variables to keep data from May 2016 - May 2017
keep_vars <- c(paste0("val.2016",sprintf("%02.0f",c(5:12))),paste0("val.2017",sprintf("%02.0f",c(1:5))))

zillow.past_year <- zillow[,c("RegionID","RegionName",keep_vars)]
nyc <- zillow.past_year[zillow.past_year$RegionName=="New York, NY",]
nyc

## Uh oh! We need to make the data "long" in order to analyze it
nyc.long <- reshape(data=nyc,
                       direction="long",
                       varying=grep("val",names(zillow.past_year)),
                       v.names = "value_t",
                       timevar = "month",
                       idvar = "RegionID"
)
nyc.long
nyc.long$month <- nyc.long$month - 1
nyc.long$lnvalue_t <- log(nyc.long$value_t)

## DESCRIBE DATA
g.base <- ggplot(nyc.long, aes(x=month,y=lnvalue_t)) +
        geom_point() +
        scale_y_continuous(limits=c(5.45,5.53),breaks=seq(5.45,5.53,.01)) +
        scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])
g.base

## ANALYZE DATA
m.nyc <- lm(lnvalue_t ~ month,data=nyc.long)
m.nyc

nyc.long$lnvalue_t_hat <- predict(m.nyc)
nyc.long$e_t <- nyc.long$lnvalue_t - nyc.long$lnvalue_t_hat

## INTERPRET DATA
g.pred <- g.base + geom_smooth(method="lm",se=FALSE)
g.pred

sapply(list(mean=mean(nyc.long$e_t),sd=sd(nyc.long$e_t)),round,4)

