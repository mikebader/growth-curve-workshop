#### ANALYSIS EXAMPLE: LINEAR TREND ####
## Description: This file estimates a single linear trend of NYC metro housing
##              values over past 12 months
## Author: Michael Bader

rm(list=ls())
source("R/_functions.R")
library(ggplot2)
current_year <- 2018

## GATHER DATA
## Relabel variables to indicate that they represent housing values
load("../data/zillow.RData")
X.idx <- grep("X",names(zillow))
names(zillow)[X.idx]<-sub("X","val.",sub("\\.","",names(zillow)[X.idx]))

## Create list of variables to keep data from April of past year
## to April of current year
keep_vars <- c(
                paste0("val.",current_year-1,sprintf("%02.0f",c(4:12))),
                paste0("val.",current_year,sprintf("%02.0f",c(1:4)))
                )

nyc <- zillow[zillow$RegionName=="New York, NY",
                        c("RegionID","RegionName",keep_vars)]
nyc

## Uh oh! We need to make the data "long" in order to analyze it
nyc.long <- reshape(data=nyc,
                       direction="long",
                       varying=grep("val",names(nyc)),
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
        scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[4:16])
g.base

## ANALYZE DATA
m.nyc <- lm(lnvalue_t ~ month,data=nyc.long)
summary(m.nyc)

nyc.long$lnvalue_t_hat <- predict(m.nyc)
nyc.long$e_t <- nyc.long$lnvalue_t - nyc.long$lnvalue_t_hat

## INTERPRET DATA
g.pred <- g.base + geom_smooth(method="lm",se=FALSE)
g.pred

sapply(list(mean=mean(nyc.long$e_t),sd=sd(nyc.long$e_t)),round,4)

