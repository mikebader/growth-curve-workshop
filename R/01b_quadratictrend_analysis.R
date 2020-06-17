#### ANALYSIS EXAMPLE: QUADRATIC TREND ####
## Description: This file estimates a single quadratic trend of NYC metro
##              housing values during the housing boom & bust (Jan 2004
##              to Dec 2009)
## Author: Michael Bader

rm(list=ls())
source("R/_functions.R")
library(ggplot2)

## GATHER DATA
## Relabel variables to indicate that they represent housing values
load("../data/zillow.RData")
X.idx <- grep("X",names(zillow))
names(zillow)[X.idx]<-sub("X","val.",sub("\\.","",names(zillow)[X.idx]))

## Create list of variables to keep data from Jan 2004 - Dec 2008
keep_vars <- paste0("val.",unlist(
                        lapply(c(2004:2008),paste0,sprintf("%02.0f",1:12))
                    ))

nyc.boom_bust <- zillow[
                        zillow$RegionName=="New York, NY",
                        c("RegionID","RegionName",keep_vars)
                        ]


## Reshape the data into long format
nyc.long <- reshape(data=nyc.boom_bust,
                    direction="long",
                    varying=grep("val",names(nyc.boom_bust)),
                    v.names = "value_t",
                    timevar = "month",
                    idvar = "RegionID"
)
head(nyc.long)

nyc.long$month <- nyc.long$month - 1
nyc.long$lnvalue_t <- log(nyc.long$value_t)

## Describe data
g.base <- ggplot(nyc.long, aes(x=month,y=lnvalue_t)) +
    geom_point(size=.95) +
    scale_x_continuous(breaks=seq(0,72,12),labels=paste0("Jan ",2004:2010))
g.base

## Analyze the data
m.nyc.bb <- lm(lnvalue_t ~ month + I(month^2), data=nyc.long)
summary(m.nyc.bb)

nyc.long$lnvalue_t_hat <- predict(m.nyc.bb)
nyc.long$e_t <- nyc.long$lnvalue_t - nyc.long$lnvalue_t_hat
sapply(list(sum=sum(nyc.long$e_t),sd=sd(nyc.long$e_t)),round,4)

g.base + geom_line(data=nyc.long,aes(y=lnvalue_t_hat),color="blue")




