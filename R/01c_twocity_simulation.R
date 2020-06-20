#### SIMULATION EXAMPLE: LINEAR TREND ACROSS TWO METROS ####
## Description: This file simulates a dataset with a linear trend across two
##              metros and estimates that trend
## Author: Michael Bader

rm(list=ls())
source("R/_functions.R")
library(ggplot2)
library(latex2exp)

## PLAN OUR POPULATION
month <- c(0:23)

# New York
nyc.b_0 <- log(700) ## $700/sqft
nyc.b_1 <- .03/12   ## 3% annual increase
nyc.sigma <- .03/48 ## 0.25% fluctuation off of trend in given month 

# Washington
was.b_0 <- log(500) ## $500/sqft
was.b_1 <- .03/12   ## 3% annual increase
was.sigma <- .03/48 ## 0.25% fluctuation off of trend in given month 

## CONJURE OUR POPULATION
nyc.lnvalue_t <- nyc.b_0 + nyc.b_1*month + rnorm(24,0,nyc.sigma)
d.nyc.sim <- data.frame(month,lnvalue_t=nyc.lnvalue_t)

was.lnvalue_t <- was.b_0 + was.b_1*month + rnorm(24,0,was.sigma)
d.was.sim <- data.frame(month,lnvalue_t=was.lnvalue_t)

d.sim <- rbind(d.nyc.sim,d.was.sim)
d.sim$city <- factor(rep(c("nyc","was"),each=24))
d.sim
qplot(x=month,y=lnvalue_t,col=city,data=d.sim)



## ANALYZE OUR POPULATION
## Analyze data by city
m.nyc.sim <- lm(lnvalue_t ~ month, data=d.sim[d.sim$city=="nyc",])
m.nyc.sim

m.was.sim <- lm(lnvalue_t ~ month, data=d.sim[d.sim$city=="was",])
m.was.sim

i.coefs <- matrix(c(m.was.sim$coefficients,m.nyc.sim$coefficients),ncol=2)

## Analyze the trend as a single model
m.both <- lm(lnvalue_t ~ month, data=d.sim)

## Plot total error
qplot(m.both$residuals, bins=1000)
d.sim$yhat_t <- predict(m.both)
d.sim$e_t <- d.sim$lnvalue_t - d.sim$yhat_t
qplot(d.sim$mboth_e_t[d.sim$city=="was"], bins=20)

time_plt <- ggplot(d.sim,aes(x=month,y=lnvalue_t)) +
    geom_point(aes(color=city)) +
    geom_abline(slope=mean(i.coefs[2,]),intercept=mean(i.coefs[1,])
                ,size=1, col="blue") +
    scale_x_continuous(breaks=seq(0,24,1),labels=rep(month.abb,3)[4:28])
time_plt
ggsave("images/0103_twocity_single_model.png",
       plot=time_plt, height=2.5, width=4, units="in")

## Calculate predicted values and errors
d.sim$yhat_t <- mean(i.coefs[1,]) + mean(i.coefs[2,])*d.sim$month
d.sim$e_t <- d.sim$lnvalue_t - d.sim$yhat_t

report.e <- function(e_t) {sapply(list(mean=sum(e_t),sd=sd(e_t)),round,5)}
report.e(d.sim$e_t)
by(d.sim[,"e_t"],d.sim$city,report.e)

## DESCRIBE ERROR STRUCTURES
d.sim$yhat_t <- c(predict)

d.sim$city.lnvaluehat_hat <- c(predict(m.nyc.sim), predict(m.was.sim))
d.sim$city.r_t <- d.sim$city.lnvaluehat_t - d.sim$lnvaluehat_t
d.sim$city.e_t <- d.sim$lnvalue_t - d.sim$city.lnvaluehat_t

## Plot error structure
g.nyc.was <- g.base +
    geom_smooth(method="lm",se=FALSE) +
    geom_smooth(aes(col=city),method="lm",se=FALSE,size=.5)

## Example for single point
d.sim.was <- d.sim[d.sim$city=="was",]
was.dec <- d.sim[d.sim$city=="was",][8,]
was.dec

## Plot of city error
g.city_e <- g.nyc.was +
    geom_segment(x=7,xend=7,
                 y=was.dec$value_t_hat,yend=was.dec$city.value_t_hat,
                 col="blue",linetype=2
    ) +
    geom_text(x=7.2,y=(was.dec$value_t_hat+was.dec$city.value_t_hat)/2,
             label=paste(round(was.dec$city.r_t,4),"(r_0)"),
             hjust="outward",col="blue"
             )
g.city_e

## Adding plot of error for the month
g.month_e <- g.city_e +
    geom_segment(x=7,xend=7,
                 y=was.dec$city.value_t_hat,yend=was.dec$value_t,
                 col="#F8766D",linetype=2) +
    geom_text(x=7.2,y=(was.dec$city.value_t_hat+was.dec$value_t)/2,
              label=paste(round(was.dec$city.e_t,4),"(e)"),
              hjust="outward",col="#F8766D")
g.month_e

## Adding the total error to the plot
g.tot_e <- g.month_e +
    geom_text(x=7.2,y=was.dec$value_t,
              label=paste(round(was.dec$e_t,4),"(total error)"),
              hjust="outward",vjust="outward",col="black")
g.tot_e
ggsave("images/0103_twocity_error_decomposition.png",plot=g.tot_e,height=2.5,width=4,units="in")

## Show city-specific error for each of Philly's observations
g.city_e.all <- g.nyc.was +
    geom_segment(x=d.sim.was$month,xend=d.sim.was$month,
                 y=d.sim.was$value_t_hat,yend=d.sim.was$city.value_t_hat,data=d.sim.was,
                 col="blue",linetype=2
    ) +
    geom_text(data=d.sim.was,
              aes(x=I(month + .2),y=(d.sim.was$value_t_hat+d.sim.was$city.value_t_hat)/2),
              label=round(d.sim.was$city.r_t,2),
              hjust="right",col="blue"
    )
g.city_e.all




## Analyze the data as a single dataset
m.sim <- lm(value_t ~ month,data=d.sim)
summary(m.sim)
m.sim.coef <- m.sim$coefficients

## Compare the coefficients from the pooled model to the
## mean of the coefficients from the individual models
c(m.sim.coef[1],mean(i.coefs[1,]))
c(m.sim.coef[2],mean(i.coefs[2,]))


