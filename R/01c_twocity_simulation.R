#### SIMULATION EXAMPLE: LINEAR TREND ACROSS TWO METROS ####
## Description: This file simulates a dataset with a linear trend across two
##              metros and estimates that trend
## Author: Michael Bader

rm(list=ls())
source("R/_functions.R")
library(ggplot2)
library(latex2exp)

## PLAN OUR POPULATION
month <- c(0:12)

# New York
nyc.b_0 <- log(236)
nyc.b_1 <- 0.005
nyc.sigma <- 0.004

# Philadelphia
phl.b_0 <- log(220)
phl.b_1 <- 0.005
phl.sigma <- 0.004

## CONJURE OUR POPULATION
nyc.value_t <- nyc.b_0 + nyc.b_1*month + rnorm(13,0,nyc.sigma)
d.nyc.sim <- data.frame(month,value_t=nyc.value_t)

phl.value_t <- phl.b_0 + phl.b_1*month + rnorm(13,0,phl.sigma)
d.phl.sim <- data.frame(month,value_t=phl.value_t)

d.sim <- rbind(d.nyc.sim,d.phl.sim)
d.sim$city <- factor(rep(c("nyc","phl"),each=13))
d.sim
qplot(x=month,y=value_t,col=city,data=d.sim)

## ANALYZE OUR POPULATION
## Analyze data by city
m.nyc.sim <- lm(value_t ~ month, data=d.sim[d.sim$city=="nyc",])
m.nyc.sim

m.phl.sim <- lm(value_t ~ month, data=d.sim[d.sim$city=="phl",])
m.phl.sim

i.coefs <- matrix(c(m.phl.sim$coefficients,m.nyc.sim$coefficients),ncol=2)

g.base <- ggplot(d.sim,aes(x=month,y=value_t)) +
    geom_point(aes(col=city)) +
    geom_abline(slope=mean(i.coefs[2,]),intercept=mean(i.coefs[1,])
                ,size=1, col="blue") +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])
g.base
ggsave("images/0103_twocity_single_model.png",plot=g.base,height=2.5,width=4,units="in")

## Calculate predicted values and errors
d.sim$value_t_hat <- mean(i.coefs[1,]) + mean(i.coefs[2,])*d.sim$month
d.sim$e_t <- d.sim$value_t - d.sim$value_t_hat

report.e <- function(e_t) {sapply(list(mean=sum(e_t),sd=sd(e_t)),round,4)}
report.e(d.sim$e_t)
by(d.sim[,"e_t"],d.sim$city,report.e)

## DESCRIBE ERROR STRUCTURES
d.sim$city.value_t_hat <- c(predict(m.nyc.sim),predict(m.phl.sim))
d.sim$city.r_t <- d.sim$city.value_t_hat - d.sim$value_t_hat
d.sim$city.e_t <- d.sim$value_t - d.sim$city.value_t_hat

## Plot error structure
g.nyc.phl <- g.base +
    geom_smooth(method="lm",se=FALSE) +
    geom_smooth(aes(col=city),method="lm",se=FALSE,size=.5)

## Example for single point
d.sim.phl <- d.sim[d.sim$city=="phl",]
phl.dec <- d.sim[d.sim$city=="phl",][8,]
phl.dec

## Plot of city error
g.city_e <- g.nyc.phl +
    geom_segment(x=7,xend=7,
                 y=phl.dec$value_t_hat,yend=phl.dec$city.value_t_hat,
                 col="blue",linetype=2
    ) +
    geom_text(x=7.2,y=(phl.dec$value_t_hat+phl.dec$city.value_t_hat)/2,
             label=paste(round(phl.dec$city.r_t,4),"(r_0)"),
             hjust="outward",col="blue"
             )
g.city_e

## Adding plot of error for the month
g.month_e <- g.city_e +
    geom_segment(x=7,xend=7,
                 y=phl.dec$city.value_t_hat,yend=phl.dec$value_t,
                 col="#F8766D",linetype=2) +
    geom_text(x=7.2,y=(phl.dec$city.value_t_hat+phl.dec$value_t)/2,
              label=paste(round(phl.dec$city.e_t,4),"(e)"),
              hjust="outward",col="#F8766D")
g.month_e

## Adding the total error to the plot
g.tot_e <- g.month_e +
    geom_text(x=7.2,y=phl.dec$value_t,
              label=paste(round(phl.dec$e_t,4),"(total error)"),
              hjust="outward",vjust="outward",col="black")
g.tot_e
ggsave("images/0103_twocity_error_decomposition.png",plot=g.tot_e,height=2.5,width=4,units="in")

## Show city-specific error for each of Philly's observations
g.city_e.all <- g.nyc.phl +
    geom_segment(x=d.sim.phl$month,xend=d.sim.phl$month,
                 y=d.sim.phl$value_t_hat,yend=d.sim.phl$city.value_t_hat,data=d.sim.phl,
                 col="blue",linetype=2
    ) +
    geom_text(data=d.sim.phl,
              aes(x=I(month + .2),y=(d.sim.phl$value_t_hat+d.sim.phl$city.value_t_hat)/2),
              label=round(d.sim.phl$city.r_t,2),
              hjust="right",col="blue"
    )
g.city_e.all




## Analyze the data as a single dataset
m.sim <- lm(value_t ~ month,data=d.sim)
m.sim
m.sim.coef <- m.sim$coefficients

## Compare the coefficients from the pooled model to the
## mean of the coefficients from the individual models
c(m.sim.coef[1],mean(i.coefs[1,]))
c(m.sim.coef[2],mean(i.coefs[2,]))


