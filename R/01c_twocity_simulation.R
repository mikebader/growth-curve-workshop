#### SIMULATION EXAMPLE: LINEAR TREND ACROSS TWO METROS ####
## Description: This file simulates a dataset with a linear trend across two
##              metros and estimates that trend
## Author: Michael Bader

rm(list=ls())
source("_functions.R")
library(ggplot2) # This loads a library that makes prettier plots than standard R


## PLAN OUR POPULATION
month <- c(0:12)

# New York
nyc.b_0 <- log(236)
nyc.b_1 <- 0.005
nyc.sigma <- 0.002

# Los Angeles
la.b_0 <- log(220)
la.b_1 <- 0.005
la.sigma <- 0.003

## CONJURE OUR POPULATION
nyc.price_t <- nyc.b_0 + nyc.b_1*month + rnorm(13,0,nyc.sigma)
d.nyc.sim <- data.frame(month,price_t=nyc.price_t)

la.price_t <- la.b_0 + la.b_1*month + rnorm(13,0,la.sigma)
d.la.sim <- data.frame(month,price_t=la.price_t)

d.sim <- rbind(d.nyc.sim,d.la.sim)
d.sim$city <- factor(rep(c("nyc","la"),each=13))
d.sim
qplot(x=month,y=price_t,col=city,data=d.sim)

## ANALYZE OUR POPULATION
m.sim <- lm(price_t ~ month,data=d.sim)
m.sim
m.sim.coef <- m.sim$coefficients

d.sim$price_t_hat <- predict(m.sim)
d.sim$e_t <- d.sim$price_t - d.sim$price_t_hat

g.base <- ggplot(d.sim,aes(x=month,y=price_t)) +
    geom_point(aes(col=city)) +
    geom_abline(slope=m.sim.coef[2],intercept=m.sim.coef[1],size=1, col="blue") +
    scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17]) +
    labs(
        y="Logged price",
        x="Month"
    )
g.base

report.e <- function(e_t) {sapply(list(mean=sum(e_t),sd=sd(e_t)),round,4)}
report.e(d.sim$e_t)
by(d.sim[,"e_t"],d.sim$city,report.e)

## Analyze data by city
m.nyc.sim <- lm(price_t ~ month, data=d.sim[d.sim$city=="nyc",])
m.nyc.sim

m.la.sim <- lm(price_t ~ month, data=d.sim[d.sim$city=="la",])
m.la.sim

d.sim$city.price_t_hat <- c(predict(m.nyc.sim),predict(m.la.sim))
d.sim$city.r_t <- d.sim$city.price_t_hat - d.sim$price_t_hat
d.sim$city.e_t <- d.sim$price_t - d.sim$city.price_t_hat
d.sim


g.nyc.la <- g.base +
    geom_smooth(method="lm",se=FALSE) +
    geom_smooth(aes(col=city),method="lm",se=FALSE,size=.5)
g.nyc.la


## DESCRIBE ERROR STRUCTURES
d.sim.la <- d.sim[d.sim$city=="la",]
la.dec <- d.sim[d.sim$city=="la",][8,]
la.dec

g.nyc.la
g.city_e <- g.nyc.la +
    geom_segment(x=7,xend=7,
                 y=la.dec$price_t_hat,yend=la.dec$city.price_t_hat,
                 col="blue",linetype=2
    ) +
    geom_text(x=7.2,y=(la.dec$price_t_hat+la.dec$city.price_t_hat)/2,
              label=round(la.dec$city.r_t,4),
              hjust="outward",col="blue"
    )
g.city_e

g.city_e.all <- g.nyc.la +
    geom_segment(x=d.sim.la$month,xend=d.sim.la$month,
                 y=d.sim.la$price_t_hat,yend=d.sim.la$city.price_t_hat,data=d.sim.la,
                 col="blue",linetype=2
                ) +
    geom_text(data=d.sim.la,
              x=I(month + .2),y=(d.sim.la$price_t_hat+d.sim.la$city.price_t_hat)/2,
              label=round(d.sim.la$city.r_t,2),
              hjust="right",col="blue"
    )
g.city_e.all

g.month_e <- g.city_e +
    geom_segment(x=7,xend=7,
                 y=la.dec$city.price_t_hat,yend=la.dec$price_t,
                 col="#F8766D",linetype=2) +
    geom_text(x=7.2,y=(la.dec$city.price_t_hat+la.dec$price_t)/2,
              label=round(la.dec$city.e_t,4),
              hjust="outward",col="#F8766D")
g.month_e

g.tot_e <- g.month_e +
    geom_text(x=7.2,y=5.42,label=round(la.dec$e_t,4),
              hjust="outward",vjust="outward",col="black")
g.tot_e

round(la.dec[,c("price_t","price_t_hat","e_t", "city.r_t","city.e_t")],4)

d.sim$city.e_t_sum <- d.sim$city.r_t + d.sim$city.e_t
round(d.sim[,c("e_t","city.e_t_sum")],4)


