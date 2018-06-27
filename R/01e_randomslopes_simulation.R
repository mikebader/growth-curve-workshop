#### SIMULATION EXAMPLE: RANDOM SLOPES ####
## Description: This file simulates a dataset where trends start at the same
##              intercept and changes at different rate for 150 metros and
##              analyzes that data
## Author: Michael Bader

rm(list=ls())
source('R/_functions.R')
library(lme4)
library(ggplot2)

#### SIMULATION EXAMPLE: RANDOM SLOPES
## PLAN THE POPULATION
month <- c(0:12)
N <- 150
T <- length(month)
t <- rep(month,N)
i <- rep(c(1:N),each=T)

gamma_00 <- log(117)
gamma_10 <- 0.005
sigma_ti <- 0.0008
tau_1i <- 0.002

## CONJURE THE POPULATION
set.seed(20180628)
beta_1i <- gamma_10 + rnorm(N,0,tau_1i)
lnvalue_ti <- gamma_00 + rep(beta_1i,each=T)*t + rnorm(N*T,0,sigma_ti)
d.sim <- data.frame(i,t,lnvalue_ti)
head(d.sim,T*2) ## Observe data for two "metros"

## DESCRIBE OUR POPULATION
g.sim <- ggplot(d.sim,aes(x=t,y=lnvalue_ti,group=i)) + geom_line()
g.sim

## Take a sample of all trends to see more clearly how individual
## trends vary
g.samp <- ggplot(d.sim[d.sim$i%in%sample(i,20),],
                 aes(x=t,y=lnvalue_ti,group=i)
            ) +
            geom_line()
g.samp

## ANALYZE THE DATA
## For illustration, let's estimate a line of best fit for each metro
m.sim.i <- by(d.sim,i,function(d) lm(lnvalue_ti~t,data=d))
m.sim.i <- data.frame(t(sapply(m.sim.i,coef)))
names(m.sim.i) <- c("beta_0","beta_1")
qplot(m.sim.i$beta_1,bins=15)

gamma_00_hat <- mean(m.sim.i$beta_0)
gamma_01_hat <- mean(m.sim.i$beta_1)
round(c(gamma_00_hat,gamma_01_hat),4)

## Estimate multilevel model
library(lme4)
m.sim <- lmer(lnvalue_ti ~ t + (0 + t | i),data=d.sim)
summary(m.sim)
m.sim.fe <- round(fixef(m.sim),4)

g.sim.pred  <- g.sim + geom_abline(
    intercept=m.sim.fe[["(Intercept)"]],
    slope=m.sim.fe[["t"]],col="orange",size=1.2
)
g.sim.pred

## Record predicted values and total error
d.sim$lnvalue_ti_hat <- predict(m.sim,re.form=NA) ## Predicted value
d.sim$e_tot <- d.sim$lnvalue_ti - d.sim$lnvalue_ti_hat ## Total error

## Record stochastic compoenents
r_1i_hat <- ranef(m.sim)$i[,1]  ## Error of slope from overall model,
d.sim$r_1i <- r_1i_hat[d.sim$i] ## also called random slopes
d.sim$r_1iXt <- predict(m.sim,random.only=TRUE) ## Random slope times time
d.sim$lnvalue_t_hat <- d.sim$lnvalue_ti_hat + d.sim$r_1iXt
d.sim$e_ti <- d.sim$e_tot - d.sim$r_1iXt ## Error off of city slope per month

## Plot two terms in stochastic component
qplot(d.sim$e_ti,bins=20)
qplot(r_1i_hat,bins=15)

## Demonstrate random slope model with two slopes
d.two <- d.sim[d.sim$i%in%c(1,4),]
d.two$i <- factor(d.two$i)
g.two <- ggplot(data=d.two,aes(x=t,y=lnvalue_ti,color=i)) +
            geom_point() +
            geom_line(aes(y=lnvalue_t_hat)) +
            geom_abline(intercept=m.sim.fe[["(Intercept)"]],
                        slope=m.sim.fe[["t"]],col="blue",size=1.2) +
            scale_x_continuous(breaks=(0:12))
g.two

## Plot zoomed detail to show error components
d.two.t_4 <- d.two[d.two$t==4&i==1,]
g.two.zoom <- g.two +
        geom_segment(x=3.99,xend=3.99,
                     y=d.two.t_4$lnvalue_ti_hat,yend=d.two.t_4$lnvalue_t_hat,
                     color="blue",linetype=2) +
        geom_label(x=3.98,
                  y=(d.two.t_4$lnvalue_ti_hat+d.two.t_4$lnvalue_t_hat)/2,
                  label=paste("4 x r_1i (",round(d.two.t_4$r_1i,4),") ",
                            "=","\n",round(d.two.t_4$r_1iXt,4)),
                  color="blue",size=3,hjust="outward") +
        geom_segment(x=4.01,xend=4.01,
                     y=d.two.t_4$lnvalue_ti,yend=d.two.t_4$lnvalue_t_hat,
                     color="black",linetype=2
                     ) +
        geom_label(x=4.02,
                   y=(d.two.t_4$lnvalue_ti+d.two.t_4$lnvalue_t_hat)/2,
                   label=paste("e_it\n=",round(d.two.t_4$e_ti,4)),
                   color="black",size=3,hjust="outward") +
        scale_x_continuous(limits=c(3,5)) +
        scale_y_continuous(limits=c(4.76,4.80)
        )
ggsave("images/0104_twotrends_zoomdetail.png",
       plot=g.two.zoom,height=2.5,width=4,units="in")
