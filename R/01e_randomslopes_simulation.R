#### SIMULATION EXAMPLE: RANDOM SLOPES ####
## Description: This file simulates a dataset where trends start at the same
##              intercept and changes at different rate for 150 metros and
##              analyzes that data
## Author: Michael Bader

source("_functions.R")
library(tidyverse)
library(lme4)


#### SIMULATION EXAMPLE: RANDOM SLOPES
## PLAN THE POPULATION
month <- c(0:12)
N <- 150
N_t <- length(month)
t <- rep(month,N)
i <- as.factor(rep(c(1:N),each=N_t))

beta_0   <- log(117)
gamma_10 <- 0.005
tau_1i   <- 0.001
sigma_ti <- 0.0006

## CONJURE THE POPULATION
beta_1i <- gamma_10 + rnorm(N,0,tau_1i)
lnvalue_ti <- beta_0 + rep(beta_1i, each=N_t)*t + rnorm(N*N_t, 0, sigma_ti)
d.sim <- tibble(i,t,lnvalue_ti)
head(d.sim,N_t*2) ## Observe data for two "metros"

## DESCRIBE OUR POPULATION
g.sim <- ggplot(d.sim,
                aes(x=t,y=lnvalue_ti,group=i)) + geom_line() +
    labs(
        title = "Simulated data with random slopes",
        y = "Outcome", 
        x = "Time"
    )
g.sim

## Take a sample of all trends to see more clearly how individual
## trends vary
g.samp <- ggplot(filter(d.sim, i %in% sample(levels(i), 40)),
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

beta_0_hat <- mean(m.sim.i$beta_0)
gamma_01_hat <- mean(m.sim.i$beta_1)
round(c(beta_0_hat,gamma_01_hat),4)

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
ggsave("../images/sims/random_slopes.pdf", plot = g.sim.pred, 
       width = 9, height = 6, units =)

## Record predicted values and total error
d.sim$lnvalue_ti_hat <- predict(m.sim,re.form=NA) ## Predicted value
d.sim$e_tot <- d.sim$lnvalue_ti - d.sim$lnvalue_ti_hat ## Total error

## Record stochastic compoenents
r_1i_hat <- ranef(m.sim)$i[,1]  ## Error of slope from overall model,
d.sim$r_1i <- r_1i_hat[d.sim$i] ## also called random slopes
d.sim$r_1iXt <- predict(m.sim,random.only=TRUE) ## Random slope times time
d.sim$lnvalue_t_hat <- d.sim$lnvalue_ti_hat + d.sim$r_1iXt
d.sim$e_ti <- d.sim$e_tot - d.sim$r_1iXt ## Month-to-month error off of city trend

## Plot two terms in stochastic component
qplot(d.sim$e_ti,bins=20)
qplot(r_1i_hat,bins=15)

## Demonstrate components of errors
## (The code below produces the plot that I use to demonstrate the components
## of errors)
library(cowplot) ## Allows plots to be combined into a single plot
d.exm = filter(d.sim, i==63)
d.exm$r_1i[1]
d.exm$e_ti[c(5, 10)]

g.tmp <- ggplot(d.exm, aes(x=t, y=lnvalue_ti, group=i)) +
    geom_line(color="#777777") +
    geom_line(
        aes(y=lnvalue_ti_hat),
        # aes(intercept = m.sim.fe[1], slope = m.sim.fe[2]),
        color = "blue", size = 1.2
    ) +
    geom_line(
        aes(y=lnvalue_ti_hat+r_1iXt), color='blue', linetype=2
    ) +
    geom_polygon(data=tibble(x=c(4, 4, 10, 10), y=c(4.78, 4.812, 4.812, 4.78)),
                 aes(x=x, y=y, group=NULL), alpha=0.1) +
    scale_x_continuous(breaks=0:12) +
    theme_minimal()
g.tmp

g.tmp.focus <- g.tmp +
    scale_x_continuous(limits=c(4,10)) +
    scale_y_continuous(limits=c(4.78, 4.812))
g.tmp.cmb <- plot_grid(g.tmp, g.tmp.focus)

ggsave("../images/0104_trend_zoomdetail.pdf",
       plot=g.tmp.cmb,height=4,width=9,units="in")
