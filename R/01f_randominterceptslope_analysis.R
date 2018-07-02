#### ANALYSIS EXAMPLE: RANDOM INTERCEPTS & SLOPES ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros
## Author: Michael Bader

## Show that slopes do vary across metro areas in Zillow data
source('R/01d_randomintercepts_analysis.R')
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

## GATHER THE DATA
## The data are the same as those used in the random intercept model

## DESCRIBE THE DATA
g.sim <- ggplot(zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID)) +
            geom_line()
g.sim

sampsize <- 30
samp <- sample(zillow.long$RegionName,sampsize)
g.samp <- ggplot(zillow.long[zillow.long$RegionName%in%samp,],
                 aes(x=month,y=lnvalue_ti,group=RegionName)) +
            geom_line()
g.samp

## ANALYZE THE DATA
m.ana <- lmer(lnvalue_ti ~ month + (1 + month | RegionID),data=zillow.long)
summary(m.ana)
m.ana.fe <- fixef(m.ana)
m.ana.re <- ranef(m.ana)$RegionID

test <- m.ana.re[abs(m.ana.re[,1])<.02,]
zillow.long[zillow.long$RegionID%in%rownames(test),"RegionName"]

## INTERPRET THE DATA
g.pred <- geom_abline(intercept=m.ana.fe[1],slope=m.ana.fe[2],
                      col="orange",size=1.2)
g.ana <- ggplot(data=zillow.long,aes(x=month,y=lnvalue_ti,group=RegionID)) +
            geom_line() +
            g.pred +
            scale_x_continuous(breaks=seq(0,12,1),labels=rep(month.abb,2)[5:17])
g.ana

## Record predicted values and total error
zillow.long$lnvalue_ti_hat <- predict(m.ana,re.form=NA)
zillow.long$e_tot <- zillow.long$lnvalue_ti - zillow.long$lnvalue_ti_hat ## Total error

## Record stochastic components of the model
zillow.long$r_0i <- predict(m.ana,re.form=~(1|RegionID),random.only=T)
zillow.long$r_1i <- rep(m.ana.re[,2],each=13)
zillow.long$r_1iXt <- predict(m.ana,re.form=~(0+month|RegionID),random.only=T)
zillow.long$e_ti <- (zillow.long$e_tot - zillow.long$r_0i - zillow.long$r_1iXt)

## EXAMPLE METRO-SPECIFIC TRENDS
ex.metros <- c(394640,394974,395012)
zillow.ex.metros <- zillow.long[zillow.long$RegionID%in%ex.metros,]
g.ex <- ggplot(zillow.ex.metros,
       aes(x=month,y=lnvalue_ti,color=RegionName)) +
    geom_line(size=.5,linetype=2) +
    geom_smooth(method="lm",se=FALSE,size=.75) +
    g.pred
g.ex
ggsave("images/0105_randominterceptsslopes_example.png",
       plot=g.ex,height=2.5,width=4,units="in")

zillow.ex.metros[zillow.ex.metros$month==0,c("RegionName","r_0i","r_1i")]
