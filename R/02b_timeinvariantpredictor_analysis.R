#### ANALYSIS EXAMPLE: TIME-INVARIANT PREDICTORS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros with logged 
##              population used to refine estimates of intercepts and slopes
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(ggplot2)
library(lme4)
current_year <- 2020

## GATHER DATA
## Download and format Zillow's crosswalk dataset
xwlk_url <- 'http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv'
xwlk <- read.csv(xwlk_url,header=TRUE)[,c('CBSAName','MetroRegionID_Zillow', 'CBSACode')]
xwlk <- xwlk[!duplicated(xwlk),]
names(xwlk) <- c("ZillowName","RegionID","Geo_CBSA")

## Download Census 2010 data (that includes geographic identifiers
## and total population)
census2010 <- read.delim("../data/R11740339_SL320.txt",encoding="UTF-8")
census2010 <- census2010[,c("Geo_NAME","Geo_CBSA","Geo_REGION","SE_T001_001")]

## Select part of metro with largest population
census2010 <- census2010[order(census2010$SE_T001_001),]
census2010$i <- 1:nrow(census2010)
vals <- tapply(census2010$i,census2010$Geo_NAME,max)
metros <- census2010[census2010$i %in% vals,]

metros <- merge(xwlk,metros,by="Geo_CBSA")

## Load Zillow data and merge to Census
load("../data/zillow_long.Rdata")
zillow_acs <- merge(zillow.long,metros,by="RegionID",all.x=TRUE)
head(zillow_acs[zillow_acs$month==0,
                   c("RegionName","Geo_NAME")],20) #Check merge

## PREPARE DATA
zillow_acs <- zillow_acs[order(zillow_acs$RegionName,zillow_acs$month),]
zillow_acs <- zillow_acs[zillow_acs$month>=(max(zillow_acs$month)-24),]
zillow_acs$month <- zillow_acs$month - min(zillow_acs$month)
zillow_acs$lnvalue_ti <- log(zillow_acs$value_t)
zillow_acs$Geo_REGION <- factor(zillow_acs$Geo_REGION,
                                labels=c("Northeast","Midwest","South","West"))
zillow_acs$RegionName <- factor(zillow_acs$RegionName)

## Center the log of population around its mean
mean_lnpop <- mean(log(zillow_acs$SE_T001_001), na.rm=TRUE)
zillow_acs$clnpop_i <- log(zillow_acs$SE_T001_001) - mean_lnpop

## DESCRIBE DATA
## Let's take a look at the first couple of metros with the four key 
## variables
d.ana <- zillow_acs[, c("RegionID", "month", "lnvalue_ti", "clnpop_i")]
d.ana$RegionID <- factor(d.ana$RegionID)
head(d.ana, 50)

## Plot individual trajectories
g.desc <- ggplot(d.ana,aes(x=month,y=lnvalue_ti,
                        group=RegionID, color=cut(clnpop_i, 5))) +
    geom_line() +
    scale_color_brewer(palette="Blues")
g.desc

samp <- sample(levels(d.ana$RegionID),75)
g.samp <- ggplot(d.ana[d.ana$RegionID%in%samp,],
                 aes(x=month,y=lnvalue_ti,
                     group=RegionID,color=cut(clnpop_i, 5))) +
    geom_line() +
    scale_color_brewer(palette="Blues")
g.samp

## Estimate regression for each group i, accounting for lnpop
betas <- sapply(unique(d.ana$RegionID), function(i){
    coef(lm(lnvalue_ti ~ month, data=d.ana[d.ana$RegionID==i,]))
})

## Plot distribution of intercepts
qplot(betas[1,], bins=15) +
    geom_vline(xintercept = mean(betas[1,]), color="orange", size=1.5) 

## Plot distribution of slopes
qplot(betas[2,], bins=15) + 
    geom_vline(xintercept = mean(betas[2,]), color="orange", size=1.5) +
    scale_x_continuous(breaks=seq(-.01,.01,.002), limits=c(-.006,.01))

## ANALYZE THE DATA
m.ctr <- lmer(lnvalue_ti ~ clnpop_i * month + (1 + month | RegionID), 
              data=d.ana)
summary(m.ctr)
huxtable::huxreg(m.ctr)

## Plot estimated values of parameters over original data
g.centered <- g.desc +
    geom_abline(
        intercept = fixef(m.ctr)["(Intercept)"],
        slope = fixef(m.ctr)["month"],
        size = 2, color = "darkorange"        
    )
g.centered

## Plot predicted influence of logged population on change in housing values
gammahat_00 <- fixef(m.ctr)["(Intercept)"]
gammahat_01 <- fixef(m.ctr)["clnpop_i"]
gammahat_10 <- fixef(m.ctr)["month"]
gammahat_11 <- fixef(m.ctr)["clnpop_i:month"]

m <- 0:24
sd_lnpop <- sd(d.ana$clnpop_i, na.rm=TRUE)
g.pres <- ggplot() +
    scale_x_continuous(limits = c(0,24)) +
    scale_y_continuous(limits = c(5, 6)) +
    geom_abline(
        intercept = gammahat_00, 
        slope = gammahat_10) + ## Mean pop
    geom_abline(
        intercept = gammahat_00 + gammahat_01*sd_lnpop,
        slope = gammahat_10 + gammahat_11*sd_lnpop,
        linetype = 2
    ) +
    geom_abline(
        intercept = gammahat_00 + gammahat_01*-1*sd_lnpop,
        slope = gammahat_10 + gammahat_11*-1*sd_lnpop,
        linetype = 3
    ) +
    labs(
        title = "Predicted logged values of price/sq.ft. of metro areas",
        subtitle = paste("March", current_year-2, "to March ", current_year),
        caption = "Note: Based on Zillow monthly median price data",
        y = "Logged price/sq.ft.",
        x = "Month"
    )
g.pres

## But its difficult to think in terms of logged price/sqft, let's make 
## the plot of price/sqft
g.presdollar <- ggplot() +
    scale_x_continuous(limits = c(0,24)) +
    scale_y_continuous(limits = c(exp(5), exp(6))) +
    geom_abline(
        intercept = exp(gammahat_00), 
        slope = exp(gammahat_10)) + ## Mean pop
    geom_abline(
        intercept = exp(gammahat_00 + gammahat_01*sd_lnpop),
        slope = exp(gammahat_10 + gammahat_11*sd_lnpop),
        linetype = 2
    ) +
    geom_abline(
        intercept = exp(gammahat_00 + gammahat_01*-1*sd_lnpop),
        slope = exp(gammahat_10 + gammahat_11*-1*sd_lnpop),
        linetype = 3
    ) +
    labs(
        title = "Predicted values of price/sq.ft. of metro areas",
        subtitle = paste("March", current_year-2, "to March ", current_year),
        caption = "Note: Based on Zillow monthly median price data",
        y = "$/sq.ft.",
        x = "Month"
    ) 
g.presdollar 
