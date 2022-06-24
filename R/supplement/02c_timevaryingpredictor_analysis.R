#### ANALYSIS EXAMPLE: TIME-INVARIANT PREDICTORS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros with Census region
##              used to refine estimates of intercepts and slopes
## Author: Michael Bader

rm(list=ls())
source('_functions.R')
library(ggplot2)
library(lme4)
library(cowplot)

## GATHER DATA
## Load Bureau of Labor Statistics monthly unemployment data
bls <- read.csv('../data/bls.csv')

## MERGE ZILLOW DATA TO BLS DATA
load("../data/zillow_long.Rdata")
zillow.long <- zillow.long[,-3]
xwlk_url <- 'http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv'
xwlk <- read.csv(xwlk_url,header=TRUE)[,c('CBSAName','MetroRegionID_Zillow', 'CBSACode')]
xwlk <- xwlk[!duplicated(xwlk),]
names(xwlk) <- c("RegionName","RegionID","fips")

zillow.long$month.num <- sapply(zillow.long$month.abbr,
                                function(x) which(month.abb==x))

zillow.bls <- merge(zillow.long,xwlk[,2:3],by="RegionID")
zillow.bls$fips <- as.numeric(as.character(zillow.bls$fips))
zillow.bls$uniqid <- paste0(
                        sprintf("%05.0f",zillow.bls$fips),
                        sprintf("%04.0f",zillow.bls$year),
                        sprintf("%02.0f",zillow.bls$month.num)
                    )

## NOTE: These data are missing values for New England metropolitan areas
zillow.bls <- merge(zillow.bls,bls[,c("uniqid","unemp_rate")],by="uniqid",all.x=TRUE)

zillow.bls$lnvalue_ti <- log(zillow.bls$value_t)
zillow.bls <- zillow.bls[zillow.bls$month>=(max(zillow.bls$month)-24),]
zillow.bls$month <- zillow.bls$month - min(zillow.bls$month)
zillow.bls <- zillow.bls[,-8:-9]

## DESCRIBE THE DATA
head(zillow.bls)
cor(zillow.bls[,c("lnvalue_ti","unemp_rate")],use="complete.obs")

## MODEL WITH RANDOM INTERCEPTS
m.ana <- lmer(lnvalue_ti~month + unemp_rate + (1|RegionID),data=zillow.bls)
summary(m.ana)

## When I estimated a model with random slopes, the variance component on
## the random slope effect (i.e., rho_1) was *very* small, and led to 
## convergence problems. I decided to use only a random intercepts model
## instead. 
##
## If you want to see what the model with random slopes looks like, then 
## uncomment the following two lines
# m.slp <- lmer(lnvalue_ti~month + unemp_rate + (1 + month|RegionID),data=zillow.bls)
# summary(m.slp)

## INTERPRET THE RESULTS
## We will interpret what happens if the unemployment rate drops 4% halfway
## through our study period. This is an unreasonable change, but it 
## illustrates the change well. 

## Make a mock dataset with values at which we want data to be estimated
mean_unemp <- mean(zillow.bls[zillow.bls$month==0,"unemp_rate"],na.rm=TRUE)
mock <- data.frame(
    RegionID = factor(rep(1:2,each=25),
                      labels=c("constant unemp.","4 pct. decline\n(Mar 2019)")),
    month = rep(0:24,2),
    unemp_rate = c(rep(mean_unemp,25), rep(0, 12), rep(-4, 13))
)
mock$lnvalue_ti_hat <- predict(m.ana,newdata=mock,re.form=NA)
mock$value_ti_hat <- exp(mock$lnvalue_ti_hat)

## Plot change due to 4% decline in unemployment in March 2019
month_labels <- mapply(paste, 
                       rep(month.abb, 3)[seq(3,28,4)], 
                       rep(2018:2020, each=3)[1:7])
g.full <- ggplot(data=mock,aes(x=month,y=value_ti_hat,
                     group=RegionID,color=RegionID)) +
    geom_line() +
    scale_x_continuous(breaks=seq(0,24,4), labels=month_labels)
g.detail <- ggplot(data=mock[mock$month%in%10:15,], 
                   aes(x=month, y=value_ti_hat, group=RegionID, 
                       color=RegionID)) +
    geom_line() +
    scale_x_continuous(breaks=10:15, labels=paste(month.abb[1:6], "2019"))
plot_grid(g.full, g.detail, nrow=2)
