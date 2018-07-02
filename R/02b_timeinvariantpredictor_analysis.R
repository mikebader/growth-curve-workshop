#### ANALYSIS EXAMPLE: TIME-INVARIANT PREDICTORS ####
## Description: This file analyzes Zillow dataset assuming random intercept
##              and random slopes for 150 largest metros with Census region
##              used to refine estimates of intercepts and slopes
## Author: Michael Bader
library(ggplot2)
library(lme4)

rm(list=ls())

## GATHER DATA
## Download and format Zillow's crosswalk dataset
xwlk_url <- 'http://files.zillowstatic.com/research/public/CountyCrossWalk_Zillow.csv'
xwlk <- read.csv(xwlk_url,header=TRUE)[,c('CBSAName','MetroRegionID_Zillow', 'CBSACode')]
xwlk <- xwlk[!duplicated(xwlk),]
names(xwlk) <- c("ZillowName","RegionID","Geo_CBSA")

## Download Census 2010 data (that includes geographic identifiers
## and total population)
census2010 <- read.delim("data/R11740339_SL320.txt",encoding="UTF-8")
census2010 <- census2010[,c("Geo_NAME","Geo_CBSA","Geo_REGION","SE_T001_001")]

## Select part of metro with largest population
census2010 <- census2010[order(census2010$SE_T001_001),]
census2010$i <- 1:nrow(census2010)
vals <- tapply(census2010$i,census2010$Geo_NAME,max)
metros <- census2010[census2010$i %in% vals,]

metros <- merge(xwlk,metros,by="Geo_CBSA")

## Load Zillow data and merge to Census
load("data/zillow_long.Rdata")
zillow_acs <- merge(zillow.long,metros,by="RegionID",all.x=TRUE)
head(zillow_acs[zillow_acs$month==0,
                   c("RegionName","Geo_NAME")],20) #Check merge

## PREPARE DATA
zillow_acs <- zillow_acs[order(zillow_acs$RegionName,zillow_acs$month),]
zillow_acs <- zillow_acs[zillow_acs$month>=(max(zillow_acs$month)-12),]
zillow_acs$month <- zillow_acs$month - min(zillow_acs$month)
zillow_acs$lnvalue_ti <- log(zillow_acs$value_t)
zillow_acs$Geo_REGION <- factor(zillow_acs$Geo_REGION,
                                labels=c("Northeast","Midwest","South","West"))
zillow_acs$RegionName <- factor(zillow_acs$RegionName)

## DESCRIBE DATA
g.desc <- ggplot(zillow_acs[],aes(x=month,y=lnvalue_ti,
                        group=RegionName,color=Geo_REGION)) +
            geom_line()
g.desc

samp <- sample(levels(zillow_acs$RegionName),75)
g.samp <- ggplot(zillow_acs[zillow_acs$RegionName%in%samp,],
                 aes(x=month,y=lnvalue_ti,
                     group=RegionName,color=Geo_REGION)) +
            geom_line()
g.samp

## ANALYZE THE DATA
## Assume that increases in home values are constant across regions
m.ana <- lmer(lnvalue_ti ~ month + Geo_REGION + (1 + month | RegionID),data=zillow_acs)
summary(m.ana)
m.ana.fe <- fixef(m.ana)
m.ana.re <- ranef(m.ana)$RegionID

## Plot results by region
mock <- data.frame(
    Geo_REGION = rep(levels(zillow_acs$Geo_REGION),each=13),
    month = rep(0:12,4)
)
mock$lnvalue_ti_hat_1 <- predict(m.ana,newdata=mock,re.form=NA)
g.intonly <- ggplot(data=mock,aes(x=month,y=lnvalue_ti_hat_1,
                     group=Geo_REGION,color=Geo_REGION)) +
                geom_line()
g.intonly

## Assume that increases in home values differ by region
m.ana2 <- lmer(lnvalue_ti ~ month * Geo_REGION + (1 + month | RegionID),
               data=zillow_acs)
summary(m.ana2)

## Plot results by region
mock$lnvalue_ti_hat_2 <- predict(m.ana2,newdata=mock,re.form=NA)
g.intslp <- ggplot(data=mock,aes(x=month,y=lnvalue_ti_hat_2,
                     group=Geo_REGION,color=Geo_REGION)) +
                geom_line()
g.intslp


